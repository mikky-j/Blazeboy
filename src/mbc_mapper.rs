use crate::{
    cartridge::{CartridgeError, CatridgeType},
    get_bit, get_bits,
};

#[derive(Debug)]
pub enum MbcRegister {
    Mbc1 { bank1: u8, bank2: u8 },
    Mbc2 { romb: u8 },
}

impl MbcRegister {
    fn get_register(mbc: CatridgeType) -> Self {
        use CatridgeType::*;
        match mbc {
            Mbc1 => Self::Mbc1 { bank1: 0, bank2: 0 },
            Mbc2 => Self::Mbc2 { romb: 0 },
            _ => panic!("Cannot get the register for an unsupported Mbc: `{mbc}`"),
        }
    }

    fn get_value(&self) -> Vec<u8> {
        match *self {
            Self::Mbc1 { bank1, bank2 } => vec![bank1, bank2],
            Self::Mbc2 { romb } => vec![romb],
        }
    }
}

// Todo: remove any unused fields
#[derive(Debug)]
pub struct MbcMapper {
    _rom_size: usize,
    mbc: CatridgeType,
    ramg: bool,
    register: MbcRegister,
    mode: bool,
    _ram: bool,
}

impl<'a> MbcMapper {
    pub fn new(mbc: CatridgeType, rom_size: usize, ram: bool) -> Self {
        let register = MbcRegister::get_register(mbc);
        MbcMapper {
            mbc,
            _rom_size: rom_size,
            _ram: ram,
            register,
            ramg: false,
            mode: false,
        }
    }

    pub fn get_read_address(&self, address: u16) -> Result<usize, CartridgeError> {
        use crate::cartridge::CatridgeType::*;

        let effective_address = match self.mbc {
            Mbc1 => {
                let [bank1, bank2]: [u8; 2] = self.register.get_value().try_into().unwrap();

                match address {
                    0x0000..=0x3FFF => {
                        let bank_number = if self.mode { bank2 << 5 } else { 0 };
                        ((bank_number as u32) << 14) | get_bits!((address as u32), 14)
                    }
                    0x4000..=0x7FFF => {
                        let bank_number = (bank2 << 5) | bank1;
                        // println!("The bank number: {bank_number:04x}");
                        ((bank_number as u32) << 14) | get_bits!((address as u32), 14)
                    }
                    0xA000..=0xBFFF => {
                        if self.ramg {
                            let bank_number = if self.mode { bank2 } else { 0 };
                            ((bank_number as u32) << 13) | get_bits!((address as u32), 13)
                        } else {
                            // Todo: Verify this place ooo
                            println!("The Ram is disabled ooo");
                            return Err(CartridgeError::MbcRamDisabled(self.mbc));
                        }
                    }
                    _ => panic!("Unsupported {} Address: {address}", self.mbc),
                }
            }
            Mbc2 => {
                let [romb]: [u8; 1] = self.register.get_value().try_into().unwrap();
                match address {
                    0x0000..=0x3FFF => get_bits!(address, 14) as u32,
                    0x4000..=0x7FFF => (romb as u32) << 14 | (get_bits!(address, 14) as u32),
                    _ => panic!("Unsupported {} Address: {address}", self.mbc),
                }
            }
            _ => panic!("Unsupported MBC: {}", self.mbc),
        };
        Ok(effective_address as usize)
    }

    pub fn handle_write(&mut self, address: u16, value: u8) {
        use crate::cartridge::CatridgeType::*;
        match self.mbc {
            Mbc1 => {
                let [mut bank1, mut bank2]: [u8; 2] = self.register.get_value().try_into().unwrap();
                match address {
                    0x0000..=0x1FFF => {
                        self.ramg = get_bits!(value, 4) == 0b1010;
                    }
                    0x2000..=0x3FFF => {
                        bank1 = if value == 0 { 1 } else { get_bits!(value, 5) };
                    }
                    0x4000..=0x5FFF => {
                        bank2 = get_bits!(value, 2);
                    }
                    0x6000..=0x7FFF => self.mode = get_bit!(value, 0) == 1,
                    _ => panic!("Unsupported {} Address: {address}", self.mbc),
                }
                self.register = MbcRegister::Mbc1 { bank1, bank2 };
            }
            Mbc2 => {
                let [mut romb]: [u8; 1] = self.register.get_value().try_into().unwrap();
                match address {
                    0x0000..=0x3FFF => {
                        if get_bit!(address, 8) == 0 {
                            self.ramg = get_bits!(value, 4) == 0b1010;
                        } else {
                            romb = if value == 0 { 1 } else { get_bits!(value, 4) };
                        }
                    }
                    _ => panic!("Unsupported {} Write Address: {address}", self.mbc),
                }
                self.register = MbcRegister::Mbc2 { romb }
            }
            _ => panic!("Unsupported MBC found: {}", self.mbc),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::cartridge::Cartridge;

    #[test]
    fn test_mbc1_rom_loading() {
        let cartridge = Cartridge::new("src/roms/Legend of Zelda.gb").unwrap();
        let mut mbc_mapper = cartridge.mapper.unwrap();
        // Enabling the ram
        mbc_mapper.handle_write(0x0023, 0b1010);
        // Writing to the BANK1 Register
        mbc_mapper.handle_write(0x2222, 0b00100);
        // Writing to the BANK2 Register
        mbc_mapper.handle_write(0x4444, 0b10);
        // Writing to the MODE Register
        mbc_mapper.handle_write(0x6666, 0);

        let byte = mbc_mapper.get_read_address(0x72A7).unwrap();
        println!("The address to be read was {byte:02x}");
    }

    #[test]
    fn test_mbc1_ram_loading() {
        let cartridge = Cartridge::new("src/roms/Legend of Zelda.gb").unwrap();
        let mut mbc_mapper = cartridge.mapper.unwrap();
        // Enabling the ram
        mbc_mapper.handle_write(0x0023, 0b1010);
        // Writing to the BANK1 Register
        mbc_mapper.handle_write(0x2222, 0b00100);
        // Writing to the BANK2 Register
        mbc_mapper.handle_write(0x4444, 0b10);
        // Writing to the MODE Register
        mbc_mapper.handle_write(0x6666, 1);

        let byte = mbc_mapper.get_read_address(0xB123).unwrap();
        println!("The address to be read was {byte:02x}");
    }
}
