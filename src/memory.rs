use crate::{
    cartridge::{Cartridge, CartridgeError},
    Bus,
    io_registers::IORegisters, Wrapper
};

#[derive(Debug)]
pub enum MemoryError {
    InvalidWrite(u16),
    InvalidRead(u16),
    CartridgeError(CartridgeError),
}

impl MemoryError {
    pub fn output(&self) -> String {
        match self {
            Self::InvalidRead(address) => {
                format!("You tried to access `0x{address:04x}` which is not meant to be read from")
            }
            Self::InvalidWrite(address) => format!(
                "You tried to write to `0x{address:04x}` which is not a valid write address",
            ),
            Self::CartridgeError(v) => format!("{v}"),
        }
    }
}
pub struct Memory {
    wram: [u8; 0x2000],
    hram: [u8; 0x7e],
    cartridge: Cartridge,
    io_registers: Wrapper<IORegisters>,
}

impl Memory {
    pub fn new(cartridge: Cartridge, io_registers: Wrapper<IORegisters>) -> Self {
        Memory {
            wram: [0; 0x2000],
            hram: [0; 0x7e],
            cartridge,
            io_registers
        }
    }
}

impl Bus for Memory {
    fn read(&mut self, address: u16) -> Result<u8, MemoryError> {
        if address < 0x8000 {
            //? ROM DATA
            return self.cartridge
                .cart_read(address)
                .map_err(|v| MemoryError::CartridgeError(v));
        } else if address < 0xA000 {
            //? VIDEO RAM
            println!("Unsupported Reading from {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xC000 {
            //? CARTRIDGE RAM
            return self.cartridge
                .cart_read(address)
                .map_err(|v| MemoryError::CartridgeError(v));
        } else if address < 0xE000 {
            //? WORK RAM
            let address = address - 0xC000;
            if address < 0x2000 {
                return Ok(self.wram[address as usize]);
            } else {
                return Err(MemoryError::InvalidRead(address + 0xC000));
            }
        } else if address < 0xFE00{
            //? ECHO RAM
            let address = address - 0xE000;
            if address < 0x2000 {
                return Ok(self.wram[address as usize]);
            } else {
                return Err(MemoryError::InvalidRead(address + 0xC000));
            }
        }else if address < 0xFEA0 {
            //? OAM DATA
            println!("Unsupported Reading from {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFF00 {
            //? NOT USABLE AT ALL
            println!("Unsupported Reading from {:04x}", address)
            // Err(MemoryError::InvalidRead(address))
        } else if address < 0xFF80 {
            //? IO REGISTERS
            return self.io_registers.borrow_mut().read(address);
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFFFE {
            //? HI RAM
            let address = address - 0xFF80;
            if address < 0x7e {
                return Ok(self.hram[address as usize]);
            } else {
                return Err(MemoryError::InvalidRead(address + 0xFF80));
            }
        } else {
            //? MASTER INTERRUPT REGISTER
            // unimplemented!("Not yet implemented")
        }
        Ok(0)
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), MemoryError> {
        if address < 0x8000 {
            //? ROM DATA
            self.cartridge.cart_write(address, value);
        }else if address < 0xA000 {
            //? VIDEO RAM
            println!("Unsupported Writing to {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xC000 {
            //? CARTRIDGE RAM
            self.cartridge
                .cart_write(address, value);
        } else if address < 0xE000 {
            //? WORK RAM
            let address = address - 0xC000;
            if address < 0x2000 {
                self.wram[address as usize] = value;
            } else {
                return Err(MemoryError::InvalidWrite(address + 0xC000));
            }
        } else if address < 0xFE00 {
            //? ECHO RAM
            return Err(MemoryError::InvalidWrite(address));
        }else if address < 0xFEA0 {
            //? OAM DATA
            println!("Unsupported Writing to {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFF00 {
            //? NOT USABLE AT ALL
            return Err(MemoryError::InvalidWrite(address));
        } else if address < 0xFF80 {
            //? IO REGISTERS
            return self.io_registers.borrow_mut().write(address, value);
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFFFE {
            //? HI RAM
            let address = address - 0xFF80;
            if address < 0x7e {
                self.hram[address as usize] = value;
            } else {
                return Err(MemoryError::InvalidWrite(address + 0xFF80));
            }
        } else {
            //? MASTER INTERRUPT REGISTER
            println!("Unsupported Writing to {:04x}", address)
        }
        Ok(())
    }
}
