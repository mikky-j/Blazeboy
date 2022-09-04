use std::{fs::File, io::BufReader};

use crate::{
    cartridge::{Cartridge, CartridgeError},
    io_registers::IORegisters,
    Bus, Wrapper,
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
    hram: [u8; 0x7f],
    vram: [u8; 0x2000],
    oam: [u8; 0xa0],
    boot_rom: BufReader<File>,
    booting: bool,
    cartridge: Cartridge,
    io_registers: Wrapper<IORegisters>,
}

impl Memory {
    pub fn new(
        cartridge: Cartridge,
        boot_rom: BufReader<File>,
        io_registers: Wrapper<IORegisters>,
    ) -> Self {
        Memory {
            wram: [0; 0x2000],
            vram: [0; 0x2000],
            hram: [0; 0x7f],
            oam:[0; 0xa0],
            cartridge,
            boot_rom,
            booting: true,
            io_registers,
        }
    }
}

impl Bus for Memory {
    fn read(&mut self, address: u16) -> Result<u8, MemoryError> {
        if address < 0x100 && self.booting {
            let vector = crate::cartridge::read(&mut self.boot_rom, address as usize, 1)
                .map_err(|v| MemoryError::CartridgeError(v))?;
            return Ok(vector[0]);
        } else if address < 0x8000 {
            // self.booting = false;
            //? ROM DATA
            return self
                .cartridge
                .cart_read(address)
                .map_err(|v| MemoryError::CartridgeError(v));
        } else if address < 0xA000 {
            //? VIDEO RAM
            let offset =  address - 0x8000;
            // println!("Unsupported Reading from {:04x}", address)
            return Ok(self.vram[offset as usize])
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xC000 {
            //? CARTRIDGE RAM
            return self
                .cartridge
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
        } else if address < 0xFE00 {
            //? ECHO RAM
            let address = address - 0xE000;
            if address < 0x2000 {
                return Ok(self.wram[address as usize]);
            } else {
                return Err(MemoryError::InvalidRead(address + 0xC000));
            }
        } else if address < 0xFEA0 {
            //? OAM DATA
            let offset =  address - 0xFE00;
            return Ok(self.oam[offset as usize])
            // println!("Unsupported Reading from {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFF00 {
            //? NOT USABLE AT ALL
            println!("Unsupported Reading from {:04x}", address)
            // Err(MemoryError::InvalidRead(address))
        } else if address < 0xFF80 {
            //? IO REGISTERS
            return self.io_registers.borrow_mut().read(address);
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFFFF {
            //? HI RAM
            let address = address - 0xFF80;
            if address < 0x7f {
                return Ok(self.hram[address as usize]);
            } else {
                return Err(MemoryError::InvalidRead(address + 0xFF80));
            }
        } else {
            //? MASTER INTERRUPT REGISTER
            return self.io_registers.borrow_mut().read(address);
            // unimplemented!("Not yet implemented")
        }
        Ok(0)
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), MemoryError> {
        if address < 0x8000 {
            //? ROM DATA
            self.cartridge.cart_write(address, value);
        } else if address < 0xA000 {
            //? VIDEO RAM
            let offset =  address - 0x8000;
            self.vram[offset as usize] = value;
            // println!("Unsupported Writing to {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xC000 {
            //? CARTRIDGE RAM
            self.cartridge.cart_write(address, value);
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
        } else if address < 0xFEA0 {
            //? OAM DATA
            let offset =  address - 0xFE00;
            self.oam[offset as usize] = value;
            // println!("Unsupported Writing to {:04x}", address)
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFF00 {
            //? NOT USABLE AT ALL
            return Err(MemoryError::InvalidWrite(address));
        } else if address < 0xFF80 {
            //? IO REGISTERS
            return self.io_registers.borrow_mut().write(address, value);
            // unimplemented!("Tried to access `{address:02x}`")
        } else if address < 0xFFFF {
            //? HI RAM
            let address = address - 0xFF80;
            if address < 0x7f {
                self.hram[address as usize] = value;
            } else {
                return Err(MemoryError::InvalidWrite(address + 0xFF80));
            }
        } else {
            //? MASTER INTERRUPT REGISTER
            // println!("Unsupported Writing to {:04x}", address)
            return self.io_registers.borrow_mut().write(address, value);
        }
        Ok(())
    }
}
