use std::{fmt::Display, fs::File, io::BufReader};

use crate::{
    cartridge::{Cartridge, CartridgeError},
    io_registers::IORegisters,
    ppu_registers::PPURegisters,
    Bus, Wrapper,
};

#[derive(Debug)]
pub enum MemoryError {
    InvalidWrite(u16),
    InvalidRead(u16),
    CartridgeError(CartridgeError),
}

impl Display for MemoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Self::InvalidRead(address) => {
                format!("You tried to access `0x{address:04x}` which is not meant to be read from")
            }
            Self::InvalidWrite(address) => format!(
                "You tried to write to `0x{address:04x}` which is not a valid write address",
            ),
            Self::CartridgeError(v) => {
                format!("A CatridgeError was thrown by Memory.\nError:`{v}`")
            }
        };
        output.fmt(f)
    }
}
pub struct Memory {
    wram: [u8; 0x2000],
    hram: [u8; 0x7f],
    boot_rom: BufReader<File>,
    booting: bool,
    cartridge: Cartridge,
    io_registers: Wrapper<IORegisters>,
    ppu_registers: Wrapper<PPURegisters>,
}

impl Memory {
    pub fn new(
        cartridge: Cartridge,
        boot_rom: BufReader<File>,
        io_registers: Wrapper<IORegisters>,
        ppu_registers: Wrapper<PPURegisters>,
    ) -> Self {
        Memory {
            wram: [0; 0x2000],
            hram: [0; 0x7f],
            cartridge,
            boot_rom,
            booting: true,
            io_registers,
            ppu_registers,
        }
    }
}

impl Bus for Memory {
    fn read(&mut self, address: u16) -> Result<u8, MemoryError> {
        match address {
            0x0000..=0x00FF => {
                if self.booting {
                    let vector = crate::cartridge::read(&mut self.boot_rom, address as usize, 1)
                        .map_err(|v| MemoryError::CartridgeError(v))?;
                    return Ok(vector[0]);
                } else {
                    return self
                        .cartridge
                        .cart_read(address)
                        .map_err(|v| MemoryError::CartridgeError(v));
                }
            }

            //? ROM DATA
            0x0100..=0x7FFF => {
                return self
                    .cartridge
                    .cart_read(address)
                    .map_err(|v| MemoryError::CartridgeError(v));
            }

            //? VIDEO RAM
            0x8000..=0x9FFF => {
                return self.ppu_registers.borrow_mut().read(address);
            }

            //? CARTRIDGE RAM
            0xA000..=0xBFFF => {
                return self
                    .cartridge
                    .cart_read(address)
                    .map_err(|v| MemoryError::CartridgeError(v));
            }

            //? WORK RAM
            0xC000..=0xDFFF => {
                let address = address - 0xC000;
                return Ok(self.wram[address as usize]);
            }

            //? ECHO RAM
            0xE000..=0xFDFF => {
                let address = address - 0xE000;
                if address < 0x2000 {
                    return Ok(self.wram[address as usize]);
                } else {
                    return Err(MemoryError::InvalidRead(address + 0xC000));
                }
            }

            //? OAM DATA
            0xFE00..=0xFE9F => {
                return self.ppu_registers.borrow_mut().read(address);
            }

            //? NOT USABLE AT ALL
            0xFEA0..=0xFEFF => return Err(MemoryError::InvalidRead(address)),

            //? IO REGISTERS
            0xFF00..=0xFF7F => match address {
                0xFF40..=0xFF4B => return self.ppu_registers.borrow_mut().read(address),
                _ => return self.io_registers.borrow_mut().read(address),
            },

            //? HI RAM
            0xFF80..=0xFFFE => {
                let address = address - 0xFF80;
                if address < 0x7f {
                    return Ok(self.hram[address as usize]);
                } else {
                    return Err(MemoryError::InvalidRead(address + 0xFF80));
                }
            }

            //? MASTER INTERRUPT REGISTER
            0xFFFF => {
                return self.io_registers.borrow_mut().read(address);
            }
        }
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), MemoryError> {
        match address {
            //? ROM DATA
            0x0000..=0x7FFF => {
                self.cartridge.cart_write(address, value);
            }

            //? VIDEO RAM
            0x8000..=0x9FFF => {
                return self.ppu_registers.borrow_mut().write(address, value);
            }

            //? CARTRIDGE RAM
            0xA000..=0xBFFF => {
                self.cartridge.cart_write(address, value);
            }

            //? WORK RAM
            0xC000..=0xDFFF => {
                let address = address - 0xC000;
                if address < 0x2000 {
                    self.wram[address as usize] = value;
                } else {
                    return Err(MemoryError::InvalidWrite(address + 0xC000));
                }
            }

            //? ECHO RAM
            0xE000..=0xFDFF => {
                return Err(MemoryError::InvalidWrite(address));
            }

            //? OAM DATA
            0xFE00..=0xFE9F => {
                return self.ppu_registers.borrow_mut().write(address, value);
            }

            //? NOT USABLE AT ALL
            0xFEA0..=0xFEFF => {
                return Err(MemoryError::InvalidWrite(address));
            }

            //? IO REGISTERS
            0xFF00..=0xFF7F => match address {
                0xFF40..=0xFF4B => return self.ppu_registers.borrow_mut().write(address, value),
                _ => return self.io_registers.borrow_mut().write(address, value),
            },
            //? HI RAM
            0xFF80..=0xFFFE => {
                let address = address - 0xFF80;
                if address < 0x7f {
                    self.hram[address as usize] = value;
                } else {
                    return Err(MemoryError::InvalidWrite(address + 0xFF80));
                }
            }

            //? MASTER INTERRUPT REGISTER
            0xFFFF => {
                return self.io_registers.borrow_mut().write(address, value);
            }
        }
        Ok(())
    }
}
