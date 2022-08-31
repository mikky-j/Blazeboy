use crate::Bus;

#[derive(Default)]
pub struct IORegisters {
    // 0xFF00
    joypad_input: u8,
    // 0xFF01 - 0xFF02
    serial_transfer: [u8; 2],
    // 0xFF04 - 0xFF07
    timer_divider: [u8; 4],
    // 0xFF10 - 0xFF26
    sound: [u8; 23],
    // 0xFF30 - 0xFF3F
    wave_pattern: [u8; 16],
    // 0xFF40 - 0xFF4B
    lcd_registers: [u8; 12],
    // 0xFF4F
    vram_bank_select: u8,
    // 0xFF50
    boot_rom: u8,
    // 0xFF51 - 0xFF55
    vram_dma: [u8; 5],
    // 0xFF68 - 0xFF69
    bg_palletes: [u8; 2],
    // 0xFF70
    wram_bank_select: u8,
}

impl Bus for IORegisters {
    fn read(&mut self, address: u16) -> Result<u8, crate::memory::MemoryError> {
        match address {
            0xFF00 => return Ok(self.joypad_input),
            0xFF01..=0xFF02 => {
                let offset = address - 0xFF01;
                return Ok(self.serial_transfer[offset as usize]);
            }
            0xFF04..=0xFF07 => {
                let offset = address - 0xFF04;
                return Ok(self.timer_divider[offset as usize]);
            }
            0xFF10..=0xFF26 => {
                let offset = address - 0xFF10;
                return Ok(self.sound[offset as usize]);
            }
            0xFF30..=0xFF3F => {
                let offset = address - 0xFF30;
                return Ok(self.wave_pattern[offset as usize]);
            }
            0xFF40..=0xFF4B => {
                let offset = address - 0xFF40;
                return Ok(self.lcd_registers[offset as usize]);
            }
            0xFF4F => return Ok(self.vram_bank_select),
            0xFF50 => return Ok(self.boot_rom),
            0xFF51..=0xFF55 => {
                let offset = address - 0xFF51;
                return Ok(self.vram_dma[offset as usize]);
            }
            0xFF68..=0xFF69 => {
                let offset = address - 0xFF68;
                return Ok(self.bg_palletes[offset as usize]);
            }
            0xFF70 => return Ok(self.wram_bank_select),
            _ => {
                println!("Unsupported address {:02x}", address);
                Ok(0)
            }
        }
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), crate::memory::MemoryError> {
        match address {
            0xFF00 => {
                self.joypad_input = value;
                Ok(())
            }
            0xFF01..=0xFF02 => {
                let offset = address - 0xFF01;
                self.serial_transfer[offset as usize] = value;
                Ok(())
            }
            0xFF04..=0xFF07 => {
                let offset = address - 0xFF04;
                self.timer_divider[offset as usize] = value;
                Ok(())
            }
            0xFF10..=0xFF26 => {
                let offset = address - 0xFF10;
                self.sound[offset as usize] = value;
                Ok(())
            }
            0xFF30..=0xFF3F => {
                let offset = address - 0xFF30;
                self.wave_pattern[offset as usize] = value;
                Ok(())
            }
            0xFF40..=0xFF4B => {
                let offset = address - 0xFF40;
                self.lcd_registers[offset as usize] = value;
                Ok(())
            }
            0xFF4F => {
                self.vram_bank_select = value;
                Ok(())
            }
            0xFF50 => {
                self.boot_rom = value;
                Ok(())
            }
            0xFF51..=0xFF55 => {
                let offset = address - 0xFF51;
                self.vram_dma[offset as usize] = value;
                Ok(())
            }
            0xFF68..=0xFF69 => {
                let offset = address - 0xFF68;
                self.bg_palletes[offset as usize] = value;
                Ok(())
            }
            0xFF70 => {
                self.wram_bank_select = value;
                Ok(())
            }
            // _ => Err(crate::MemoryError::InvalidWrite(address)),
            _ => {
                println!("Unsupported address {:02x}", address);
                Ok(())
            }
        }
    }
}
