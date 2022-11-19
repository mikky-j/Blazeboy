use crate::{
    audio_registers::AudioRegisters, get_bit, interrupt::InterruptSource, set_bit, Bus,
    InterruptRef, Wrapper,
};

#[derive(Default)]
pub struct IORegisters {
    interrupt: Option<InterruptRef>,
    // 0xFF00
    pub joypad_input: u8,
    // 0xFF01 - 0xFF02
    pub serial_transfer: [u8; 2],
    // 0xFF04 - 0xFF07
    pub timer_divider: [u8; 4],
    // 0xFF0F
    pub interrupt_flag: u8,
    // 0xFF10 - 0xFF26
    pub audio_register: Wrapper<AudioRegisters>,
    // 0xFF30 - 0xFF3F
    pub wave_pattern: [u8; 16],
    // 0xFF4F
    pub vram_bank_select: u8,
    // 0xFF50
    pub boot_rom: u8,
    // 0xFF51 - 0xFF55
    pub vram_dma: [u8; 5],
    // 0xFF68 - 0xFF69
    pub bg_palletes: [u8; 2],
    // 0xFF70
    pub wram_bank_select: u8,
    // 0xFFFF
    pub interrupt_enable: u8,
}

impl IORegisters {
    pub fn add_interrupt_handler(&mut self, interrupt: InterruptRef) {
        self.interrupt = Some(interrupt);
    }

    /// This method takes an interrupt source and checks if the bit from the "Interrupt Enable" register is set for that interrupt
    pub fn check_interrupt_enable(&self, interrupt: InterruptSource) -> bool {
        use InterruptSource::*;
        let bit = match interrupt {
            Vblank => 0,
            LcdStat => 1,
            Timer => 2,
            Serial => 3,
            Joypad => 4,
        };
        get_bit!(self.interrupt_enable, bit) == 1
    }

    /// This clears the bit of the interrupt source in the `IF` register
    pub fn clear_interrupt_bit(&mut self, interrupt: InterruptSource) {
        use InterruptSource::*;
        let bit = match interrupt {
            Vblank => 0,
            LcdStat => 1,
            Timer => 2,
            Serial => 3,
            Joypad => 4,
        };
        self.interrupt_flag = set_bit!(self.interrupt_flag, bit, false);
    }

    pub fn handle_interrupt(&mut self, interrupt_data: u8) {
        use InterruptSource::*;
        let get_source_from_bits = |data: u8| match data {
            0 => Vblank,
            1 => LcdStat,
            2 => Timer,
            3 => Serial,
            4 => Joypad,
            _ => panic!("An error occurred"),
        };
        let mut interrupt_sources = Vec::with_capacity(5);
        for i in 0..=4 {
            if get_bit!(interrupt_data, i) == 1 {
                interrupt_sources.push(get_source_from_bits(i));
            }
        }

        self.interrupt
            .clone()
            .unwrap()
            .borrow_mut()
            .add_sources(interrupt_sources)
    }
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
            0xFF0F => return Ok(self.interrupt_flag),
            0xFF10..=0xFF26 | 0xFF30..=0xFF3F => {
                return self.audio_register.borrow_mut().read(address)
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
            0xFFFF => return Ok(self.interrupt_enable),
            _ => Err(crate::MemoryError::InvalidRead(address)),
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
            0xFF0F => {
                self.interrupt_flag = value;
                self.handle_interrupt(value);
                Ok(())
            }
            0xFF10..=0xFF26 | 0xFF30..=0xFF3F => {
                return self.audio_register.borrow_mut().write(address, value)
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
            0xFFFF => {
                self.interrupt_enable = value;
                Ok(())
            }
            _ => Err(crate::MemoryError::InvalidWrite(address)),
            // _ => {
            //     println!("Unsupported address {:02x}", address);
            //     Ok(())
            // }
        }
    }
}
