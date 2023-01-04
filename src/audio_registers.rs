use crate::{memory::MemoryError, Bus};

#[derive(Default)]
pub struct AudioRegisters {
    /// Address- 0xFF10-0xFF14
    nr1: [u8; 5],
    /// Address- 0xFF16-0xFF19
    nr2: [u8; 4],
    /// Address- 0xFF1A-0xFF1E
    nr3: [u8; 5],
    /// Address - 0xFF30 - 0xFF3F
    wave_pattern: [u8; 0xF],
    /// Address - 0xFF20 - 0xFF23
    nr4: [u8; 4],
    /// Address - 0xFF24 - 0xFF26
    nr5: [u8; 3],
}

impl Bus for AudioRegisters {
    fn read(&mut self, address: u16) -> Result<u8, crate::memory::MemoryError> {
        let result = match address {
            0xFF10..=0xFF14 => {
                let index = address - 0xFF10;
                self.nr1[index as usize]
            }
            0xFF16..=0xFF19 => {
                let index = address - 0xFF16;
                self.nr2[index as usize]
            }
            0xFF1A..=0xFF1E => {
                let index = address - 0xFF1A;
                self.nr3[index as usize]
            }
            0xFF20..=0xFF23 => {
                let index = address - 0xFF20;
                self.nr4[index as usize]
            }
            0xFF24..=0xFF26 => {
                let index = address - 0xFF24;
                self.nr5[index as usize]
            }
            0xFF30..=0xFF3F => {
                let index = address - 0xFF30;
                self.wave_pattern[index as usize]
            }

            _ => return Err(MemoryError::InvalidRead(address)),
        };
        Ok(result)
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), crate::memory::MemoryError> {
        match address {
            0xFF10..=0xFF14 => {
                let index = address - 0xFF10;
                self.nr1[index as usize] = value;
            }
            0xFF16..=0xFF19 => {
                let index = address - 0xFF16;
                self.nr2[index as usize] = value;
            }
            0xFF1A..=0xFF1E => {
                let index = address - 0xFF1A;
                self.nr3[index as usize] = value;
            }
            0xFF20..=0xFF23 => {
                let index = address - 0xFF20;
                self.nr4[index as usize] = value;
            }
            0xFF24..=0xFF26 => {
                let index = address - 0xFF24;
                self.nr5[index as usize] = value;
            }
            0xFF30..=0xFF3F => {
                let index = address - 0xFF30;
                self.wave_pattern[index as usize] = value;
            }
            _ => return Err(MemoryError::InvalidWrite(address)),
        };
        Ok(())
    }
}
