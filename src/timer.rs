use crate::{
    get_bit, get_bits,
    memory::{Memory, MemoryError},
    Bus, Wrapper,
};

#[derive(Default)]
pub struct TimerRegisters {
    div: u16,
    tima: u8,
    tma: u8,
    tac: u8,
    tima_rate: u16,
}

impl TimerRegisters {
    pub fn new() -> Self {
        Self {
            tima_rate: 1024,
            ..Default::default()
        }
    }
    fn get_tima_rate(value: u8) -> u16 {
        match get_bits!(value, 2) {
            0b00 => 1024,
            0b01 => 16,
            0b10 => 64,
            0b11 => 256,
            _ => unreachable!(),
        }
    }
}

impl Bus for TimerRegisters {
    fn read(&mut self, address: u16) -> Result<u8, crate::memory::MemoryError> {
        let data = match address {
            0xFF04 => (self.div >> 8) as u8,
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            0xFF07 => self.tac,
            _ => return Err(MemoryError::InvalidRead(address)),
        };
        Ok(data)
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), crate::memory::MemoryError> {
        match address {
            0xFF04 => self.div = 0,
            0xFF05 => self.tima = value,
            0xFF06 => self.tma = value,
            0xFF07 => {
                self.tima_rate = Self::get_tima_rate(value);
                self.tac = value;
            }
            _ => return Err(MemoryError::InvalidWrite(address)),
        }
        Ok(())
    }
}
pub struct Timer {
    last_edge: u8,
    last_timer_enable: bool,
    tima_carried: bool,
    cycles: u16,
    memory: Wrapper<Memory>,
    register: Wrapper<TimerRegisters>,
}

impl Timer {
    pub fn new(register: Wrapper<TimerRegisters>, memory: Wrapper<Memory>) -> Self {
        Timer {
            tima_carried: false,
            last_timer_enable: false,
            last_edge: 0,
            register,
            memory,
            cycles: 0,
        }
    }

    /// We use the current `tima_rate` to determine the div bit<br>
    /// This should be faster than looking at the TAC register because
    /// we don't have to perfom the bitwise operation to get the two lower bits
    fn get_div_bit(value: u16) -> u8 {
        match value {
            //0b00
            1024 => 9,
            //0b01
            16 => 3,
            //0b10
            64 => 5,
            //0b11
            256 => 7,
            _ => unreachable!(),
        }
    }

    //TODO: Add use the falling edge of the div register to keep track of cycles
    pub fn tick(&mut self) {
        let mut register = self.register.borrow_mut();
        let div_bit = Self::get_div_bit(register.tima_rate);
        let timer_enable = get_bit!(register.tac, 2);
        let current_edge = get_bit!(register.div, div_bit) as u8 & timer_enable;
        if self.last_timer_enable && self.last_edge == 1 && current_edge == 0 {
            if self.cycles == register.tima_rate {
                (register.tima, self.tima_carried) = register.tima.overflowing_add(1);
                self.cycles = 0;
            }
        }
        if self.tima_carried && self.cycles == 4 && register.tima == 0 {
            register.tima = register.tma;
            self.memory.borrow_mut().write(0xFF0F, 0b100).unwrap();
            self.tima_carried = false;
        }
        self.last_timer_enable = timer_enable == 1;
        self.last_edge = current_edge;
        register.div = register.div.wrapping_add(1);
        self.cycles += 1;
    }
}
