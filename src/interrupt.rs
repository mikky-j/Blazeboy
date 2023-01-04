use std::{collections::VecDeque, fmt::Display};

use crate::{cpu::CpuError, io_registers::IORegisters, EmulatorError, Wrapper};

pub trait InterruptHandler<T: Into<EmulatorError>> {
    fn handle_interrupt(&mut self, return_vector: u16) -> Result<(), T>;
    fn get_interrupt_status(&self) -> bool;
}

#[derive(Clone, Copy)]
pub enum InterruptSource {
    Vblank,
    LcdStat,
    Timer,
    Serial,
    Joypad,
}

impl Display for InterruptSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            InterruptSource::Vblank => "Vblank",
            InterruptSource::LcdStat => "LcdStat",
            InterruptSource::Timer => "Timer",
            InterruptSource::Serial => "Serial",
            InterruptSource::Joypad => "Joypad",
        }
        .fmt(f)
    }
}

impl From<u8> for InterruptSource {
    fn from(value: u8) -> Self {
        use InterruptSource::*;
        match value {
            0 => Vblank,
            1 => LcdStat,
            2 => Timer,
            3 => Serial,
            4 => Joypad,
            _ => unreachable!(),
        }
    }
}

impl InterruptSource {
    pub fn get_address(&self) -> u16 {
        match self {
            Self::Vblank => 0x0040,
            Self::LcdStat => 0x0048,
            Self::Timer => 0x0050,
            Self::Serial => 0x0058,
            Self::Joypad => 0x0060,
            // _ => panic!("Tried to get the return vector of an unknown Interrupt Source"),
        }
    }
}

pub struct Interrupt<T>
where
    T: InterruptHandler<CpuError>,
{
    handler: Wrapper<T>,
    io_registers: Wrapper<IORegisters>,
    interrupt_buffer: VecDeque<InterruptSource>,
}

impl<T> Interrupt<T>
where
    T: InterruptHandler<CpuError>,
{
    pub fn new(handler: Wrapper<T>, io_registers: Wrapper<IORegisters>) -> Self {
        Interrupt {
            handler,
            io_registers,
            interrupt_buffer: VecDeque::with_capacity(5),
        }
    }

    #[inline]
    pub fn add_source(&mut self, source: InterruptSource) {
        self.interrupt_buffer.push_front(source);
    }

    ///! THIS WILL CAUSE PROBLEMS IN THE FUTURE
    pub fn handle_interrupt(&mut self) -> Result<(), CpuError> {
        if let Some(interrupt_source) = self.interrupt_buffer.pop_back() {
            if !self.handler.borrow().get_interrupt_status()
                && !self
                    .io_registers
                    .borrow()
                    .check_interrupt_enable(interrupt_source)
            {
                self.add_source(interrupt_source);
                return Ok(());
            }
            println!("Handling `{interrupt_source}` interrupt");
            self.handler
                .borrow_mut()
                .handle_interrupt(interrupt_source.get_address())?;
            self.io_registers
                .borrow_mut()
                .clear_interrupt_bit(interrupt_source);
        }
        Ok(())
    }
}
