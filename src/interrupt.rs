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
    interrupt_buffer: Vec<InterruptSource>,
}

impl<T> Interrupt<T>
where
    T: InterruptHandler<CpuError>,
{
    pub fn new(handler: Wrapper<T>, io_registers: Wrapper<IORegisters>) -> Self {
        Interrupt {
            handler,
            io_registers,
            interrupt_buffer: vec![],
        }
    }

    pub fn add_sources(&mut self, sources: Vec<InterruptSource>) {
        for source in sources {
            self.interrupt_buffer.insert(0, source);
        }
    }

    pub fn handle_interrupt(&mut self) -> Result<(), CpuError> {
        if self.interrupt_buffer.len() > 0 {
            let interrupt_source = self.interrupt_buffer.pop().unwrap();
            if !self.handler.borrow().get_interrupt_status()
                && !self
                    .io_registers
                    .borrow()
                    .check_interrupt_enable(interrupt_source)
            {
                return Ok(());
            }
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
