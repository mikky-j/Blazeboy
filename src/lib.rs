mod cartridge;
mod cpu;
mod cpu_registers;
mod instruction;
mod instruction_data;
mod interrupt;
mod io_registers;
mod memory;
mod ppu;
mod ppu_registers;

use std::{cell::RefCell, fmt::Display, fs::File, io::BufReader, rc::Rc};

use cartridge::{Cartridge, CartridgeError};
use cpu::{Cpu, CpuError};
use interrupt::Interrupt;
use memory::{Memory, MemoryError};
use ppu::{PPUError, Ppu};
use ppu_registers::PPURegisters;

/// This is a wrapper type that enables other modules to store a reference to the type T
pub type Wrapper<T> = Rc<RefCell<T>>;

/// This is just an alias for the Wrapper of the Interrupt type. I am too lazy to be writing that long line of code
pub type InterruptRef = Wrapper<Interrupt<Cpu<Memory>>>;

/// This fuction just returns a wrapper type of a value
pub fn wrap_with_wrapper<T>(value: T) -> Wrapper<T> {
    Rc::new(RefCell::new(value))
}

pub enum EmulatorError {
    CPUError(CpuError),
    CatridgeError(CartridgeError),
    MemoryError(MemoryError),
    PPUError(PPUError),
}

impl Display for EmulatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EmulatorError::CatridgeError(v) => write!(f, "{v}"),
            EmulatorError::CPUError(v) => write!(f, "{v}"),
            EmulatorError::MemoryError(v) => write!(f, "{}", v),
            EmulatorError::PPUError(v) => v.fmt(f),
        }
    }
}

pub struct Emulator {
    cpu: Wrapper<Cpu<Memory>>,
    master_interrupt: InterruptRef,
    ppu: Ppu,
}

impl Emulator {
    pub fn new(cart_filename: &str) -> Result<Self, EmulatorError> {
        let cartridge =
            Cartridge::new(cart_filename).map_err(|v| EmulatorError::CatridgeError(v))?;
        let ppu_registers = wrap_with_wrapper(PPURegisters::new());
        let io_registers = wrap_with_wrapper(io_registers::IORegisters::default());
        let boot_file = File::open("src/roms/boot.gb").map_err(|_| {
            EmulatorError::CatridgeError(CartridgeError::FileDoesNotExist(format!(
                "src/roms/boot.gb"
            )))
        })?;
        let boot_rom = BufReader::with_capacity(256, boot_file);
        let memory = Memory::new(
            cartridge,
            boot_rom,
            io_registers.clone(),
            ppu_registers.clone(),
        );
        let mem_ref = wrap_with_wrapper(memory);
        let cpu = Cpu::new(mem_ref.clone());
        let cpu_ref = wrap_with_wrapper(cpu);
        let interrupt = Interrupt::new(cpu_ref.clone(), io_registers.clone());
        let interrupt_ref = wrap_with_wrapper(interrupt);
        io_registers
            .borrow_mut()
            .add_interrupt_handler(interrupt_ref.clone());
        let ppu = Ppu::new(ppu_registers.clone(), interrupt_ref.clone());
        Ok(Emulator {
            cpu: cpu_ref,
            ppu,
            master_interrupt: interrupt_ref.clone(),
        })
    }

    pub fn run(&mut self) -> Result<(), EmulatorError> {
        loop {
            self.cpu.borrow_mut().execute()?;
            self.ppu.execute().map_err(|v| EmulatorError::PPUError(v))?;
            self.master_interrupt.borrow_mut().handle_interrupt()?
            // std::thread::sleep(std::time::Duration::from_millis(250))
        }
    }
}

/// This is a macro that gets a bit of a number
/// ```rust
/// use gameboy_emulator::get_bit;
/// fn main() {
///     let number: u8 = 0b11111011;
///     assert_eq!(get_bit!(number, 3), 0b1);
/// }
/// ```
#[macro_export]
macro_rules! get_bit {
    ($value:expr, $bit:expr) => {
        ($value >> $bit) & 1
    };
}

/// This is a macro that sets a bit of a number either to 1 or 0
/// ```rust
/// use gameboy_emulator::{clear_bits, get_bits};
/// use gameboy_emulator::set_bit;
/// fn main() {
///     let number: u8 = 0b11111111;
///     assert_eq!(set_bit!(number, 3, false), 0b11111011);
/// }
/// ```
#[macro_export]
macro_rules! set_bit {
    ($value:expr, $bit:expr, $on: expr) => {{
        use crate::{clear_bits, get_bits};
        if $on {
            $value | 1 << $bit
        } else {
            let end = get_bits!($value, if $bit > 0 { $bit - 1 } else { $bit });
            clear_bits!($value, $bit) | end
        }
    }};
}

/// This is a macro that gets all the bits of a number up until a particular bit position
/// ```rust
/// use gameboy_emulator::get_bits;
/// fn main() {
///     let number: u8 = 0b11111111;
///     assert_eq!(get_bits!(number, 3), 0b111);
/// }
/// ```
#[macro_export]
macro_rules! get_bits {
    ($value:expr, $bit_pos: expr) => {
        $value & ((1 << $bit_pos) - 1)
    };
}

/// This is a macro that clears all the bits of a number up until a particular bit position
/// ```rust
/// use gameboy_emulator::clear_bits;
/// fn main() {
///     let number: u8 = 0b1111111;
///     assert_eq!(clear_bits!(number, 6), 0b1000000);
/// }
/// ```
#[macro_export]
macro_rules! clear_bits {
    ($x:expr, $bit_pos:expr) => {
        ($x >> $bit_pos) << $bit_pos
    };
}
/// This constructs a 16 bit value in the following way
/// ```rust,noplayground
/// hi_byte << 8 | lo_byte
/// ```
pub fn construct_16bit(hi_byte: u8, lo_byte: u8) -> u16 {
    (hi_byte as u16) << 8 | lo_byte as u16
}

pub fn split_16bit(value: u16) -> (u8, u8) {
    ((value >> 8) as u8, (value & 0xff) as u8)
}

pub trait Bus {
    fn read(&mut self, address: u16) -> Result<u8, MemoryError>;
    fn write(&mut self, address: u16, value: u8) -> Result<(), MemoryError>;
}
