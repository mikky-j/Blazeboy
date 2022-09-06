mod cartridge;
mod cpu;
mod cpu_registers;
mod instruction;
mod instruction_data;
mod io_registers;
mod memory;
mod ppu;
mod ppu_registers;

use std::{cell::RefCell, fmt::Display, fs::File, io::BufReader, rc::Rc};

use cartridge::{Cartridge, CartridgeError};
use cpu::Cpu;
use memory::{Memory, MemoryError};
use ppu::Ppu;
use ppu_registers::PPURegisters;

/// This is a wrapper type that enables other modules to store a reference to the type T
pub type Wrapper<T> = Rc<RefCell<T>>;


pub enum EmulatorError {
    CatridgeError(CartridgeError),
    MemoryError(MemoryError),
}

impl Display for EmulatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EmulatorError::CatridgeError(v) => write!(f, "{v}"),
            EmulatorError::MemoryError(v) => write!(f, "{}", v.output()),
        }
    }
}

pub struct Emulator {
    cpu: Cpu<Memory>,
    ppu: Ppu
}

impl Emulator {
    pub fn new(cart_filename: &str) -> Result<Self, EmulatorError> {
        let cartridge =
            Cartridge::new(cart_filename).map_err(|v| EmulatorError::CatridgeError(v))?;
        let ppu_registers = Rc::new(RefCell::new(PPURegisters::new()));
        let io_registers = Rc::new(RefCell::new(io_registers::IORegisters::default()));
        let boot_file = File::open("src/roms/boot.gb").map_err(|_| {
            EmulatorError::CatridgeError(CartridgeError::FileDoesNotExist(format!(
                "src/roms/boot.gb"
            )))
        })?;
        let boot_rom = BufReader::with_capacity(256, boot_file);
        let memory = Memory::new(cartridge, boot_rom, io_registers, ppu_registers.clone());
        let mem_ref = Rc::new(RefCell::new(memory));
        let cpu = Cpu::new(mem_ref.clone());
        let ppu = Ppu::new(ppu_registers.clone());
        Ok(Emulator { cpu, ppu })
    }

    pub fn run(&mut self) -> Result<(), EmulatorError> {
        loop {
            self.cpu.fetch()?;
            self.cpu.execute()?;
            // std::thread::sleep(std::time::Duration::from_millis(250))
        }
    }
}

pub fn get_bit(value: u8, bit: u8) -> u8 {
    (value >> bit) & 1
}

pub fn set_bit(value: u8, bit: u8, on: bool) -> u8 {
    if on {
        value | 1 << bit
    } else {
        value & (0xFF ^ (1 << bit))
    }
}

/// This constructs a 16 bit value in the following way
/// ```rust,noplayground
/// hi_byte << 8 | lo_byte
/// ```
pub fn construct_16bit(val1: u8, val2: u8) -> u16 {
    (val1 as u16) << 8 | val2 as u16
}

pub fn split_16bit(value: u16) -> (u8, u8) {
    ((value >> 8) as u8, (value & 0xff) as u8)
}

pub trait Bus {
    fn read(&mut self, address: u16) -> Result<u8, MemoryError>;
    fn write(&mut self, address: u16, value: u8) -> Result<(), MemoryError>;
}
