use std::fmt::Display;

use crate::{
    construct_16bit,
    cpu_registers::{CpuRegisters, Flags, Registers},
    get_bit,
    instruction::{Instruction, InstructionConditions},
    instruction_data::AddressingMode::*,
    instruction_data::InstructionType,
    interrupt::InterruptHandler,
    memory::MemoryError,
    set_bit, split_16bit, Bus, EmulatorError, Wrapper,
};

macro_rules! unsupported_params {
    ($x: expr) => {
        panic!(
            "Addressing mode `{}` not supported for `{}` instruction",
            $x.instruction.address_mode, $x.instruction.instruction_type
        )
    };
}

/// This is an enum that holds all possible CPU Runtime Errors
pub enum CpuError {
    InvalidRegister(String),
    MemoryError(MemoryError),
}

impl Display for CpuError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CpuError::InvalidRegister(v) => {
                write!(f, "There was an error that involved a register\nError: {v}")
            }
            CpuError::MemoryError(v) => {
                write!(f, "A MemoryError was thrown by Cpu.\nError: `{v}`")
            }
        }
    }
}

impl From<CpuError> for EmulatorError {
    fn from(value: CpuError) -> Self {
        EmulatorError::CPUError(value)
    }
}

impl From<MemoryError> for CpuError {
    fn from(value: MemoryError) -> Self {
        Self::MemoryError(value)
    }
}

type CpuResult<T> = Result<T, CpuError>;

pub struct Cpu<T>
where
    T: Bus,
{
    halted: bool,
    // stepping: bool,
    memory: Wrapper<T>,
    registers: CpuRegisters,
    opcode: u8,
    cb: bool,
    ime: bool,
    cycles: u8,
    // fetched_data: u8,
    // is_dest_mem: bool,
    instruction: Instruction,
    // mem_dest: Option<u16>,
}

impl<T> InterruptHandler<CpuError> for Cpu<T>
where
    T: Bus,
{
    fn handle_interrupt(&mut self, return_vector: u16) -> Result<(), CpuError> {
        if self.ime {
            // Pushing the current value of the program counter to the stack
            let (hi, lo) = split_16bit(self.registers.get_16bit(Registers::PC));
            let sp = self.registers.get_16bit(Registers::SP);
            self.write(sp - 1, hi)?;
            self.write(sp - 2, lo)?;
            self.registers.set_16bit(Registers::SP, sp - 2);
            self.registers.set_16bit(Registers::PC, return_vector);
            // Setting the Interrupt Master Enable to false so that
            self.ime = false;
        }
        Ok(())
    }

    fn get_interrupt_status(&self) -> bool {
        self.ime
    }
}

impl<T> Cpu<T>
where
    T: Bus,
{
    pub fn new(memory: Wrapper<T>) -> Self {
        Cpu {
            halted: false,
            // stepping: true,
            ime: true,
            memory: memory,
            cycles: 0,
            registers: CpuRegisters::new(),
            cb: false,
            opcode: 0,
            // fetched_data: 0,
            // is_dest_mem: false,
            instruction: Instruction::new(),
            // mem_dest: None,
        }
    }

    fn handle_dma_transfer(&mut self) -> CpuResult<()> {
        let value = self.read(0xFF46)? as u16;
        if value <= 0xDF {
            println!("ENTERED DMA TRANSFER");
            let start_address = value << 8;
            for address in 0..0x100 {
                let data = self.read(start_address + address)?;
                self.write(0xFF00 + address, data)?;
            }
            self.write(0xFF46, 0xFF)?;
            self.cycles += 40;
        }
        Ok(())
    }

    fn read(&self, address: u16) -> CpuResult<u8> {
        let result = self.memory.borrow_mut().read(address)?;
        Ok(result)
    }

    fn write(&self, address: u16, value: u8) -> CpuResult<()> {
        println!("Writing {value:02x} to {address:04x}");
        self.memory.borrow_mut().write(address, value)?;
        Ok(())
    }

    pub fn fetch(&mut self) -> CpuResult<()> {
        self.opcode = self.fetch_data()?;
        if self.cb {
            self.instruction.update_cb(self.opcode);
            self.cb = false;
        } else {
            self.instruction.update(self.opcode);
        }
        Ok(())
    }

    fn fetch_data_16bit(&mut self) -> CpuResult<u16> {
        let lo = self.fetch_data()?;
        let hi = self.fetch_data()?;
        Ok(construct_16bit(hi, lo))
    }

    fn fetch_data(&mut self) -> CpuResult<u8> {
        let pc = self.registers.get_16bit(Registers::PC);
        let data = self.read(pc)?;
        self.registers.set_16bit(Registers::PC, pc + 1);
        Ok(data)
    }

    fn execute_instruction(&mut self) -> CpuResult<()> {
        use InstructionType::*;
        match self.instruction.instruction_type {
            Nop => {
                std::thread::sleep(std::time::Duration::from_secs(1));
                Ok(())
            }
            Halt => self.halt(),
            Stop => self.stop(),
            Di => self.di(),
            Ei => self.ei(),
            Cb => self.cb(),

            // Control instructions
            Ld => self.ld(),
            Ldh => self.ldh(),
            Pop => self.pop(),
            Push => self.push(),
            Jp => self.jp(),
            Jr => self.jr(),
            Call => self.call(),
            Rst => self.rst(),
            Ret => self.ret(),
            Reti => self.reti(),

            // Arithmetic Operations
            Inc => self.inc(),
            Dec => self.dec(),
            And => self.and(),
            Or => self.or(),
            Xor => self.xor(),
            Add => self.add(),
            Sub => self.sub(),
            Adc => self.adc(),
            Sbc => self.sbc(),
            Cp => self.cp(),
            Cpl => self.cpl(),
            Ccf => self.ccf(),
            Daa => self.daa(),
            Scf => self.scf(),

            // Bit Operations
            Rlca => self.rlca(),
            Rla => self.rla(),
            Rrca => self.rrca(),
            Rra => self.rra(),
            Rlc => self.rlc(),
            Rrc => self.rrc(),
            Rl => self.rl(),
            Rr => self.rr(),
            Sla => self.sla(),
            Sra => self.sra(),
            Swap => self.swap(),
            Srl => self.srl(),
            Bit => self.bit(),
            Set => self.set(),
            Res => self.res(),
            _ => unimplemented!("Not yet Implemented this feature"),
        }
    }

    pub fn show_debug_output(&self) -> CpuResult<()> {
        println!("{:04x}: {:<5} ({:02x} {:02x} {:02x}) AF: {:04x} BC: {:04x} DE: {:04x} HL: {:04x} SP: {:04x}",
            self.registers.get_16bit(Registers::PC) - 1,
            self.instruction.instruction_type,
            self.opcode,
            self.read(self.registers.get_16bit(Registers::PC))?,
            self.read(self.registers.get_16bit(Registers::PC)+1)?,
            self.registers.get_16bit(Registers::AF),
            self.registers.get_16bit(Registers::BC),
            self.registers.get_16bit(Registers::DE),
            self.registers.get_16bit(Registers::HL),
            self.registers.get_16bit(Registers::SP),
        );
        Ok(())
    }

    fn update_cycle(&mut self) {
        self.cycles = self.instruction.get_instruction_cycle();
    }

    pub fn execute(&mut self) -> CpuResult<()> {
        if self.cycles <= 1 {
            self.fetch()?;
            self.show_debug_output()?;
            self.update_cycle();
            self.execute_instruction()?;
            self.handle_dma_transfer()?;
        }
        self.cycles -= 1;
        Ok(())
    }

    fn ld(&mut self) -> CpuResult<()> {
        let first_reg = self.instruction.register1;
        let second_reg = self.instruction.register2;
        match self.instruction.address_mode {
            Reg_Reg => {
                if CpuRegisters::is_16bit_reg(first_reg) {
                    self.registers
                        .set_16bit(first_reg, self.registers.get_16bit(second_reg))
                } else {
                    self.registers
                        .set_8bit(first_reg, self.registers.get_8bit(second_reg))
                }
            }
            Reg_d8 => {
                let data = self.fetch_data()?;
                self.registers.set_8bit(first_reg, data);
            }
            Reg_d16 => {
                let data = self.fetch_data_16bit()?;
                self.registers.set_16bit(first_reg, data);
            }
            Reg_Memreg => {
                let address = if second_reg == Registers::C {
                    0xFF00 | self.registers.get_8bit(second_reg) as u16
                } else {
                    self.registers.get_16bit(second_reg)
                };
                let value = self.read(address)?;
                self.registers.set_8bit(first_reg, value);
            }
            Memreg_Reg => {
                let address = if first_reg == Registers::C {
                    0xFF00 | self.registers.get_8bit(first_reg) as u16
                } else {
                    self.registers.get_16bit(first_reg)
                };
                let data = self.registers.get_8bit(second_reg);
                self.write(address, data)?;
            }
            Memreg_d8 => {
                let data = self.fetch_data()?;
                let address = self.registers.get_16bit(first_reg);
                self.write(address, data)?;
            }
            Reg_a16 => {
                let address = self.fetch_data_16bit()?;
                if CpuRegisters::is_16bit_reg(first_reg) {
                    let lo = self.read(address)?;
                    let hi = self.read(address + 1)?;
                    let value = construct_16bit(hi, lo);
                    self.registers.set_16bit(first_reg, value);
                } else {
                    let value = self.read(address)?;
                    self.registers.set_8bit(first_reg, value);
                }
            }
            a16_Reg => {
                let address = self.fetch_data_16bit()?;
                if CpuRegisters::is_16bit_reg(second_reg) {
                    let (hi, lo) = split_16bit(self.registers.get_16bit(second_reg));
                    self.write(address, lo)?;
                    self.write(address + 1, hi)?;
                } else {
                    let data = self.registers.get_8bit(second_reg);
                    self.write(address, data)?;
                }
            }
            Reg_HLI => {
                let address = self.registers.get_16bit(second_reg);
                let value = self.read(address)?;
                self.registers.set_8bit(first_reg, value);
                self.registers.set_16bit(second_reg, address + 1);
            }
            Reg_HLD => {
                let address = self.registers.get_16bit(second_reg);
                let value = self.read(address)?;
                self.registers.set_8bit(first_reg, value);
                self.registers.set_16bit(second_reg, address - 1);
            }
            HLI_Reg => {
                let address = self.registers.get_16bit(first_reg);
                let value = self.registers.get_8bit(second_reg);
                self.write(address, value)?;
                self.registers.set_16bit(first_reg, address + 1);
            }
            HLD_Reg => {
                let address = self.registers.get_16bit(first_reg);
                let value = self.registers.get_8bit(second_reg);
                self.write(address, value)?;
                self.registers.set_16bit(first_reg, address - 1);
            }
            Reg_Reg_r8 => {
                let offset = i8::from_le_bytes([self.fetch_data()?]);
                let pc = self.registers.get_16bit(Registers::SP);
                let half_carry = (((pc as i16) & 0xf) - ((pc as i16) & 0xf)) & 0x10 == 0x10;
                let (value, carry) = if offset < 0 {
                    pc.overflowing_sub(offset.abs() as u16)
                } else {
                    pc.overflowing_add(offset.abs() as u16)
                };
                self.registers.set_16bit(Registers::HL, value);
                let flags = [
                    Flags::Zero(false),
                    Flags::Subtraction(false),
                    Flags::HalfCarry(half_carry),
                    Flags::Carry(carry),
                ];
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn cb(&mut self) -> CpuResult<()> {
        self.cb = true;
        Ok(())
    }

    fn ldh(&mut self) -> CpuResult<()> {
        let first_reg = self.instruction.register1;
        let second_reg = self.instruction.register2;
        match self.instruction.address_mode {
            Reg_a8 => {
                let address = 0xFF00 | self.fetch_data()? as u16;
                let data = self.read(address)?;
                self.registers.set_8bit(first_reg, data);
            }
            a8_Reg => {
                let address = 0xFF00 | self.fetch_data()? as u16;
                let data = self.registers.get_8bit(second_reg);
                self.write(address, data)?;
            }

            _ => unsupported_params!(&self),
        }
        Ok(())
    }
    fn push(&mut self) -> CpuResult<()> {
        let first_reg = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let (hi, lo) = split_16bit(self.registers.get_16bit(first_reg));
                let sp = self.registers.get_16bit(Registers::SP);
                self.write(sp - 1, hi)?;
                self.write(sp - 2, lo)?;
                self.registers.set_16bit(Registers::SP, sp - 2);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn pop(&mut self) -> CpuResult<()> {
        let first_reg = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                if CpuRegisters::is_16bit_reg(first_reg) {
                    let sp = self.registers.get_16bit(Registers::SP);
                    let lo = self.read(sp)?;
                    let hi = self.read(sp + 1)?;
                    let data = construct_16bit(hi, lo);
                    self.registers.set_16bit(first_reg, data);
                    self.registers.set_16bit(Registers::SP, sp + 2);
                } else {
                    unsupported_params!(&self)
                }
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn jp(&mut self) -> CpuResult<()> {
        let first_reg = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                if CpuRegisters::is_16bit_reg(first_reg) {
                    let address = self.registers.get_16bit(first_reg);
                    self.registers.set_16bit(Registers::PC, address);
                } else {
                    unsupported_params!(&self)
                }
            }
            a16 => {
                let address = self.fetch_data_16bit()?;
                if self.registers.check_condition(self.instruction.cond_type) {
                    self.registers.set_16bit(Registers::PC, address);
                }
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn jr(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            r8 => {
                let data = self.fetch_data()?;
                if self.registers.check_condition(self.instruction.cond_type) {
                    let offset = i8::from_le_bytes([data]);
                    println!("The value is {offset}");
                    let pc = self.registers.get_16bit(Registers::PC);
                    self.registers
                        .set_16bit(Registers::PC, (pc as i32 + offset as i32).abs() as u16);
                }
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn call(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            a16 => {
                let data = self.fetch_data_16bit()?;
                if self.registers.check_condition(self.instruction.cond_type) {
                    let (hi, lo) = split_16bit(self.registers.get_16bit(Registers::PC));
                    let sp = self.registers.get_16bit(Registers::SP);
                    self.write(sp - 1, hi)?;
                    self.write(sp - 2, lo)?;
                    self.registers.set_16bit(Registers::SP, sp - 2);
                    self.registers.set_16bit(Registers::PC, data);
                }
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn ret(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                if self.registers.check_condition(self.instruction.cond_type) {
                    let sp = self.registers.get_16bit(Registers::SP);
                    let lo = self.read(sp)?;
                    let hi = self.read(sp.wrapping_add(1))?;
                    let data = construct_16bit(hi, lo);
                    self.registers.set_16bit(Registers::PC, data);
                    self.registers.set_16bit(Registers::SP, sp.wrapping_add(2));
                }
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn reti(&mut self) -> CpuResult<()> {
        self.ime = true;
        self.ret()?;
        Ok(())
    }

    fn rst(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let pc = self.registers.get_16bit(Registers::PC);
                let ret_addr = self.opcode & 0x38;
                let (hi, lo) = split_16bit(pc);
                let sp = self.registers.get_16bit(Registers::SP);
                self.write(sp - 1, hi)?;
                self.write(sp - 2, lo)?;
                self.registers.set_16bit(Registers::SP, sp - 2);
                self.registers.set_16bit(Registers::PC, ret_addr as u16);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn halt(&mut self) -> CpuResult<()> {
        self.halted = true;
        Ok(())
    }

    // Todo
    fn stop(&mut self) -> CpuResult<()> {
        Ok(())
    }

    // Todo
    fn di(&mut self) -> CpuResult<()> {
        Ok(())
    }

    // Todo
    fn ei(&mut self) -> CpuResult<()> {
        Ok(())
    }

    fn ccf(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let carry = self.registers.check_condition(InstructionConditions::C);
                let flags = [
                    Flags::Subtraction(false),
                    Flags::HalfCarry(false),
                    Flags::Carry(!carry),
                ];
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn scf(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let flags = [
                    Flags::Subtraction(false),
                    Flags::HalfCarry(false),
                    Flags::Carry(true),
                ];
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    // Don't ask me how I know this function is written like this coz I don't know
    fn daa(&mut self) -> CpuResult<()> {
        let negative = self.registers.get_flag(Flags::Subtraction(true));
        let carry = self.registers.get_flag(Flags::Carry(true));
        let half_carry = self.registers.get_flag(Flags::HalfCarry(true));
        let value = self.registers.get_8bit(Registers::A);
        let mut flags: Vec<Flags> = vec![];
        if !negative {
            if carry || value > 0x99 {
                self.registers
                    .set_8bit(Registers::A, value.wrapping_add(0x60));
                flags.push(Flags::Carry(true));
            }
            if half_carry || (value & 0xf > 0x9) {
                self.registers
                    .set_8bit(Registers::A, value.wrapping_add(0x6));
            }
        } else {
            if carry {
                self.registers
                    .set_8bit(Registers::A, value.wrapping_sub(0x60));
            }
            if half_carry {
                self.registers
                    .set_8bit(Registers::A, value.wrapping_sub(0x6));
            }
        }
        flags.push(Flags::Zero(self.registers.get_8bit(Registers::A) == 0));
        flags.push(Flags::HalfCarry(false));
        self.registers.set_flags(&flags);
        Ok(())
    }

    fn cpl(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let flags = [Flags::Subtraction(true), Flags::HalfCarry(true)];
                let data = self.registers.get_8bit(Registers::A);
                self.registers.set_8bit(Registers::A, !data);
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
}

// ---------------ALU IMPLEMNTATION FOR CPU<T>---------------
impl<T> Cpu<T>
where
    T: Bus,
{
    fn inc(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                if CpuRegisters::is_16bit_reg(register) {
                    let value = self.registers.get_16bit(register);
                    self.registers.set_16bit(register, value.wrapping_add(1));
                } else {
                    let value = self.registers.get_8bit(register);
                    let half_carry = (value & 0xf) + 1 == 0x10;
                    self.registers.set_8bit(register, value.wrapping_add(1));
                    let flags = [
                        Flags::HalfCarry(half_carry),
                        Flags::Zero(value.wrapping_add(1) == 1),
                        Flags::Subtraction(false),
                    ];
                    self.registers.set_flags(&flags);
                }
            }
            Memreg => {
                let addr = self.registers.get_16bit(register);
                let value = self.read(addr)?;
                let half_carry = (value & 0xf) + 1 == 0x10;
                self.write(addr, value.wrapping_add(1))?;
                let flags = [
                    Flags::HalfCarry(half_carry),
                    Flags::Zero(value.wrapping_add(1) == 1),
                    Flags::Subtraction(false),
                ];
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn dec(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                if CpuRegisters::is_16bit_reg(register) {
                    let value = self.registers.get_16bit(register);
                    self.registers.set_16bit(register, value.wrapping_sub(1));
                } else {
                    let value = self.registers.get_8bit(register);
                    let half_carry = value & 0xf == 0;
                    self.registers.set_8bit(register, value.wrapping_sub(1));
                    let flags = [
                        Flags::HalfCarry(half_carry),
                        Flags::Zero(value.wrapping_sub(1) == 0),
                        Flags::Subtraction(true),
                    ];
                    self.registers.set_flags(&flags);
                }
            }
            Memreg => {
                let addr = self.registers.get_16bit(register);
                let value = self.read(addr)?;
                let half_carry = (value & 0xf) - 1 == 0x10;
                self.write(addr, value.wrapping_sub(1))?;
                let flags = [
                    Flags::HalfCarry(half_carry),
                    Flags::Zero(value.wrapping_sub(1) == 0),
                    Flags::Subtraction(true),
                ];
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn rlca(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let value = self.registers.get_8bit(Registers::A);
                let res = (value << 1) | value >> 7;
                let flags = [
                    Flags::Carry(get_bit!(value, 7) == 1),
                    Flags::Zero(false),
                    Flags::Subtraction(false),
                    Flags::HalfCarry(false),
                ];
                self.registers.set_flags(&flags);
                self.registers.set_8bit(Registers::A, res);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
    fn rrca(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let value = self.registers.get_8bit(Registers::A);
                let res = ((value & 1) << 7) | (value >> 1);
                let flags = [
                    Flags::Carry(value & 1 == 1),
                    Flags::Zero(false),
                    Flags::Subtraction(false),
                    Flags::HalfCarry(false),
                ];
                self.registers.set_8bit(Registers::A, res);
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn rla(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let value = self.registers.get_8bit(Registers::A);
                let res = value << 1 | get_bit!(self.registers.get_8bit(Registers::F), 4);
                self.registers.set_8bit(Registers::A, res);
                let flags = [
                    Flags::Carry(get_bit!(value, 7) == 1),
                    Flags::Zero(false),
                    Flags::Subtraction(false),
                    Flags::HalfCarry(false),
                ];
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn rra(&mut self) -> CpuResult<()> {
        match self.instruction.address_mode {
            Impl => {
                let value = self.registers.get_8bit(Registers::A);
                let res = (((self.registers.get_8bit(Registers::F) >> 4) & 1) << 7) | value >> 1;
                let flags = [
                    Flags::Carry(value & 1 == 1),
                    Flags::Zero(false),
                    Flags::Subtraction(false),
                    Flags::HalfCarry(false),
                ];
                self.registers.set_8bit(Registers::A, res);
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn add(&mut self) -> CpuResult<()> {
        let reg_1 = self.instruction.register1;
        let reg_2 = self.instruction.register2;
        match self.instruction.address_mode {
            Reg_Reg => {
                if CpuRegisters::is_16bit_reg(reg_1) {
                    let val_1 = self.registers.get_16bit(reg_1);
                    let val_2 = self.registers.get_16bit(reg_2);
                    let half_carry = ((val_1 & 0xfff) + (val_2 & 0xfff)) & 0x1000 == 0x1000;
                    let (value, carry) = val_1.overflowing_add(val_2);

                    let flags = [
                        Flags::HalfCarry(half_carry),
                        Flags::Zero(value == 0),
                        Flags::Subtraction(false),
                        Flags::Carry(carry),
                    ];
                    self.registers.set_16bit(reg_1, value);
                    self.registers.set_flags(&flags);
                } else {
                    let val_1 = self.registers.get_8bit(reg_1);
                    let val_2 = self.registers.get_8bit(reg_2);
                    let result = Instruction::add_8bit_base(&mut self.registers, val_1, val_2);
                    self.registers.set_8bit(reg_1, result);
                }
            }
            Reg_Memreg => {
                let val_1 = self.registers.get_8bit(reg_1);
                let address = self.registers.get_16bit(reg_2);
                let val_2 = self.read(address)?;
                let value = Instruction::add_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, value);
            }

            Reg_d8 => {
                let val_1 = self.registers.get_8bit(reg_1);
                let val_2 = self.fetch_data()?;
                let value = Instruction::add_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, value);
            }

            Reg_r8 => {
                let val_1 = self.registers.get_16bit(Registers::SP) as i32;
                let val_2 = i8::from_le_bytes([self.fetch_data()?]) as i32;
                let half_carry = ((val_1 & 0xfff) + (val_2 & 0xfff)) & 0x100 == 0x100;
                let (value, carry) = val_1.overflowing_add(val_2);
                let flags = [
                    Flags::HalfCarry(half_carry),
                    Flags::Zero(value == 0),
                    Flags::Subtraction(false),
                    Flags::Carry(carry),
                ];
                self.registers.set_16bit(reg_1, value as u16);
                self.registers.set_flags(&flags);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    // Todo: Check if the implementation is correct
    fn adc(&mut self) -> CpuResult<()> {
        let reg_1 = self.instruction.register1;
        let reg_2 = self.instruction.register2;
        match self.instruction.address_mode {
            Reg_Reg => {
                let val_1 = self.registers.get_8bit(reg_1);
                let val_2 = self.registers.get_8bit(reg_2);
                let result = Instruction::adc_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, result);
            }
            Reg_Memreg => {
                let val_1 = self.registers.get_8bit(reg_1);
                let address = self.registers.get_16bit(reg_2);
                let val_2 = self.read(address)?;
                let result = Instruction::adc_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, result);
            }

            Reg_d8 => {
                let val_1 = self.registers.get_8bit(reg_1);
                let val_2 = self.fetch_data()?;
                let result = Instruction::adc_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, result);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn sub(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.registers.get_8bit(register);
                let value = Instruction::sub_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, value);
            }
            Memreg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let address = self.registers.get_16bit(register);
                let val_2 = self.read(address)?;
                let value = Instruction::sub_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, value);
            }
            d8 => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.fetch_data()?;
                let value = Instruction::sub_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, value);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    // Todo: Check if the implementation is correct
    fn sbc(&mut self) -> CpuResult<()> {
        let reg_1 = self.instruction.register1;
        let reg_2 = self.instruction.register2;
        match self.instruction.address_mode {
            Reg_Reg => {
                let val_1 = self.registers.get_8bit(reg_1);
                let val_2 = self.registers.get_8bit(reg_2);
                let result = Instruction::sbc_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, result);
            }
            Reg_Memreg => {
                let val_1 = self.registers.get_8bit(reg_1);
                let address = self.registers.get_16bit(reg_2);
                let val_2 = self.read(address)?;
                let result = Instruction::sbc_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, result);
            }
            Reg_d8 => {
                let val_1 = self.registers.get_8bit(reg_1);
                let val_2 = self.fetch_data()?;
                let result = Instruction::sbc_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(reg_1, result);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
    fn cp(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.registers.get_8bit(register);
                Instruction::sub_8bit_base(&mut self.registers, val_1, val_2);
            }
            Memreg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let address = self.registers.get_16bit(register);
                let val_2 = self.read(address)?;
                Instruction::sub_8bit_base(&mut self.registers, val_1, val_2);
            }
            d8 => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.fetch_data()?;
                Instruction::sub_8bit_base(&mut self.registers, val_1, val_2);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn and(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.registers.get_8bit(register);
                let result = Instruction::and_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            Memreg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let address = self.registers.get_16bit(register);
                let val_2 = self.read(address)?;
                let result = Instruction::and_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            d8 => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.fetch_data()?;
                let result = Instruction::and_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn or(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.registers.get_8bit(register);
                let result = Instruction::or_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            Memreg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let address = self.registers.get_16bit(register);
                let val_2 = self.read(address)?;
                let result = Instruction::or_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            d8 => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.fetch_data()?;
                let result = Instruction::or_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn xor(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.registers.get_8bit(register);
                let result = Instruction::xor_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            Memreg => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let address = self.registers.get_16bit(register);
                let val_2 = self.read(address)?;
                let result = Instruction::xor_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            d8 => {
                let val_1 = self.registers.get_8bit(Registers::A);
                let val_2 = self.fetch_data()?;
                let result = Instruction::xor_8bit_base(&mut self.registers, val_1, val_2);
                self.registers.set_8bit(Registers::A, result);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn rlc(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::rlc_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::rlc_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn rrc(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::rrc_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::rrc_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn rl(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::rl_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::rl_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
    fn rr(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::rr_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::rr_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
    fn sla(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::sla_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::sla_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
    fn sra(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::sra_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::sra_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn swap(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::swap_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::swap_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn srl(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let result = Instruction::srl_8bit_base(&mut self.registers, value);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let result = Instruction::srl_8bit_base(&mut self.registers, value);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn bit(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        let (row, col) = (self.opcode >> 4, self.opcode & 0xf);
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let bit = ((row - 0x4) * 2) + if col > 0x7 { 1 } else { 0 };
                Instruction::bit_8bit_base(&mut self.registers, value, bit);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let bit = ((row - 0x4) * 2) + if col > 0x7 { 1 } else { 0 };
                Instruction::bit_8bit_base(&mut self.registers, value, bit);
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn res(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        let (row, col) = (self.opcode >> 4, self.opcode & 0xf);
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let bit = ((row - 0x8) * 2) + if col > 0x7 { 1 } else { 0 };
                let result = if get_bit!(value, bit) != 0 {
                    value ^ (1 << bit)
                } else {
                    value
                };
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let bit = ((row - 0x8) * 2) + if col > 0x7 { 1 } else { 0 };
                let result = if get_bit!(value, bit) != 0 {
                    value ^ (1 << bit)
                } else {
                    value
                };
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }

    fn set(&mut self) -> CpuResult<()> {
        let register = self.instruction.register1;
        let (row, col) = (self.opcode >> 4, self.opcode & 0xf);
        match self.instruction.address_mode {
            Reg => {
                let value = self.registers.get_8bit(register);
                let bit = ((row - 0xc) * 2) + if col > 0x7 { 1 } else { 0 };
                let result = set_bit!(value, bit, true);
                self.registers.set_8bit(register, result);
            }
            Memreg => {
                let address = self.registers.get_16bit(register);
                let value = self.read(address)?;
                let bit = ((row - 0xc) * 2) + if col > 0x7 { 1 } else { 0 };
                let result = set_bit!(value, bit, true);
                self.write(address, result)?;
            }
            _ => unsupported_params!(&self),
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::{fs::File, io::BufReader};

    use crate::{cartridge::Cartridge, io_registers, memory::Memory, wrap_with_wrapper};

    use super::*;
    #[test]
    fn test_memory() {
        let cartridge = Cartridge::new("src/roms/cpu_instrs.gb").unwrap();
        let ppu_registers = wrap_with_wrapper(crate::ppu_registers::PPURegisters::new());
        let io_registers = wrap_with_wrapper(io_registers::IORegisters::default());
        let boot_file = File::open("src/roms/boot.gb").unwrap();
        let boot_rom = BufReader::with_capacity(256, boot_file);
        let memory = Memory::new(
            cartridge,
            boot_rom,
            io_registers.clone(),
            ppu_registers.clone(),
        );
        let mem_ref = wrap_with_wrapper(memory);
        let cpu = Cpu::new(mem_ref.clone());
        cpu.memory
            .borrow_mut()
            .ppu_registers
            .borrow_mut()
            .write(0xFF40, 0x91)
            .unwrap();
    }
}
