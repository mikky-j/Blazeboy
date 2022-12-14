use crate::cpu_registers::{CpuRegisters, Flags, Registers};
use crate::{get_bit, instruction_data::*};
#[derive(Clone, Copy)]
pub enum InstructionConditions {
    None,
    C,
    Z,
    NC,
    NZ,
}

impl InstructionConditions {
    fn get_conditions(opcode: u8) -> Self {
        use InstructionConditions::*;
        let (row, col) = (opcode >> 4, opcode & 0xf);
        match (row, col) {
            (0x2 | 0xc, 0x0) | (0xc, 0x2 | 0x4) => NZ,
            (0x3 | 0xd, 0x0) | (0xd, 0x2 | 0x4) => NC,
            (0x2 | 0xc, 0x8) | (0xc, 0xa | 0xc) => Z,
            (0x3 | 0xd, 0x8) | (0xd, 0xa | 0xc) => C,
            _ => None,
        }
    }
}
pub struct Instruction {
    pub address_mode: AddressingMode,
    pub instruction_type: InstructionType,
    pub register1: Registers,
    pub register2: Registers,
    pub cond_type: InstructionConditions,
}

impl Instruction {
    pub fn new() -> Self {
        Instruction {
            address_mode: AddressingMode::Impl,
            instruction_type: InstructionType::None,
            register1: Registers::None,
            register2: Registers::None,
            cond_type: InstructionConditions::None,
        }
    }
    pub fn update(&mut self, opcode: u8) {
        self.address_mode = AddressingMode::get_addressing_mode(opcode);
        self.instruction_type = InstructionType::get_instruction(opcode);
        self.register1 = Registers::get_instuctions_left(opcode);
        self.register2 = Registers::get_instruction_right(opcode);
        self.cond_type = InstructionConditions::get_conditions(opcode);
    }

    pub fn update_cb(&mut self, opcode: u8) {
        self.address_mode = AddressingMode::get_addressing_mode_cb(opcode);
        self.instruction_type = InstructionType::get_instruction_cb(opcode);
        self.register1 = Registers::get_register_left_cb(opcode);
        self.register2 = Registers::get_register_right_cb(opcode);
        self.cond_type = InstructionConditions::None;
    }

    // Some Default implementation of some functions
    // I did this in order to avoid code duplication
    // plus this makes it easiter if I want to change how an instruction works internally
    pub fn add_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let half_carry = ((val_1 & 0xf) + (val_2 & 0xf)) & 0x10 == 0x10;
        let (value, carry) = val_1.overflowing_add(val_2);

        let flags = [
            Flags::HalfCarry(half_carry),
            Flags::Zero(value == 0),
            Flags::Subtraction(false),
            Flags::Carry(carry),
        ];
        registers.set_flags(&flags);
        value
    }

    pub fn adc_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let prev_carry_flag = registers.get_flag(Flags::Carry(true));
        let mut result = Self::add_8bit_base(registers, val_1, val_2);
        let prev_half_carry_flag = registers.get_flag(Flags::HalfCarry(true));
        if prev_carry_flag {
            result = Self::add_8bit_base(registers, result, 1);
        }
        let current_half_carry_flag = registers.get_flag(Flags::HalfCarry(true));
        let current_carry_flag = registers.get_flag(Flags::Carry(true));
        let flags = [
            Flags::Carry(prev_half_carry_flag & current_carry_flag),
            Flags::HalfCarry(prev_half_carry_flag | current_half_carry_flag),
        ];
        registers.set_flags(&flags);
        result
    }

    pub fn sub_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let half_carry = (((val_1 as i16) & 0xf) - ((val_2 as i16) & 0xf)) & 0x10 == 0x10;
        let (value, carry) = val_1.overflowing_sub(val_2);
        let flags = [
            Flags::HalfCarry(half_carry),
            Flags::Zero(value == 0),
            Flags::Subtraction(true),
            Flags::Carry(carry),
        ];
        registers.set_flags(&flags);
        value
    }

    pub fn sbc_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let prev_carry_flag = registers.get_flag(Flags::Carry(true));
        let mut result = Instruction::sub_8bit_base(registers, val_1, val_2);
        let prev_half_carry_flag = registers.get_flag(Flags::HalfCarry(true));

        if prev_carry_flag {
            result = Instruction::sub_8bit_base(registers, result, 1);
        }
        let current_half_carry_flag = registers.get_flag(Flags::HalfCarry(true));
        let current_carry_flag = registers.get_flag(Flags::Carry(true));
        let flags = [
            Flags::Carry(prev_half_carry_flag & current_carry_flag),
            Flags::HalfCarry(prev_half_carry_flag | current_half_carry_flag),
        ];
        registers.set_flags(&flags);
        result
    }

    pub fn and_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let result = val_1 & val_2;
        let flags = [
            Flags::Carry(false),
            Flags::HalfCarry(true),
            Flags::Subtraction(false),
            Flags::Zero(result == 0),
        ];
        registers.set_flags(&flags);
        result
    }

    pub fn or_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let result = val_1 | val_2;
        let flags = [
            Flags::Carry(false),
            Flags::HalfCarry(false),
            Flags::Subtraction(false),
            Flags::Zero(result == 0),
        ];
        registers.set_flags(&flags);
        result
    }

    pub fn xor_8bit_base(registers: &mut CpuRegisters, val_1: u8, val_2: u8) -> u8 {
        let result = val_1 ^ val_2;
        let flags = [
            Flags::Carry(false),
            Flags::HalfCarry(false),
            Flags::Subtraction(false),
            Flags::Zero(result == 0),
        ];
        registers.set_flags(&flags);
        result
    }
    pub fn rlc_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = value << 1 | value >> 7;
        let flags = [
            Flags::Carry(get_bit(value, 7) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }

    pub fn rl_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = value << 1
            | if registers.get_flag(Flags::Carry(true)) {
                1
            } else {
                0
            };
        let flags = [
            Flags::Carry(get_bit(value, 7) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }

    pub fn rrc_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = (value & 1) << 7 | value >> 1;
        let flags = [
            Flags::Carry(get_bit(value, 0) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }
    pub fn rr_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = if registers.get_flag(Flags::Carry(true)) {
            1
        } else {
            0
        } | value >> 7;
        let flags = [
            Flags::Carry(get_bit(value, 0) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }

    pub fn sla_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = value << 1;
        let flags = [
            Flags::Carry(get_bit(value, 7) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }

    pub fn sra_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = get_bit(value, 7) << 7 | value >> 1;
        let flags = [
            Flags::Carry(get_bit(value, 0) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }
    pub fn srl_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = value >> 1;
        let flags = [
            Flags::Carry(get_bit(value, 0) == 1),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }
    pub fn swap_8bit_base(registers: &mut CpuRegisters, value: u8) -> u8 {
        let res = (value & 0xF) << 4 | (value & 0xF0) >> 4;
        let flags = [
            Flags::Carry(false),
            Flags::Zero(res == 0),
            Flags::Subtraction(false),
            Flags::HalfCarry(false),
        ];
        registers.set_flags(&flags);
        res
    }

    pub fn bit_8bit_base(registers: &mut CpuRegisters, value: u8, bit: u8) -> u8 {
        let res = get_bit(value, bit) ^ 0b1;
        let flags = [
            Flags::Zero(res == 1),
            Flags::Subtraction(false),
            Flags::HalfCarry(true),
        ];
        registers.set_flags(&flags);
        res
    }
}
