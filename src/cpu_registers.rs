use std::fmt::Display;

use crate::{construct_16bit, get_bit, instruction::InstructionConditions, set_bit, split_16bit};

pub struct CpuRegisters {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    pc: u16,
    sp: u16,
}

impl CpuRegisters {
    pub fn new() -> Self {
        CpuRegisters {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0,
            pc: 0x100,
            sp: 0xff00,
        }
    }
    pub fn is_16bit_reg(register: Registers) -> bool {
        use Registers::*;
        let result: bool = match register {
            AF | BC | DE | HL | PC | SP => true,
            None => panic!("Register was not given"),
            _ => false,
        };
        result
    }
    pub fn get_16bit(&self, register: Registers) -> u16 {
        use Registers::*;
        let result: u16 = match register {
            AF => construct_16bit(self.a, self.f),
            BC => construct_16bit(self.b, self.c),
            DE => construct_16bit(self.d, self.e),
            HL => construct_16bit(self.h, self.l),
            PC => self.pc,
            SP => self.sp,
            None => panic!("Register was not given"),
            _ => panic!("Tried to read the 16 bit address of an 8 bit register"),
        };
        result
    }

    pub fn get_8bit(&self, register: Registers) -> u8 {
        use Registers::*;
        let result: u8 = match register {
            A => self.a,
            B => self.b,
            C => self.c,
            D => self.d,
            E => self.e,
            F => self.f,
            H => self.h,
            L => self.l,
            None => panic!("Register was not given"),
            _ => panic!("Tried to read the 8 bit value of a 16 bit register"),
        };
        result
    }

    pub fn set_8bit(&mut self, register: Registers, value: u8) {
        use Registers::*;
        match register {
            A => self.a = value,
            B => self.b = value,
            C => self.c = value,
            D => self.d = value,
            E => self.e = value,
            F => self.f = value,
            H => self.h = value,
            L => self.l = value,
            None => panic!("Register was not given"),
            _ => panic!("Tried to read the 16 bit address of an 8 bit register"),
        }
    }

    pub fn set_16bit(&mut self, register: Registers, value: u16) {
        use Registers::*;
        match register {
            AF => (self.a, self.f) = split_16bit(value),
            BC => (self.b, self.c) = split_16bit(value),
            DE => (self.d, self.e) = split_16bit(value),
            HL => (self.h, self.l) = split_16bit(value),
            PC => self.pc = value,
            SP => self.sp = value,
            None => panic!("Register was not given"),
            _ => panic!("Tried to read the 16 bit address of an 8 bit register"),
        }
    }

    pub fn set_flags(&mut self, flags: &[Flags]) {
        for flag in flags {
            match *flag {
                Flags::Carry(x) => self.f = set_bit(self.f, 4, x),
                Flags::HalfCarry(x) => self.f = set_bit(self.f, 5, x),
                Flags::Subtraction(x) => self.f = set_bit(self.f, 6, x),
                Flags::Zero(x) => self.f = set_bit(self.f, 7, x),
            }
        }
    }

    pub fn check_condition(&self, condition: InstructionConditions) -> bool {
        use InstructionConditions::*;
        match condition {
            C => get_bit(self.f, 4) == 1,
            Z => get_bit(self.f, 7) == 1,
            NC => get_bit(self.f, 4) == 0,
            NZ => get_bit(self.f, 7) == 0,
            None => true,
        }
    }

    pub fn get_flag(&self, flag: Flags) -> bool {
        use Flags::*;
        match flag {
            Zero(_) => get_bit(self.f, 7) == 1,
            Subtraction(_) => get_bit(self.f, 6) == 1,
            HalfCarry(_) => get_bit(self.f, 5) == 1,
            Carry(_) => get_bit(self.f, 4) == 1,
        }
    }
}

pub enum Flags {
    HalfCarry(bool),
    Carry(bool),
    Subtraction(bool),
    Zero(bool),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Registers {
    // 8 bit instructions
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,

    // 16 bit instructions
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
    None,
}

impl Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Registers::*;
        let value = match self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            F => "F",
            H => "H",
            L => "L",
            AF => "AF",
            BC => "BC",
            DE => "DE",
            HL => "HL",
            SP => "SP",
            PC => "PC",
            None => "None",
        };
    write!(f, "{}", value)
    }
}

impl Registers {
    pub fn get_instuctions_left(opcode: u8) -> Self {
        use Registers::*;
        match opcode {
            0x01..=0x03 | 0x0b | 0xc1 | 0xc5 => BC,
            0x04..=0x06 | 0x40..=0x47 | 0x90 | 0xa0 | 0xa8 | 0xb0 | 0xb8 => B,
            0x09
            | 0x19
            | 0x21..=0x23
            | 0x29
            | 0x2b
            | 0x32
            | 0x34..=0x36
            | 0x39
            | 0x70..=0x75
            | 0x77
            | 0x96
            | 0xa6
            | 0xae
            | 0xb6
            | 0xbe
            | 0xe1
            | 0xe5
            | 0xf8 => HL,
            0x0a
            | 0x1a
            | 0x2a
            | 0x3a
            | 0x3c..=0x3e
            | 0x78..=0x8f
            | 0x97..=0x9f
            | 0xa7
            | 0xaf
            | 0xb7
            | 0xbf
            | 0xc6
            | 0xce
            | 0xde
            | 0xf0
            | 0xf2
            | 0xfa => A,
            0x0c..=0x0e | 0x48..=0x4f | 0x91 | 0xa1 | 0xa9 | 0xb1 | 0xb9 | 0xe2 => C,
            0x11..=0x13 | 0x1b | 0xd1 | 0xd5 => DE,
            0x14..=0x16 | 0x50..=0x57 | 0x92 | 0xa2 | 0xaa | 0xb2 | 0xba => D,
            0x1c..=0x1e | 0x58..=0x5f | 0x93 | 0xa3 | 0xab | 0xb3 | 0xbb => E,
            0x24..=0x26 | 0x60..=0x67 | 0x94 | 0xa4 | 0xac | 0xb4 | 0xbc => H,
            0x2c..=0x2e | 0x68..=0x6f | 0x95 | 0xa5 | 0xad | 0xb5 | 0xbd => L,
            0x31 | 0x33 | 0x3b | 0xe8 | 0xf9 => SP,
            0xf1 | 0xf5 => AF,
            _ => None,
        }
    }

    pub fn get_instruction_right(opcode: u8) -> Self {
        use Registers::*;
        match opcode {
            0x02 | 0x12 | 0x22 | 0x32 | 0x47 | 0x4f | 0x57 | 0x5f | 0x67 | 0x6f | 0x77 | 0x7f
            | 0x87 | 0x8f | 0x9f | 0xe0 | 0xe2 | 0xea => A,
            0x08 | 0x39 | 0xf8 => SP,
            0x09..=0x0a => BC,
            0x19..=0x1a => DE,
            0x29..=0x2a
            | 0x3a
            | 0x46
            | 0x4e
            | 0x56
            | 0x5e
            | 0x66
            | 0x6e
            | 0x7e
            | 0x86
            | 0x8e
            | 0x9e
            | 0xf9 => HL,
            0x40 | 0x48 | 0x50 | 0x58 | 0x60 | 0x68 | 0x70 | 0x78 | 0x80 | 0x88 | 0x98 => B,
            0x41 | 0x49 | 0x51 | 0x59 | 0x61 | 0x69 | 0x71 | 0x79 | 0x81 | 0x89 | 0x99 | 0xf2 => C,
            0x42 | 0x4a | 0x52 | 0x5a | 0x62 | 0x6a | 0x72 | 0x7a | 0x82 | 0x8a | 0x9a => D,
            0x43 | 0x4b | 0x53 | 0x5b | 0x63 | 0x6b | 0x73 | 0x7b | 0x83 | 0x8b | 0x9b => E,
            0x44 | 0x4c | 0x54 | 0x5c | 0x64 | 0x6c | 0x74 | 0x7c | 0x84 | 0x8c | 0x9c => H,
            0x45 | 0x4d | 0x55 | 0x5d | 0x65 | 0x6d | 0x75 | 0x7d | 0x85 | 0x8d | 0x9d => L,
            _ => None,
        }
    }

    pub fn get_register_left_cb(opcode: u8) -> Self {
        use Registers::*;
        let col = opcode & 0xf;
        match col {
            0x0 | 0x8 => B,
            0x1 | 0x9 => C,
            0x2 | 0xA => D,
            0x3 | 0xB => E,
            0x4 | 0xC => H,
            0x5 | 0xD => L,
            0x6 | 0xE => HL,
            0x7 | 0xF => A,
            _ => None,
        }
    }

    #[allow(unused_variables)]
    pub fn get_register_right_cb(opcode: u8) -> Self {
        Self::None
    }
}
