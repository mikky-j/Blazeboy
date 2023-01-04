use std::fmt::Display;
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum InstructionType {
    None,
    // Special Instructions
    Nop,
    Halt,
    Stop,
    Di,
    Ei,
    Cb,

    // Load, Store Move, instructions
    Ld,
    Ldh,
    Pop,
    Push,

    // Control Instructions
    Jp,
    Jr,
    Call,
    Rst,
    Ret,
    Reti,

    // Arithmetic Instructions
    Inc,
    Dec,
    And,
    Or,
    Xor,
    Add,
    Sub,
    Adc,
    Sbc,
    Cp,
    Cpl,
    Ccf,
    Daa,
    Scf,

    // Bit rotations
    Rlca,
    Rla,
    Rrca,
    Rra,

    // CB Instructions
    Rlc,
    Rrc,
    Rl,
    Rr,
    Sla,
    Sra,
    Swap,
    Srl,
    Bit,
    Set,
    Res,
}

impl Display for InstructionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstructionType::*;
        let value = match *self {
            None => "None",
            Nop => "Nop",
            Halt => "Halt",
            Stop => "Stop",
            Di => "Di",
            Ei => "Ei",
            Cb => "Cb",
            Ld => "Ld",
            Ldh => "Ldh",
            Pop => "Pop",
            Push => "Push",
            Jp => "Jp",
            Jr => "Jr",
            Call => "Call",
            Rst => "Rst",
            Ret => "Ret",
            Reti => "Reti",
            Inc => "Inc",
            Dec => "Dec",
            And => "And",
            Or => "Or",
            Xor => "Xor",
            Add => "Add",
            Sub => "Sub",
            Adc => "Adc",
            Sbc => "Sbc",
            Cp => "Cp",
            Cpl => "Cpl",
            Ccf => "Ccf",
            Daa => "Daa",
            Scf => "Scf",
            Rlca => "Rlca",
            Rla => "Rla",
            Rrca => "Rrca",
            Rra => "Rra",
            Rlc => "Rlc",
            Rrc => "Rrc",
            Rl => "Rl",
            Rr => "Rr",
            Sla => "Sla",
            Sra => "Sra",
            Swap => "Swap",
            Srl => "Srl",
            Bit => "Bit",
            Set => "Set",
            Res => "Res",
        };
        value.fmt(f)
    }
}

impl InstructionType {
    pub fn get_instruction(opcode: u8) -> Self {
        use InstructionType::*;
        match opcode {
            0x00 => Nop,
            0x01..=0x02
            | 0x06
            | 0x08
            | 0x0a
            | 0x0e
            | 0x11..=0x12
            | 0x16
            | 0x1a
            | 0x1e
            | 0x21..=0x22
            | 0x26
            | 0x2a
            | 0x2e
            | 0x31..=0x32
            | 0x36
            | 0x3a
            | 0x3e
            | 0x40..=0x75
            | 0x77..=0x7f
            | 0xe2
            | 0xea
            | 0xf2
            | 0xf8..=0xfa => Ld,
            0x03..=0x04 | 0x0c | 0x13..=0x14 | 0x1c | 0x23..=0x24 | 0x2c | 0x33..=0x34 | 0x3c => {
                Inc
            }
            0x05 | 0x0b | 0x0d | 0x15 | 0x1b | 0x1d | 0x25 | 0x2b | 0x2d | 0x35 | 0x3b | 0x3d => {
                Dec
            }
            0x07 => Rlca,
            0x09 | 0x19 | 0x29 | 0x39 | 0x80..=0x87 | 0xc6 | 0xe8 => Add,
            0x0f => Rrca,
            0x10 => Stop,
            0x17 => Rla,
            0x18 | 0x20 | 0x28 | 0x30 | 0x38 => Jr,
            0x1f => Rra,
            0x27 => Daa,
            0x2f => Cpl,
            0x37 => Scf,
            0x3f => Ccf,
            0x76 => Halt,
            0x88..=0x8f | 0xce => Adc,
            0x90..=0x97 | 0xd6 => Sub,
            0x98..=0x9f | 0xde => Sbc,
            0xa0..=0xa7 | 0xe6 => And,
            0xa8..=0xaf | 0xee => Xor,
            0xb0..=0xb7 | 0xf6 => Or,
            0xb8..=0xbf | 0xfe => Cp,
            0xc0 | 0xc8..=0xc9 | 0xd0 | 0xd8 => Ret,
            0xc1 | 0xd1 | 0xe1 | 0xf1 => Pop,
            0xc2..=0xc3 | 0xca | 0xd2 | 0xda | 0xe9 => Jp,
            0xc4 | 0xcc..=0xcd | 0xd4 | 0xdc => Call,
            0xc5 | 0xd5 | 0xe5 | 0xf5 => Push,
            0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff => Rst,
            0xcb => Cb,
            0xd3 => Self::None,
            0xd9 => Reti,
            0xdb => Self::None,
            0xdd => Self::None,
            0xe0 | 0xf0 => Ldh,
            0xe3 => Self::None,
            0xe4 => Self::None,
            0xeb => Self::None,
            0xec => Self::None,
            0xed => Self::None,
            0xf3 => Di,
            0xf4 => Self::None,
            0xfb => Ei,
            0xfc => Self::None,
            0xfd => Self::None,
        }
    }

    pub fn get_instruction_cb(opcode: u8) -> Self {
        use InstructionType::*;
        match opcode {
            0x00..=0x07 => Rlc,
            0x08..=0x0f => Rrc,
            0x10..=0x17 => Rl,
            0x18..=0x1f => Rr,
            0x20..=0x27 => Sla,
            0x28..=0x2f => Sra,
            0x30..=0x37 => Swap,
            0x38..=0x3f => Srl,
            0x40..=0x7f => Bit,
            0x80..=0xbf => Res,
            0xc0..=0xff => Set,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub enum AddressingMode {
    Impl,
    Reg_d16,
    Memreg_Reg,
    Reg,
    Reg_d8,
    a16_Reg,
    Reg_Reg,
    Reg_Memreg,
    d8,
    r8,
    HLI_Reg,
    Reg_HLI,
    HLD_Reg,
    Memreg,
    Memreg_d8,
    Reg_HLD,
    a16,
    a8_Reg,
    Reg_r8,
    Reg_a8,
    Reg_Reg_r8,
    Reg_a16,
}

impl Display for AddressingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AddressingMode::*;

        let value = match *self {
            Impl => "Impl",
            Reg_d16 => "Reg_d16",
            Memreg_Reg => "Memreg_Reg",
            Reg => "Reg",
            Reg_d8 => "Reg_d8",
            a16_Reg => "a16_Reg",
            Reg_Reg => "Reg_Reg",
            Reg_Memreg => "Reg_Memreg",
            d8 => "d8",
            r8 => "r8",
            HLI_Reg => "HLI_Reg",
            Reg_HLI => "Reg_HLI",
            HLD_Reg => "HLD_Reg",
            Memreg => "Memreg",
            Memreg_d8 => "Memreg_d8",
            Reg_HLD => "Reg_HLD",
            a16 => "a16",
            a8_Reg => "a8_Reg",
            Reg_r8 => "Reg_r8",
            Reg_a8 => "Reg_a8",
            Reg_Reg_r8 => "Reg_HLI_r8",
            Reg_a16 => "Reg_a16",
        };
        write!(f, "{}", value)
    }
}

impl AddressingMode {
    pub fn get_addressing_mode(opcode: u8) -> Self {
        use AddressingMode::*;
        match opcode {
            0x01 | 0x11 | 0x21 | 0x31 => Reg_d16,
            0x02 | 0x12 | 0x70..=0x75 | 0x77 | 0xe2 => Memreg_Reg,
            0x03..=0x05
            | 0x0b..=0x0d
            | 0x13..=0x15
            | 0x1b..=0x1d
            | 0x23..=0x25
            | 0x2b..=0x2d
            | 0x33
            | 0x3b..=0x3d
            | 0x90..=0x95
            | 0x97
            | 0xa0..=0xa5
            | 0xa7..=0xad
            | 0xaf..=0xb5
            | 0xb7..=0xbd
            | 0xbf
            | 0xc1
            | 0xc5
            | 0xd1
            | 0xd5
            | 0xe1
            | 0xe5
            | 0xf1
            | 0xf5 => Reg,
            0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e | 0xc6 | 0xce | 0xde => Reg_d8,
            0x08 | 0xea => a16_Reg,
            0x09
            | 0x19
            | 0x29
            | 0x39
            | 0x40..=0x45
            | 0x47..=0x4d
            | 0x4f..=0x55
            | 0x57..=0x5d
            | 0x5f..=0x65
            | 0x67..=0x6d
            | 0x6f
            | 0x78..=0x7d
            | 0x7f..=0x85
            | 0x87..=0x8d
            | 0x8f
            | 0x98..=0x9d
            | 0x9f
            | 0xf9 => Reg_Reg,
            0x0a | 0x1a | 0x46 | 0x4e | 0x56 | 0x5e | 0x66 | 0x6e | 0x7e | 0x86 | 0x8e | 0x9e
            | 0xf2 => Reg_Memreg,
            0x10 | 0xd6 | 0xe6 | 0xee | 0xf6 | 0xfe => d8,
            0x18 | 0x20 | 0x28 | 0x30 | 0x38 => r8,
            0x22 => HLI_Reg,
            0x2a => Reg_HLI,
            0x32 => HLD_Reg,
            0x34..=0x35 | 0x96 | 0xa6 | 0xae | 0xb6 | 0xbe => Memreg,
            0x36 => Memreg_d8,
            0x3a => Reg_HLD,
            0xc2..=0xc4 | 0xca | 0xcc..=0xcd | 0xd2 | 0xd4 | 0xda | 0xdc => a16,
            0xe0 => a8_Reg,
            0xe8 => Reg_r8,
            0xf0 => Reg_a8,
            0xf8 => Reg_Reg_r8,
            0xfa => Reg_a16,
            _ => Impl,
        }
    }

    pub fn get_addressing_mode_cb(opcode: u8) -> Self {
        use AddressingMode::*;
        let (row, col) = (opcode >> 4, opcode & 0xf);

        match (row, col) {
            (0x0..=0xf, 0x0..=0x5 | 0x7..=0xd | 0xf) => Reg,
            (0x0..=0xf, 0x0..=0x6 | 0xe) => Memreg,
            _ => Impl,
        }
    }
}
