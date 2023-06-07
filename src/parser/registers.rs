use phf::phf_map;

#[derive(Debug, PartialEq, Clone)]
pub struct Register {
    pub name: String,
    pub size: u8,
    pub part_of: GPRegister,
}

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum GPRegister {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,

    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

// Register LUT
const REGISTERS: phf::Map<&str, (u8, GPRegister)> = phf_map! {
    // 64-Bit registers
    "RAX" => (8,GPRegister::RAX),
    "RBX" => (8,GPRegister::RBX),
    "RCX" => (8,GPRegister::RCX),
    "RDX" => (8,GPRegister::RDX),
    "RSI" => (8,GPRegister::RSI),
    "RDI" => (8,GPRegister::RDI),
    "RSP" => (8,GPRegister::RSP),
    "RBP" => (8,GPRegister::RBP),
    "R8"  => (8,GPRegister::R8),
    "R9"  => (8,GPRegister::R9),
    "R10" => (8,GPRegister::R10),
    "R11" => (8,GPRegister::R11),
    "R12" => (8,GPRegister::R12),
    "R13" => (8,GPRegister::R13),
    "R14" => (8,GPRegister::R14),
    "R15" => (8,GPRegister::R15),

    // 32-Bit registers
    "EAX" => (4,GPRegister::RAX),
    "EBX" => (4,GPRegister::RBX),
    "ECX" => (4,GPRegister::RCX),
    "EDX" => (4,GPRegister::RDX),
    "ESI" => (4,GPRegister::RSI),
    "EDI" => (4,GPRegister::RDI),
    "ESP" => (4,GPRegister::RSP),
    "EBP" => (4,GPRegister::RBP),
    "R8D" => (4,GPRegister::R8),
    "R9D" => (4,GPRegister::R9),
    "R10D" => (4,GPRegister::R10),
    "R11D" => (4,GPRegister::R11),
    "R12D" => (4,GPRegister::R12),
    "R13D" => (4,GPRegister::R13),
    "R14D" => (4,GPRegister::R14),
    "R15D" => (4,GPRegister::R15),

    // 16-bit registers
    "AX" => (2, GPRegister::RAX),
    "BX" => (2, GPRegister::RBX),
    "CX" => (2, GPRegister::RCX),
    "DX" => (2, GPRegister::RDX),
    "SI" => (2, GPRegister::RSI),
    "DI" => (2, GPRegister::RDI),
    "SP" => (2, GPRegister::RSP),
    "BP" => (2, GPRegister::RBP),
    "R8W" => (2, GPRegister::R8),
    "R9W" => (2, GPRegister::R9),
    "R10W" => (2, GPRegister::R10),
    "R11W" => (2, GPRegister::R11),
    "R12W" => (2, GPRegister::R12),
    "R13W" => (2, GPRegister::R13),
    "R14W" => (2, GPRegister::R14),
    "R15W" => (2, GPRegister::R15),

    // 8-bit registers
    "AH"   => (1, GPRegister::RAX),
    "AL"   => (1, GPRegister::RAX),
    "BH"   => (1, GPRegister::RBX),
    "BL"   => (1, GPRegister::RBX),
    "CH"   => (1, GPRegister::RCX),
    "CL"   => (1, GPRegister::RCX),
    "DH"   => (1, GPRegister::RDX),
    "DL"   => (1, GPRegister::RDX),
    "SIL"  => (1, GPRegister::RDI),
    "DIL"  => (1, GPRegister::RDI),
    "SPL"  => (1, GPRegister::RSP),
    "BPL"  => (1, GPRegister::RBP),
    "R8B"  => (1, GPRegister::R8),
    "R9B"  => (1, GPRegister::R9),
    "R10B" => (1, GPRegister::R10),
    "R11B" => (1, GPRegister::R11),
    "R12B" => (1, GPRegister::R12),
    "R13B" => (1, GPRegister::R13),
    "R14B" => (1, GPRegister::R14),
    "R15B" => (1, GPRegister::R15),
};

impl Register {
    pub fn parse(name: String) -> Result<Register, String> {
        let normalized_name: String = name.to_uppercase();

        let lookup = REGISTERS.get_entry(normalized_name.as_str());

        match lookup {
            Some((reg, (size, gpreg))) => Ok(Register {
                name: reg.to_string(),
                size: size.clone(),
                part_of: gpreg.clone(),
            }),
            None => Err(format!("invalid register {}", name).to_string()),
        }
    }
}

#[cfg(test)]
mod register_parse_test {
    use crate::parser::registers::{GPRegister, Register};

    #[test]
    fn invalid_registers() {
        assert_eq!(
            Register::parse("nope".to_string()),
            Err("invalid register nope".to_string())
        );
    }

    #[test]
    fn valid_registers() {
        assert_eq!(
            Register::parse("rax".to_string()),
            Ok(Register {
                name: "RAX".to_string(),
                size: 8,
                part_of: GPRegister::RAX,
            })
        );
    }

    #[test]
    fn ignore_case() {
        assert_eq!(
            Register::parse("rAx".to_string()),
            Ok(Register {
                name: "RAX".to_string(),
                size: 8,
                part_of: GPRegister::RAX,
            })
        );
        assert_eq!(
            Register::parse("eAx".to_string()),
            Ok(Register {
                name: "EAX".to_string(),
                size: 4,
                part_of: GPRegister::RAX,
            })
        );
        assert_eq!(
            Register::parse("Ax".to_string()),
            Ok(Register {
                name: "AX".to_string(),
                size: 2,
                part_of: GPRegister::RAX,
            })
        );
    }
}
