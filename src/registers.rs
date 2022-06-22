use phf::phf_map;

#[derive(Debug, PartialEq)]
pub struct Register {
    pub name: String,
    pub size: i8,
}

// Register LUT
const REGISTERS: phf::Map<&str, i8> = phf_map! {
    // 64-Bit registers
    "RAX" => 8,
    "RBX" => 8,
    "RCX" => 8,
    "RDX" => 8,
    "RSI" => 8,
    "RDI" => 8,
    "RSP" => 8,
    "RBP" => 8,
    "R8" => 8,
    "R9" => 8,
    "R10" => 8,
    "R11" => 8,
    "R12" => 8,
    "R13" => 8,
    "R14" => 8,
    "R15" => 8,

    // 32-Bit registers
    "EAX" => 4,
    "EBX" => 4,
    "ECX" => 4,
    "EDX" => 4,
    "ESI" => 4,
    "EDI" => 4,
    "ESP" => 4,
    "EBP" => 4,
    "R4D" => 4,
    "R9D" => 4,
    "R10D" => 4,
    "R11D" => 4,
    "R12D" => 4,
    "R13D" => 4,
    "R14D" => 4,
    "R15D" => 4,

    // 16-bit registers
    "AX" => 2,
    "BX" => 2,
    "CX" => 2,
    "DX" => 2,
    "SI" => 2,
    "DI" => 2,
    "SP" => 2,
    "BP" => 2,
    "R8W" => 2,
    "R9W" => 2,
    "R10W" => 2,
    "R11W" => 2,
    "R12W" => 2,
    "R13W" => 2,
    "R14W" => 2,
    "R15W" => 2,

    // 8-bit registers
    "AH" => 1,
    "AL" => 1,
    "BH" => 1,
    "BL" => 1,
    "CH" => 1,
    "CL" => 1,
    "DH" => 1,
    "DL" => 1,
    "SIL" => 1,
    "DIL" => 1,
    "SPL" => 1,
    "BPL" => 1,
    "R8B" => 1,
    "R9B" => 1,
    "R10B" => 1,
    "R11B" => 1,
    "R12B" => 1,
    "R13B" => 1,
    "R14B" => 1,
    "R15B" => 1,
};

impl Register {
    pub fn parse(name: String) -> Result<Register, String> {
        let normalized_name: String = name.to_uppercase();

        let lookup = REGISTERS.get(normalized_name.as_str());

        match lookup {
            Some(size) => Ok(Register {
                name: normalized_name,
                size: size.clone(),
            }),
            None => Err(format!("invalid register {}", name).to_string()),
        }
    }
}

#[cfg(test)]
mod register_parse_test {
    use crate::registers::Register;

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
                size: 8
            })
        );
    }

    #[test]
    fn ignore_case() {
        assert_eq!(
            Register::parse("rAx".to_string()),
            Ok(Register {
                name: "RAX".to_string(),
                size: 8
            })
        );
        assert_eq!(
            Register::parse("eAx".to_string()),
            Ok(Register {
                name: "EAX".to_string(),
                size: 4
            })
        );
        assert_eq!(
            Register::parse("Ax".to_string()),
            Ok(Register {
                name: "AX".to_string(),
                size: 2
            })
        );
    }
}
