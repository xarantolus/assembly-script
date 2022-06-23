use crate::registers::Register;
use unescape::unescape;

#[derive(PartialEq, Debug)]
pub enum Instruction {
    MOVimm { destination: Register, operand: i64 },

    PUSHreg { destination: Register },
    POPreg { destination: Register },

    ADDimm { destination: Register, operand: i64 },
}

fn parse_instruction(line: &str) -> Result<Instruction, String> {
    // Split e.g. `mov rax, 0` into `"mov", "rax", "0"`
    let split: Vec<&str> = line
        .split([' ', ',', '\t'])
        .filter(|s| !s.is_empty())
        .collect();

    let mem = split[0].to_uppercase();

    match (mem.as_str(), split.len() - 1) {
        ("MOV", 2) => Ok(Instruction::MOVimm {
            destination: Register::parse(split[1].to_string())?,
            operand: parse_immediate_arg(split[2])?,
        }),
        ("PUSH", 1) => {
            let src = Register::parse(split[1].to_string())?;
            if src.size == 1 {
                Err(format!("PUSH not supported for register {} of size 1", src.name).to_string())
            } else {
                Ok(Instruction::PUSHreg { destination: src })
            }
        }
        ("POP", 1) => {
            let dest = Register::parse(split[1].to_string())?;
            if dest.size == 1 {
                Err(format!("POP not supported for register {} of size 1", dest.name).to_string())
            } else {
                Ok(Instruction::POPreg { destination: dest })
            }
        }
        ("ADD", 2) => Ok(Instruction::ADDimm {
            destination: Register::parse(split[1].to_string())?,
            operand: parse_immediate_arg(split[2])?,
        }),
        _ => Err(format!("Cannot parse instruction {}", line).to_string()),
    }
}

#[cfg(test)]
mod instruction_parse_test {
    use crate::{
        instructions::{parse_instruction, Instruction},
        registers::Register,
    };

    #[test]
    fn mov_immediate() {
        assert_eq!(
            parse_instruction("mov rax, 53"),
            Ok(Instruction::MOVimm {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8
                },
                operand: 53,
            })
        );
        assert_eq!(
            parse_instruction("mov  eAx, 53"),
            Ok(Instruction::MOVimm {
                destination: Register {
                    name: "EAX".to_string(),
                    size: 4
                },
                operand: 53,
            })
        );
        assert_eq!(
            parse_instruction("mov  al, 'z'"),
            Ok(Instruction::MOVimm {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 122,
            })
        );
        assert_eq!(
            parse_instruction(r#"mov  al, '\n'"#),
            Ok(Instruction::MOVimm {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 10,
            })
        );
        assert_eq!(
            parse_instruction(r#"mov  al, '\t'"#),
            Ok(Instruction::MOVimm {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 9,
            })
        );
    }

    #[test]
    fn push_register() {
        assert_eq!(
            parse_instruction("push rbx"),
            Ok(Instruction::PUSHreg {
                destination: Register {
                    name: "RBX".to_string(),
                    size: 8
                },
            })
        );
        assert_eq!(
            parse_instruction("push r8d"),
            Ok(Instruction::PUSHreg {
                destination: Register {
                    name: "R8D".to_string(),
                    size: 4
                },
            })
        );
        assert_eq!(
            parse_instruction("push sp"),
            Ok(Instruction::PUSHreg {
                destination: Register {
                    name: "SP".to_string(),
                    size: 2
                },
            })
        );
        assert_eq!(
            parse_instruction("push cl"),
            Err("PUSH not supported for register CL of size 1".to_string())
        );
    }

    #[test]
    fn pop_register() {
        assert_eq!(
            parse_instruction("pop rbx"),
            Ok(Instruction::POPreg {
                destination: Register {
                    name: "RBX".to_string(),
                    size: 8
                },
            })
        );
        assert_eq!(
            parse_instruction("pop r8d"),
            Ok(Instruction::POPreg {
                destination: Register {
                    name: "R8D".to_string(),
                    size: 4
                },
            })
        );
        assert_eq!(
            parse_instruction("pop sp"),
            Ok(Instruction::POPreg {
                destination: Register {
                    name: "SP".to_string(),
                    size: 2
                },
            })
        );
        assert_eq!(
            parse_instruction("pop cl"),
            Err("POP not supported for register CL of size 1".to_string())
        );
    }

    #[test]
    fn add_immediate() {
        assert_eq!(
            parse_instruction("add rax, 53"),
            Ok(Instruction::ADDimm {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8
                },
                operand: 53,
            })
        );
        assert_eq!(
            parse_instruction("add  eAx, 53"),
            Ok(Instruction::ADDimm {
                destination: Register {
                    name: "EAX".to_string(),
                    size: 4
                },
                operand: 53,
            })
        );
        assert_eq!(
            parse_instruction("AdD  al, 'z'"),
            Ok(Instruction::ADDimm {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 122,
            })
        );
        assert_eq!(
            parse_instruction(r#"add  al, '\n'"#),
            Ok(Instruction::ADDimm {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 10,
            })
        );
        assert_eq!(
            parse_instruction(r#"add  al, '\t'"#),
            Ok(Instruction::ADDimm {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 9,
            })
        );
    }
}

fn parse_immediate_arg(nstr: &str) -> Result<i64, String> {
    match i64::from_str_radix(nstr, 10) {
        Ok(i) => Ok(i),
        _ => parse_as_char_constant(nstr),
    }
}

fn parse_as_char_constant(nstr: &str) -> Result<i64, String> {
    match unescape(nstr) {
        Some(s) => {
            let chars: Vec<char> = s.chars().collect();
            match s.len() {
                1 => Ok(chars[0] as i64),
                3 => {
                    if chars[0] == '\'' && chars[0] == chars[2] {
                        Ok(chars[1] as i64)
                    } else {
                        Err(format!("invalid quotes in string {}, expected \"'\"", nstr))
                    }
                }
                _ => Err(format!(
                    "invalid string length {} for {} (from {})",
                    s.len(),
                    s,
                    nstr
                )),
            }
        }
        None => Err(format!("char unescape not possible for {nstr}")),
    }
}
