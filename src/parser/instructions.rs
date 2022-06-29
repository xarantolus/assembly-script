use crate::parser::registers::Register;
use phf::phf_map;
use regex::Regex;
use std::{fmt, vec};
use unescape::unescape;

use lazy_static::lazy_static;
#[derive(PartialEq, Debug, Clone)]
pub enum Instruction {
    MOV {
        destination: ValueOperand,
        source: ValueOperand,
    },

    IMUL {
        destination: Register,
        source: ValueOperand,
    },

    PUSH {
        // The gnu assembler seems to treat `push 1` as pushing a 64-bit value
        source: ValueOperand,
    },
    POP {
        destination: Register,
    },

    ADD {
        destination: Register,
        source: ValueOperand,
    },

    SUB {
        destination: Register,
        source: ValueOperand,
    },

    AND {
        destination: Register,
        source: ValueOperand,
    },

    LEA {
        destination: Register,
        source: ValueOperand,
    },

    XORreg {
        destination: Register,
        source: Register,
    },

    CMP {
        src1: Register,
        src2: ValueOperand,
    },

    NOTreg {
        destination: Register,
    },

    TEST {
        src1: Register,
        src2: ValueOperand,
    },

    JMPlabel {
        target: JumpTarget,
        condition: JumpCondition,
    },

    CALLlabel {
        label: String,
    },
    RET {},

    SYSCALL {},
    // TODO: call label, ret, jmp, jl jg jz, syscall, lea
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
#[derive(PartialEq, Debug, Clone)]
pub enum JumpTarget {
    Relative { forwards: bool, label: String },
    Absolute { label: String },
}

#[derive(PartialEq, Debug, Clone)]
pub enum JumpCondition {
    None,
    ZeroEqual,
    Less,
    Greater,
}

fn split_instr_string(line: &str) -> Vec<&str> {
    // Split the mnemonic from the rest
    let split = line.split_once(&[' ', '\t']);
    match split {
        // Instructions without any whitespace
        None => vec![line],
        Some((left, right)) => {
            let mut res = vec![left];
            let rest_split: Vec<&str> = right.split(",").collect();
            res.extend_from_slice(&rest_split);
            return res
                .into_iter()
                .map(|w| w.trim())
                .filter(|w| !w.is_empty())
                .collect();
        }
    }
}

#[cfg(test)]
mod split_test {
    use super::split_instr_string;

    #[test]
    fn split() {
        assert_eq!(
            split_instr_string("mov rax, rbx"),
            vec!["mov", "rax", "rbx"]
        );
        assert_eq!(
            split_instr_string("mov byte ptr [rip + offset] , al"),
            vec!["mov", "byte ptr [rip + offset]", "al"]
        );
    }
}

pub fn parse_instruction(line: &str) -> Result<Instruction, String> {
    // Split e.g. `mov rax, 0` into `"mov", "rax", "0"`
    // but split `mov byte ptr [rip + offset], 0` into `"mov", "byte ptr [rip + offset]", "0"`

    let split = split_instr_string(line);

    let mnem = split[0].to_uppercase();

    match (mnem.as_str(), split.len() - 1) {
        ("MOV", 2) => match parse_mem_and_value(split.clone()) {
            Ok((mem, val)) => Ok(Instruction::MOV {
                destination: mem,
                source: val,
            }),
            Err(_) => {
                let (dest, src) = parse_2_instruction_args(split)?;
                return Ok(Instruction::MOV {
                    destination: ValueOperand::Register { r: dest },
                    source: src,
                });
            }
        },
        ("PUSH", 1) => match parse_1_instruction_arg(split)? {
            ValueOperand::Register { r } => {
                if r.size == 1 {
                    Err(format!("PUSH not supported for register {} of size 1", r.name).to_string())
                } else {
                    Ok(Instruction::PUSH {
                        source: ValueOperand::Register { r: r },
                    })
                }
            }
            o => Ok(Instruction::PUSH { source: o }),
        },
        ("POP", 1) => {
            let dest = Register::parse(split[1].to_string())?;
            if dest.size == 1 {
                Err(format!("POP not supported for register {} of size 1", dest.name).to_string())
            } else {
                Ok(Instruction::POP { destination: dest })
            }
        }
        ("NOT", 1) => Ok(Instruction::NOTreg {
            destination: Register::parse(split[1].to_string())?,
        }),
        ("JMP", 1) => Ok(Instruction::JMPlabel {
            target: parse_label(split[1].to_string())?,
            condition: JumpCondition::None,
        }),
        ("JZ", 1) | ("JE", 1) => Ok(Instruction::JMPlabel {
            target: parse_label(split[1].to_string())?,
            condition: JumpCondition::ZeroEqual,
        }),
        ("JG", 1) => Ok(Instruction::JMPlabel {
            target: parse_label(split[1].to_string())?,
            condition: JumpCondition::Greater,
        }),
        ("JL", 1) => Ok(Instruction::JMPlabel {
            target: parse_label(split[1].to_string())?,
            condition: JumpCondition::Less,
        }),
        ("XOR", 2) => {
            let reg1 = Register::parse(split[1].to_string())?;
            let reg2 = Register::parse(split[2].to_string())?;
            return Ok(Instruction::XORreg {
                destination: reg1,
                source: reg2,
            });
        }
        ("SYSCALL", 0) => Ok(Instruction::SYSCALL {}),
        ("RET", 0) => Ok(Instruction::RET {}),
        ("CALL", 1) => match Register::parse(split[1].to_string()) {
            Ok(_) => Err(format!("unspported call to register value {}", split[1])),
            Err(_) => Ok(Instruction::CALLlabel {
                label: split[1].to_string(),
            }),
        },
        ("IMUL", 2) => {
            let (reg, reg_or_imm) = parse_2_instruction_args(split)?;
            Ok(Instruction::IMUL {
                destination: reg,
                source: reg_or_imm,
            })
        }
        ("ADD", 2) => {
            let (dest, src) = parse_2_instruction_args(split)?;
            Ok(Instruction::ADD {
                destination: dest,
                source: src,
            })
        }
        ("SUB", 2) => {
            let (dest, src) = parse_2_instruction_args(split)?;
            Ok(Instruction::SUB {
                destination: dest,
                source: src,
            })
        }
        ("AND", 2) => {
            let (dest, src) = parse_2_instruction_args(split)?;
            Ok(Instruction::AND {
                destination: dest,
                source: src,
            })
        }
        ("CMP", 2) => {
            let (src1, src2) = parse_2_instruction_args(split)?;
            Ok(Instruction::CMP {
                src1: src1,
                src2: src2,
            })
        }
        ("TEST", 2) => {
            let (src1, src2) = parse_2_instruction_args(split)?;
            Ok(Instruction::TEST {
                src1: src1,
                src2: src2,
            })
        }
        ("LEA", 2) => {
            let (src1, src2) = parse_2_instruction_args(split)?;
            if !matches!(src2, ValueOperand::Memory { label: _, size: _ }) {
                return Err(format!(
                    "Need constant memory operand for LEA, but got {:?}",
                    src2
                ));
            }
            Ok(Instruction::LEA {
                destination: src1,
                source: src2,
            })
        }
        _ => Err(format!("Cannot parse instruction {}", line).to_string()),
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ValueOperand {
    Register { r: Register },
    Immediate { i: i64 },
    Memory { label: String, size: u8 },
    DynamicMemory { register: Register, size: u8 },
}

pub fn parse_label(label: String) -> Result<JumpTarget, String> {
    let split: Vec<&str> = label.split_ascii_whitespace().collect();
    if split.len() != 1 {
        return Err(format!("Cannot parse label with spaces: {}", label));
    }

    match Register::parse(label.to_string()) {
        Ok(_) => Err("cannot use register as label".to_string()),
        Err(_) => {
            let ends = &['b', 'f'][..];
            if label.ends_with(ends) && label[..label.len() - 1].parse::<i8>().is_ok() {
                return Ok(JumpTarget::Relative {
                    forwards: label.ends_with("f"),
                    label: label[..label.len() - 1].to_string(),
                });
            } else {
                return Ok(JumpTarget::Absolute { label: label });
            }
        }
    }
}

#[cfg(test)]
mod parse_label_test {
    #[test]
    fn parse_relative_labels() {
        assert_eq!(
            super::parse_label("1b".to_string()),
            Ok(super::JumpTarget::Relative {
                forwards: false,
                label: "1".to_string()
            })
        );
        assert_eq!(
            super::parse_label("2f".to_string()),
            Ok(super::JumpTarget::Relative {
                forwards: true,
                label: "2".to_string()
            })
        );
    }

    #[test]
    fn parse_normal_labels() {
        assert_eq!(
            super::parse_label("3".to_string()),
            Ok(super::JumpTarget::Absolute {
                label: "3".to_string()
            })
        );
    }
}

fn parse_mem_and_value(instruction: Vec<&str>) -> Result<(ValueOperand, ValueOperand), String> {
    if instruction.len() != 3 {
        panic!(
            "invalid number of operands: got {}, expected 3",
            instruction.len()
        );
    }
    let mem_operand = parse_as_memory_operand(instruction[1])?;
    let other = parse_any_arg(instruction[2])?;

    return Ok((mem_operand, other));
}

fn parse_2_instruction_args(instruction: Vec<&str>) -> Result<(Register, ValueOperand), String> {
    if instruction.len() != 3 {
        panic!(
            "invalid number of operands: got {}, expected 3",
            instruction.len()
        );
    }
    match Register::parse(instruction[1].to_string()) {
        Ok(reg1) => {
            match Register::parse(instruction[2].to_string()) {
                Ok(reg2) => {
                    if reg1.size == reg2.size {
                        Ok((reg1, ValueOperand::Register { r: reg2 }))
                    } else {
                        Err(format!("invalid use of registers {} and {} of different size in {} instruction", reg1.name, reg2.name,                         instruction[0].to_uppercase()).to_string())
                    }
                }
                _ => {
                    let mem_operand = parse_as_memory_operand(instruction[2]);
                    match mem_operand {
                        Err(_) => Ok((
                            // TODO: Check if immediate is too large for destination
                            reg1,
                            ValueOperand::Immediate {
                                i: parse_immediate_arg(instruction[2])?,
                            },
                        )),
                        Ok(mem) => Ok((reg1, mem)),
                    }
                }
            }
        }
        _ => Err(format!(
            "invalid first operand {} for {} instruction",
            instruction[1],
            instruction[0].to_uppercase()
        )),
    }
}

fn parse_1_instruction_arg(instruction: Vec<&str>) -> Result<ValueOperand, String> {
    if instruction.len() != 2 {
        panic!(
            "invalid number of operands: got {}, expected 2",
            instruction.len()
        );
    } else {
        match Register::parse(instruction[1].to_string()) {
            Ok(reg) => Ok(ValueOperand::Register { r: reg }),
            _ => Ok(
                // TODO: Check if immediate is too large for destination
                ValueOperand::Immediate {
                    i: parse_immediate_arg(instruction[1])?,
                },
            ),
        }
    }
}

#[cfg(test)]
mod instruction_parse_test {
    use crate::{
        parser::instructions::{parse_instruction, Instruction, JumpTarget, ValueOperand},
        parser::registers::{GPRegister, Register},
    };

    use super::JumpCondition;

    #[test]
    fn mov_memory() {
        assert_eq!(
            parse_instruction("mov al, BYTE PTR [rip + .LCharacter]"),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "AL".to_string(),
                        size: 1,
                        part_of: GPRegister::RAX,
                    }
                },
                source: ValueOperand::Memory {
                    label: ".LCharacter".to_string(),
                    size: 1
                },
            })
        );
    }

    #[test]
    fn mov_immediate() {
        assert_eq!(
            parse_instruction("mov rax, 53"),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "RAX".to_string(),
                        size: 8,
                        part_of: GPRegister::RAX,
                    },
                },
                source: ValueOperand::Immediate { i: 53 },
            })
        );
        assert_eq!(
            parse_instruction("mov  eAx, 53"),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "EAX".to_string(),
                        size: 4,
                        part_of: GPRegister::RAX,
                    },
                },
                source: ValueOperand::Immediate { i: 53 },
            })
        );
        assert_eq!(
            parse_instruction("mov  al, 'z'"),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "AL".to_string(),
                        size: 1,
                        part_of: GPRegister::RAX,
                    }
                },
                source: ValueOperand::Immediate { i: 122 },
            })
        );
        assert_eq!(
            parse_instruction(r#"mov  al, '\n'"#),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "AL".to_string(),
                        size: 1,
                        part_of: GPRegister::RAX,
                    }
                },
                source: ValueOperand::Immediate { i: 10 },
            })
        );
        assert_eq!(
            parse_instruction(r#"mov  al, '\t'"#),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "AL".to_string(),
                        size: 1,
                        part_of: GPRegister::RAX,
                    }
                },
                source: ValueOperand::Immediate { i: 9 },
            })
        );
    }

    #[test]
    fn mov_register() {
        assert_eq!(
            parse_instruction("mov rdi, rax"),
            Ok(Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "RDI".to_string(),
                        size: 8,
                        part_of: GPRegister::RDI,
                    }
                },
                source: ValueOperand::Register {
                    r: Register {
                        name: "RAX".to_string(),
                        size: 8,
                        part_of: GPRegister::RAX,
                    }
                }
            })
        )
    }

    #[test]
    fn imul() {
        assert_eq!(
            parse_instruction("imul rax, rbx"),
            Ok(Instruction::IMUL {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Register {
                    r: Register {
                        name: "RBX".to_string(),
                        size: 8,
                        part_of: GPRegister::RBX,
                    }
                }
            })
        );
        assert_eq!(
            parse_instruction("imul rax, 52"),
            Ok(Instruction::IMUL {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 52 }
            })
        );
    }

    #[test]
    fn push_register() {
        assert_eq!(
            parse_instruction("push rbx"),
            Ok(Instruction::PUSH {
                source: ValueOperand::Register {
                    r: Register {
                        name: "RBX".to_string(),
                        size: 8,
                        part_of: GPRegister::RBX,
                    },
                }
            })
        );
        assert_eq!(
            parse_instruction("push r8d"),
            Ok(Instruction::PUSH {
                source: ValueOperand::Register {
                    r: Register {
                        name: "R8D".to_string(),
                        size: 4,
                        part_of: GPRegister::R8,
                    },
                }
            })
        );
        assert_eq!(
            parse_instruction("push sp"),
            Ok(Instruction::PUSH {
                source: ValueOperand::Register {
                    r: Register {
                        name: "SP".to_string(),
                        size: 2,
                        part_of: GPRegister::RSP,
                    },
                }
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
            Ok(Instruction::POP {
                destination: Register {
                    name: "RBX".to_string(),
                    size: 8,
                    part_of: GPRegister::RBX,
                },
            })
        );
        assert_eq!(
            parse_instruction("pop r8d"),
            Ok(Instruction::POP {
                destination: Register {
                    name: "R8D".to_string(),
                    size: 4,
                    part_of: GPRegister::R8,
                },
            })
        );
        assert_eq!(
            parse_instruction("pop sp"),
            Ok(Instruction::POP {
                destination: Register {
                    name: "SP".to_string(),
                    size: 2,
                    part_of: GPRegister::RSP,
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
            Ok(Instruction::ADD {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 53 },
            })
        );
        assert_eq!(
            parse_instruction("add  eAx, 53"),
            Ok(Instruction::ADD {
                destination: Register {
                    name: "EAX".to_string(),
                    size: 4,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 53 },
            })
        );
        assert_eq!(
            parse_instruction("AdD  al, 'z'"),
            Ok(Instruction::ADD {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 122 },
            })
        );
        assert_eq!(
            parse_instruction(r#"add  al, '\n'"#),
            Ok(Instruction::ADD {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 10 },
            })
        );
        assert_eq!(
            parse_instruction(r#"add  al, '\t'"#),
            Ok(Instruction::ADD {
                destination: Register {
                    name: "AL".to_string(),
                    size: 1,
                    part_of: GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 9 },
            })
        );
    }

    #[test]
    fn not_register() {
        assert_eq!(
            parse_instruction("not rbx"),
            Ok(Instruction::NOTreg {
                destination: Register {
                    name: "RBX".to_string(),
                    size: 8,
                    part_of: GPRegister::RBX,
                },
            })
        );
    }

    #[test]
    fn test_immediate() {
        assert_eq!(
            parse_instruction("test rsp, 0xF"),
            Ok(Instruction::TEST {
                src1: Register {
                    name: "RSP".to_string(),
                    size: 8,
                    part_of: GPRegister::RSP,
                },
                src2: ValueOperand::Immediate { i: 0xf }
            })
        )
    }

    #[test]
    fn sub() {
        assert_eq!(
            parse_instruction("sub rsp, 8"),
            Ok(Instruction::SUB {
                destination: Register {
                    name: "RSP".to_string(),
                    size: 8,
                    part_of: GPRegister::RSP,
                },
                source: ValueOperand::Immediate { i: 8 }
            })
        );
        assert_eq!(
            parse_instruction("sub rsp, rbx"),
            Ok(Instruction::SUB {
                destination: Register {
                    name: "RSP".to_string(),
                    size: 8,
                    part_of: GPRegister::RSP,
                },
                source: ValueOperand::Register {
                    r: Register {
                        name: "RBX".to_string(),
                        size: 8,
                        part_of: GPRegister::RBX,
                    },
                }
            })
        );
        assert_eq!(
            parse_instruction("sub rsp, ebx"),
            Err(
                "invalid use of registers RSP and EBX of different size in SUB instruction"
                    .to_string()
            )
        );
    }

    #[test]
    fn jump_unconditional() {
        assert_eq!(
            parse_instruction("jmp .LeaxWins_0"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Absolute {
                    label: ".LeaxWins_0".to_string()
                },
                condition: JumpCondition::None
            }),
        );
        assert_eq!(
            parse_instruction("jmp 2f"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Relative {
                    forwards: true,
                    label: "2".to_string()
                },
                condition: JumpCondition::None
            }),
        );
        assert_eq!(
            parse_instruction("jmp 1b"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Relative {
                    forwards: false,
                    label: "1".to_string()
                },
                condition: JumpCondition::None
            }),
        )
    }

    #[test]
    fn lea_label() {
        assert_eq!(
            parse_instruction("lea rsi, [rip + .LCharacter]"),
            Ok(Instruction::LEA {
                destination: Register {
                    name: "RSI".to_string(),
                    size: 8,
                    part_of: GPRegister::RSI,
                },
                source: ValueOperand::Memory {
                    label: ".LCharacter".to_string(),
                    size: 0
                }
            })
        );
    }

    #[test]
    fn jump_conditional() {
        assert_eq!(
            parse_instruction("jz 2f"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Relative {
                    forwards: true,
                    label: "2".to_string()
                },
                condition: JumpCondition::ZeroEqual
            }),
        );
        assert_eq!(
            parse_instruction("je 2f"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Relative {
                    forwards: true,
                    label: "2".to_string()
                },
                condition: JumpCondition::ZeroEqual
            }),
        );
        assert_eq!(
            parse_instruction("jg label"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Absolute {
                    label: "label".to_string()
                },
                condition: JumpCondition::Greater,
            }),
        );
        assert_eq!(
            parse_instruction("jl label"),
            Ok(Instruction::JMPlabel {
                target: JumpTarget::Absolute {
                    label: "label".to_string()
                },
                condition: JumpCondition::Less,
            }),
        );
    }

    #[test]
    fn call_label() {
        assert_eq!(
            parse_instruction("call writechar"),
            Ok(Instruction::CALLlabel {
                label: "writechar".to_string(),
            }),
        );
    }

    #[test]
    fn simple_instructions() {
        assert_eq!(parse_instruction("Syscall"), Ok(Instruction::SYSCALL {}),);
        assert_eq!(parse_instruction("rEt"), Ok(Instruction::RET {}),);
    }

    #[test]
    fn xor_reg() {
        assert_eq!(
            parse_instruction("xor rax, rax"),
            Ok(Instruction::XORreg {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: GPRegister::RAX,
                },
                source: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: GPRegister::RAX,
                },
            }),
        );
    }
}

fn parse_immediate_arg(nstr: &str) -> Result<i64, String> {
    match i64::from_str_radix(nstr, 10) {
        Ok(i) => Ok(i),
        _ => {
            if nstr.starts_with("0x") {
                match i64::from_str_radix(&nstr[2..], 16) {
                    Ok(i) => Ok(i),
                    _ => Err(format!("invalid integer constant {}", nstr).to_string()),
                }
            } else {
                parse_as_char_constant(nstr)
            }
        }
    }
}

fn parse_as_char_constant(expr: &str) -> Result<i64, String> {
    match unescape(expr) {
        Some(s) => {
            let chars: Vec<char> = s.chars().collect();
            match s.len() {
                1 => Ok(chars[0] as i64),
                3 => {
                    if chars[0] == '\'' && chars[0] == chars[2] {
                        Ok(chars[1] as i64)
                    } else {
                        Err(format!("invalid quotes in string {}, expected \"'\"", expr))
                    }
                }
                _ => Err(format!(
                    "invalid string/char constant length {} for {} (from {})",
                    s.len(),
                    s,
                    expr
                )),
            }
        }
        None => Err(format!("char unescape not possible for {}", expr)),
    }
}

lazy_static! {
    static ref MEM_OPERAND_REGEX: Regex = Regex::new(r"(?m)(\[(.*?)\]|\S+)").unwrap();
    static ref MEMORY_LABEL_OFFSET_REGEX: Regex =
        Regex::new(r"(?m)^(?:rip\s*\+)?\s*(.*?)\s*$").unwrap();
}

const MEM_OPERAND_SIZE_MAP: phf::Map<&str, u8> = phf_map! {
    "BYTE" => 1,
    "WORD" => 2,
    "DWORD" => 4,
    "QWORD" => 8,
};

fn parse_as_memory_operand(expr: &str) -> Result<ValueOperand, String> {
    let parts: Vec<&str> = MEM_OPERAND_REGEX
        .captures_iter(expr)
        .map(|m| m.get(0).map_or("", |m| m.as_str()))
        .filter(|w| !w.is_empty())
        .collect();

    match parts.as_slice() {
        [reg] => {
            // Something like "[rsp]"
            if !reg.starts_with("[") || !reg.ends_with("]") {
                return Err(format!(
                    "Invalid memory operand {}, must be something like [register]",
                    reg
                ));
            }

            let label: Vec<&str> = MEMORY_LABEL_OFFSET_REGEX
                .captures_iter(&reg[1..&reg.len() - 1])
                .map(|m| m.get(1).map_or("", |p| p.as_str()))
                .filter(|w| !w.is_empty())
                .collect();
            if label.is_empty() {
                return Err(format!(
                    "Invalid memory operand {}, must be something like [register]",
                    reg
                ));
            }

            match Register::parse(label[0].to_string()) {
                Ok(r) => Ok(ValueOperand::DynamicMemory {
                    register: r,
                    size: 0,
                }),
                Err(_) => Ok(ValueOperand::Memory {
                    label: label[0].to_string(),
                    size: 0,
                }),
            }
        }
        [size_str, ptr, deref] => {
            if !ptr.eq_ignore_ascii_case("ptr") {
                return Err(
                    "Second part of memory dereference with 3 parts must be the text \"ptr\""
                        .to_string(),
                );
            }

            let maybe_size = MEM_OPERAND_SIZE_MAP.get_entry(size_str.to_uppercase().as_str());
            if matches!(maybe_size, None) {
                return Err(format!("Invalid memory operand size {}", size_str));
            }

            let (_, size) = maybe_size.unwrap();

            if !deref.starts_with("[") || !deref.ends_with("]") {
                return Err(format!(
                    "Invalid memory operand {}, must be something like [rip + label]",
                    deref
                ));
            }

            let label: Vec<&str> = MEMORY_LABEL_OFFSET_REGEX
                .captures_iter(&deref[1..&deref.len() - 1])
                .map(|m| m.get(1).map_or("", |p| p.as_str()))
                .filter(|w| !w.is_empty())
                .collect();
            match label.len() {
                1 => {
                    return Ok(ValueOperand::Memory {
                        label: label[0].to_string(),
                        size: size.clone(),
                    })
                }
                _ => Err(format!("Cannot parse label in memory offset {}", deref).to_string()),
            }
        }
        _ => {
            return Err(format!("Not a memory dereference: {}", expr).to_string());
        }
    }
}

#[cfg(test)]
mod memory_operands {
    use crate::parser::registers::{GPRegister, Register};

    use super::{parse_as_memory_operand, ValueOperand};

    #[test]
    fn parse_mem_operands() {
        assert_eq!(
            parse_as_memory_operand("[rax]"),
            Ok(ValueOperand::DynamicMemory {
                register: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: GPRegister::RAX,
                },
                size: 0
            })
        );
        assert_eq!(
            parse_as_memory_operand("BYTE PTR [rip + .LCharacter]"),
            Ok(ValueOperand::Memory {
                label: ".LCharacter".to_string(),
                size: 1
            })
        );
        assert_eq!(
            parse_as_memory_operand("QWORD PTR [rip + .Ltmp64]"),
            Ok(ValueOperand::Memory {
                label: ".Ltmp64".to_string(),
                size: 8
            })
        )
    }
}

fn parse_any_arg(expr: &str) -> Result<ValueOperand, String> {
    let mem = parse_as_memory_operand(expr);
    if mem.is_ok() {
        return mem;
    }

    match Register::parse(expr.to_string()) {
        Ok(reg) => Ok(ValueOperand::Register { r: reg }),
        Err(_) => Ok(ValueOperand::Immediate {
            i: parse_immediate_arg(expr)?,
        }),
    }
}
