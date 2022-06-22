use crate::registers::Register;
use unescape::unescape;

#[derive(PartialEq, Debug)]
pub enum Instruction {
    MOVimm { target: Register, operand: i64 },
}

fn parse_instruction(line: &str) -> Result<Instruction, String> {
    let split: Vec<&str> = line
        .split([' ', ',', '\t'])
        .filter(|s| !s.is_empty())
        .collect();

    match (split[0].to_uppercase().as_str(), split.len()) {
        ("MOV", 3) => Ok(Instruction::MOVimm {
            target: Register::parse(split[1].to_string())?,
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
                target: Register {
                    name: "RAX".to_string(),
                    size: 8
                },
                operand: 53,
            })
        );
        assert_eq!(
            parse_instruction("mov  eAx, 53"),
            Ok(Instruction::MOVimm {
                target: Register {
                    name: "EAX".to_string(),
                    size: 4
                },
                operand: 53,
            })
        );
        assert_eq!(
            parse_instruction("mov  al, 'z'"),
            Ok(Instruction::MOVimm {
                target: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 122,
            })
        );
        assert_eq!(
            parse_instruction(r#"mov  al, '\n'"#),
            Ok(Instruction::MOVimm {
                target: Register {
                    name: "AL".to_string(),
                    size: 1
                },
                operand: 10,
            })
        );
        assert_eq!(
            parse_instruction(r#"mov  al, '\t'"#),
            Ok(Instruction::MOVimm {
                target: Register {
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
