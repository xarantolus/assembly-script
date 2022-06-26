use std::fmt;

use crate::parser::instructions::{parse_instruction, Instruction};

#[derive(PartialEq, Debug)]
pub struct InputFile {
    pub parsed_lines: Vec<LineType>,
}

#[derive(PartialEq)]
enum Section {
    None,
    Text,
    Data,
}

#[derive(Debug, PartialEq)]
pub enum LineType {
    Label { name: String },
    Instruction { i: Instruction },
}

#[derive(Debug)]
pub struct ParseError {
    line_number: i64,
    line_text: String,

    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "line {} ({}): {}",
            self.line_number, self.line_text, self.message
        )
    }
}

// TODO: reject input with multiple labels of the same name
pub fn parse_gnu_as_input(input_file_content: String) -> Result<InputFile, ParseError> {
    let lines = input_file_content.lines();

    let mut current_section = Section::None;

    let mut parsed_lines: Vec<LineType> = Vec::new();

    let mut line_num: i64 = 0;

    for mut line in lines {
        line_num += 1;

        line = line.trim();
        if is_ignored_line(line) {
            continue;
        }

        if line.starts_with(".") {
            let section_line = parse_section(line);
            if section_line.is_ok() {
                current_section = section_line.ok().unwrap();
                continue;
            }
        }

        if current_section == Section::Data || current_section == Section::None {
            continue;
        }

        // Look for labels like "function_name:"
        // some lines also are like "label: instruction", we want to handle that too
        let split = line.split_once(":");
        if split.is_some() {
            let (label, rest) = split.unwrap();
            parsed_lines.push(parse_label(label).map_err(|e| ParseError {
                line_number: line_num,
                line_text: line.to_string(),
                message: "parsing label: ".to_string() + e.as_str(),
            })?);

            let trimmed = rest.trim();
            if trimmed.is_empty() {
                continue;
            }

            // Fall though to instruction parsing
            line = trimmed;
        }

        let next_instruction = parse_instruction(line).map_err(|e| ParseError {
            line_number: line_num,
            line_text: line.to_string(),
            message: "parsing instruction: ".to_string() + e.as_str(),
        })?;

        parsed_lines.push(LineType::Instruction {
            i: next_instruction,
        })
    }

    Ok(InputFile {
        parsed_lines: parsed_lines,
    })
}

#[cfg(test)]
mod test_gnu_as_parser {
    use crate::parser::instructions::{Instruction, ValueOperand};
    use crate::parser::registers::GPRegister;
    use crate::parser::{
        input::{InputFile, LineType},
        registers::Register,
    };

    use super::parse_gnu_as_input;

    #[test]
    fn simple() {
        let res = parse_gnu_as_input(
            r#"
        .text
        label:
        mov rax, 0
        ret
        "#
            .to_string(),
        );
        assert!(res.is_ok());

        let parsed = res.ok().unwrap();

        assert_eq!(
            parsed,
            InputFile {
                parsed_lines: vec![
                    LineType::Label {
                        name: "label".to_string()
                    },
                    LineType::Instruction {
                        i: Instruction::MOV {
                            destination: ValueOperand::Register {
                                r: Register {
                                    name: "RAX".to_string(),
                                    size: 8,
                                    part_of: GPRegister::RAX,
                                }
                            },
                            source: ValueOperand::Immediate { i: 0 }
                        }
                    },
                    LineType::Instruction {
                        i: Instruction::RET {}
                    },
                ],
            }
        );
    }

    #[test]
    fn simple_call() {
        let res = parse_gnu_as_input(
            r#"
        .text
        label: call label
        ret
        "#
            .to_string(),
        );
        assert!(res.is_ok());

        let parsed = res.ok().unwrap();

        assert_eq!(
            parsed,
            InputFile {
                parsed_lines: vec![
                    LineType::Label {
                        name: "label".to_string()
                    },
                    LineType::Instruction {
                        i: Instruction::CALLlabel {
                            label: "label".to_string()
                        }
                    },
                    LineType::Instruction {
                        i: Instruction::RET {}
                    },
                ],
            }
        );
    }

    #[test]
    fn alphabet() {
        let alphabet_assembly = include_str!("testdata/alphabet.S");
        let result = parse_gnu_as_input(alphabet_assembly.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap());
    }
}

fn parse_section(line: &str) -> Result<Section, String> {
    match line {
        ".data" => Ok(Section::Data),
        ".text" => Ok(Section::Text),
        _ => Err(format!("invalid section start {}", line)),
    }
}

fn parse_label(line: &str) -> Result<LineType, String> {
    let split: Vec<&str> = line.split_whitespace().collect();

    if split.len() != 1 {
        return Err(format!("invalid label line {}", line));
    }

    let line_str = line.trim_end_matches(":");
    return Ok(LineType::Label {
        name: line_str[..line_str.len()].to_string(),
    });
}

#[cfg(test)]
mod label_parser_test {
    use crate::parser::input::{parse_label, LineType};

    #[test]
    fn happy() {
        assert_eq!(
            parse_label(".test:"),
            Ok(LineType::Label {
                name: ".test".to_string()
            })
        );
    }

    #[test]
    fn err() {
        assert_eq!(
            parse_label("t e s t:"),
            Err("invalid label line t e s t:".to_string())
        );
    }
}

fn is_comment(line: &str) -> bool {
    return line.starts_with("#");
}

fn is_ignored_line(line: &str) -> bool {
    return line.is_empty()
        || is_comment(line)
        || line == ".intel_syntax noprefix"
        || line.starts_with(".global");
}

#[cfg(test)]
mod ignored_line_tests {
    use crate::parser::input::is_ignored_line;

    #[test]
    fn ignored_lines() {
        assert!(is_ignored_line(""));
        assert!(is_ignored_line("# comment"));
        assert!(is_ignored_line(".intel_syntax noprefix"));
        assert!(is_ignored_line(".global main"));
    }
    #[test]
    fn important_lines() {
        assert!(!is_ignored_line("mov rax, 0"));
        assert!(!is_ignored_line(".label:"));
    }
}