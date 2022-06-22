pub struct InputFile {
    pub parsed_lines: Vec<LineType>,
}

#[derive(PartialEq)]
enum Section {
    None,
    Text,
    Data,
}

pub enum Instruction {}

#[derive(Debug, PartialEq)]
pub enum LineType {
    Label { name: String },
}

pub fn parse_gnu_as_input(input_file_content: String) -> Result<InputFile, String> {
    let lines = input_file_content.lines();

    let mut current_section = Section::None;

    let mut parsed_lines = Vec::new();

    for mut line in lines {
        line = line.trim();
        if is_ignored_line(line) {
            continue;
        }

        if line.starts_with(".") {
            if line.split_whitespace().count() == 1 {
                if line.ends_with(":") {
                    // Look for labels like "function_name:"
                    parsed_lines.push(parse_label(line)?)
                } else {
                    // Look for stuff like `.data` and `.text`
                    current_section = parse_section(line)?
                }
            } else if current_section == Section::Data {
                // TODO: Parse data section, especially
                // .LCharacter: .ascii "a"
                // .Ltmp64: .byte 0, 0, 0, 0, 0, 0, 0, 0
                // .LsigStruct:  // <-- This line might be interesting to handle; or I just ignore and compile with --no-matyrdom
                //     .Lsa_handler: .quad 0
                //     .quad 0x04000000
                //     .quad 0, 0
                // Alternatively just don't parse the data section at all and just allow writing/reading from any label
            }
        }
    }

    Ok(InputFile {
        parsed_lines: parsed_lines,
    })
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

    let line_str = line.to_string();
    return Ok(LineType::Label {
        name: line_str[..line_str.len() - 1].to_string(),
    });
}

#[cfg(test)]
mod label_parser_test {
    use crate::parser::{parse_label, LineType};

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
    use crate::parser::is_ignored_line;

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
