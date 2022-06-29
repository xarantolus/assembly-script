use phf::phf_map;
use std::{cell::Cell, collections::HashMap, fmt};

use iced_x86::{code_asm::*, MemoryOperand, Register};
use lazy_static::lazy_static;

use crate::parser::{
    input::{InputFile, LineType},
    instructions,
    instructions::{JumpCondition, JumpTarget, ValueOperand},
    registers,
};

#[derive(Debug)]
pub enum EncodeError {
    IcedError { e: IcedError },
    StrError { e: String },
}

impl fmt::Display for EncodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EncodeError::IcedError { e } => write!(f, "{}", e),
            EncodeError::StrError { e } => write!(f, "{}", e),
        }
    }
}

#[must_use]
fn strerror(e: String) -> Result<(), EncodeError> {
    Err(EncodeError::StrError { e: e.to_string() })
}

impl From<IcedError> for EncodeError {
    fn from(error: IcedError) -> Self {
        EncodeError::IcedError { e: error }
    }
}
impl From<String> for EncodeError {
    fn from(error: String) -> Self {
        EncodeError::StrError { e: error }
    }
}
impl From<&str> for EncodeError {
    fn from(error: &str) -> Self {
        EncodeError::StrError {
            e: error.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EncodeResult {
    code: Vec<u8>,
    first_instr_address: u64,

    data_start_address: u64,
    data_section_size: u64,
}

pub fn encode_file(
    input: InputFile,
    instr_start_address: u64,
    data_start_address: u64,
) -> Result<EncodeResult, EncodeError> {
    let mut assembler = CodeAssembler::new(64)?;

    // Maps a label line index to the instruction address
    let mut named_labels: HashMap<String, Cell<CodeLabel>> = HashMap::new();

    // data_section maps a data label name to its offset and data size
    let mut data_section: Vec<(String, i64, i8)> = vec![];
    let mut next_data_section_offset = 0;

    let mut labeled_mem_operand = |label: String, size: i8| -> Result<u64, EncodeError> {
        match data_section.iter().find(|(l, _, _)| label.eq(l)) {
            Some((_, offset, s)) => {
                if size.eq(s) {
                    Ok(data_start_address + offset.clone() as u64)
                } else {
                    Err(EncodeError::StrError {
                        e: format!("data label {} has size {} but expected {}", label, s, size),
                    })
                }
            }
            None => {
                let next = data_start_address + next_data_section_offset as u64;
                data_section.push((label, next_data_section_offset, size));
                next_data_section_offset += i64::from(size);

                Ok(next)
            }
        }
    };

    for (idx, line) in input.parsed_lines.iter().enumerate() {
        match line {
            LineType::Label { l } => match l.clone() {
                JumpTarget::Absolute { label: name } => {
                    // Get or create this label
                    let mut label = match named_labels.get(&name) {
                        Some(l) => l.to_owned(),
                        None => Cell::new(assembler.create_label()),
                    };

                    // Now set this label to the current line
                    assembler.set_label(label.get_mut())?;

                    // Create a zero-byte instruction for this to prevent conflicts on multiple labels after each other
                    assembler.zero_bytes()?;

                    named_labels.insert(name, label.to_owned());
                }
                JumpTarget::Relative {
                    label: _,
                    forwards: _,
                } => {
                    assembler.anonymous_label()?;
                    assembler.zero_bytes()?;
                }
            },
            LineType::Instruction { i } => match i {
                // mov for all combinations of memory operands, registers and immediates with a nested match statement
                instructions::Instruction::MOV {
                    destination: dst,
                    source: src,
                } => {
                    match dst.to_owned() {
                        // Implement mov for memory and register destinations
                        ValueOperand::Memory { label, size } => {
                            // Move register or immediate to memory
                            match src.to_owned() {
                                ValueOperand::Register { r } => match r.size.to_owned() {
                                    1 => assembler.mov(
                                        byte_ptr(labeled_mem_operand(label, size)?),
                                        gpr8::get_gpr8(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 8-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?,
                                    2 => assembler.mov(
                                        word_ptr(labeled_mem_operand(label, size)?),
                                        gpr16::get_gpr16(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 16-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?,
                                    4 => assembler.mov(
                                        word_ptr(labeled_mem_operand(label, size)?),
                                        gpr32::get_gpr32(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 32-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?,
                                    8 => assembler.mov(
                                        qword_ptr(labeled_mem_operand(label, size)?),
                                        gpr64::get_gpr64(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 64-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?,
                                    _ => {
                                        strerror(format!("Invalid register size {:?}", r.size))?;
                                    }
                                },
                                ValueOperand::Immediate { i } => match size.to_owned() {
                                    1 => assembler.mov(
                                        byte_ptr(labeled_mem_operand(label, size)?),
                                        i as i32,
                                    )?,
                                    2 => assembler.mov(
                                        word_ptr(labeled_mem_operand(label, size)?),
                                        i as i32,
                                    )?,
                                    4 => assembler.mov(
                                        dword_ptr(labeled_mem_operand(label, size)?),
                                        i as i32,
                                    )?,
                                    _ => {
                                        strerror(format!("Invalid immediate size {:?}", size))?;
                                    }
                                },
                                _ => {
                                    strerror(format!(
                                        "mov source {:?} is not a register or immediate",
                                        src,
                                    ))?;
                                }
                            }
                        }
                        ValueOperand::Register { r: destr } => match dst.clone() {
                            ValueOperand::Register { r } => match r.size.to_owned() {
                                1 => {
                                    assembler.mov(
                                        gpr8::get_gpr8(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 8-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                        gpr8::get_gpr8(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 8-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?;
                                }
                                2 => {
                                    assembler.mov(
                                        gpr16::get_gpr16(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 16-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                        gpr16::get_gpr16(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 16-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?;
                                }
                                4 => {
                                    assembler.mov(
                                        gpr32::get_gpr32(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 32-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                        gpr32::get_gpr32(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 32-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?;
                                }
                                8 => {
                                    assembler.mov(
                                        gpr64::get_gpr64(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 64-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                        gpr64::get_gpr64(
                                            REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 64-bit register {:?}", r)
                                                .to_string(),
                                        )?,
                                    )?;
                                }
                                _ => {
                                    strerror(format!("Invalid register size {:?}", r.size))?;
                                }
                            },
                            ValueOperand::Immediate { i } => match destr.size.to_owned() {
                                1 => {
                                    assembler.mov(
                                        gpr8::get_gpr8(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 8-bit register {:?}", destr)
                                                .to_string(),
                                        )?,
                                        i as i32,
                                    )?;
                                }
                                2 => {
                                    assembler.mov(
                                        gpr16::get_gpr16(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 16-bit register {:?}", destr)
                                                .to_string(),
                                        )?,
                                        i as i32,
                                    )?;
                                }
                                4 => {
                                    assembler.mov(
                                        gpr32::get_gpr32(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 32-bit register {:?}", destr)
                                                .to_string(),
                                        )?,
                                        i as i32,
                                    )?;
                                }
                                8 => {
                                    assembler.mov(
                                        gpr64::get_gpr64(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 64-bit register {:?}", destr)
                                                .to_string(),
                                        )?,
                                        i as i64,
                                    )?;
                                }
                                _ => {
                                    strerror(format!("Invalid register size {:?}", destr.size))?;
                                }
                            },
                            _ => {
                                strerror(format!(
                                    "mov destination {:?} is not a register or immediate",
                                    dst,
                                ))?;
                            }
                        },
                        _ => {
                            strerror(format!(
                                "mov destination {:?} is not a memory or register operand",
                                dst,
                            ))?;
                        }
                    }
                }

                // sub matching for 64-bit registers and immediate values
                instructions::Instruction::SUB {
                    destination,
                    source,
                } => {
                    match source.clone() {
                        ValueOperand::Register { r } => match destination.size.to_owned() {
                            1 => {
                                assembler.sub(
                                    gpr8::get_gpr8(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 8-bit register {:?}", r).to_string(),
                                    )?,
                                    gpr8::get_gpr8(
                                        REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 8-bit register {:?}", r).to_string(),
                                    )?,
                                )?;
                            }
                            2 => {
                                assembler.sub(
                                    gpr16::get_gpr16(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 16-bit register {:?}", r)
                                            .to_string(),
                                    )?,
                                    gpr16::get_gpr16(
                                        REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 16-bit register {:?}", r)
                                            .to_string(),
                                    )?,
                                )?;
                            }
                            4 => {
                                assembler.sub(
                                    gpr32::get_gpr32(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 32-bit register {:?}", r)
                                            .to_string(),
                                    )?,
                                    gpr32::get_gpr32(
                                        REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 32-bit register {:?}", r)
                                            .to_string(),
                                    )?,
                                )?;
                            }
                            8 => {
                                assembler.sub(
                                    gpr64::get_gpr64(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 64-bit register {:?}", r)
                                            .to_string(),
                                    )?,
                                    gpr64::get_gpr64(
                                        REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 64-bit register {:?}", r)
                                            .to_string(),
                                    )?,
                                )?;
                            }
                            _ => {
                                strerror(format!("Invalid register size {:?}", r.size))?;
                            }
                        },
                        ValueOperand::Immediate { i } => match destination.size.to_owned() {
                            1 => {
                                assembler.sub(
                                    gpr8::get_gpr8(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 8-bit register {:?}", destination)
                                            .to_string(),
                                    )?,
                                    i as i32,
                                )?;
                            }
                            2 => {
                                assembler.sub(
                                    gpr16::get_gpr16(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 16-bit register {:?}", destination)
                                            .to_string(),
                                    )?,
                                    i as i32,
                                )?;
                            }
                            4 => {
                                assembler.sub(
                                    gpr32::get_gpr32(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 32-bit register {:?}", destination)
                                            .to_string(),
                                    )?,
                                    i as i32,
                                )?;
                            }
                            8 => {
                                assembler.sub(
                                    gpr64::get_gpr64(
                                        REGISTERS
                                            .get(destination.name.as_str())
                                            .unwrap()
                                            .to_owned(),
                                    )
                                    .ok_or(
                                        format!("Could not get 64-bit register {:?}", destination)
                                            .to_string(),
                                    )?,
                                    i as i32,
                                )?;
                            }
                            _ => {
                                strerror(format!("Invalid register size {:?}", destination.size))?;
                            }
                        },
                        _ => {
                            strerror(
                                format!("sub source {:?} is not a register operand", source,),
                            )?;
                        }
                    };
                }
                // ADD matching for all register types (1,2,4,8) and immediate values as well as all other register types (1,2,4,8)
                instructions::Instruction::ADD {
                    destination,
                    source,
                } => match source.clone() {
                    ValueOperand::Register { r } => match destination.size.to_owned() {
                        1 => {
                            assembler.add(
                                gpr8::get_gpr8(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", r).to_string(),
                                )?,
                                gpr8::get_gpr8(REGISTERS.get(r.name.as_str()).unwrap().to_owned())
                                    .ok_or(
                                        format!("Could not get 8-bit register {:?}", r).to_string(),
                                    )?,
                            )?;
                        }
                        2 => {
                            assembler.add(
                                gpr16::get_gpr16(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                gpr16::get_gpr16(
                                    REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", r).to_string(),
                                )?,
                            )?;
                        }
                        4 => {
                            assembler.add(
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                gpr32::get_gpr32(
                                    REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", r).to_string(),
                                )?,
                            )?;
                        }
                        8 => {
                            assembler.add(
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                gpr64::get_gpr64(
                                    REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", r).to_string(),
                                )?,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", destination.size))?;
                        }
                    },
                    ValueOperand::Immediate { i } => match destination.size.to_owned() {
                        1 => {
                            assembler.add(
                                gpr8::get_gpr8(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i as i32,
                            )?;
                        }
                        2 => {
                            assembler.add(
                                gpr16::get_gpr16(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i as i32,
                            )?;
                        }
                        4 => {
                            assembler.add(
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i as i32,
                            )?;
                        }
                        8 => {
                            assembler.add(
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i as i32,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", destination.size))?;
                        }
                    },
                    _ => {
                        strerror(format!(
                            "add source {:?} is not a register or immediate operand",
                            source
                        ))?;
                    }
                },

                // TEST instruction with two operands: register and immediate operand, e.g. test rsp, 0xf
                instructions::Instruction::TEST { src1, src2 } => match src2 {
                    ValueOperand::Immediate { i } => match src1.size.clone() {
                        1 => {
                            assembler.test(
                                gpr8::get_gpr8(
                                    REGISTERS.get(src1.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", src1).to_string(),
                                )?,
                                i.to_owned() as i32,
                            )?;
                        }
                        2 => {
                            assembler.test(
                                gpr16::get_gpr16(
                                    REGISTERS.get(src1.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", src1).to_string(),
                                )?,
                                i.to_owned() as i32,
                            )?;
                        }
                        4 => {
                            assembler.test(
                                gpr32::get_gpr32(
                                    REGISTERS.get(src1.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", src1).to_string(),
                                )?,
                                i.to_owned() as i32,
                            )?;
                        }
                        8 => {
                            assembler.test(
                                gpr64::get_gpr64(
                                    REGISTERS.get(src1.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", src1).to_string(),
                                )?,
                                i.to_owned() as i32,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", src1.size))?;
                        }
                    },
                    _ => {
                        strerror(format!(
                            "test: second source operand is not a register or immediate"
                        ))?;
                    }
                },
                instructions::Instruction::JMPlabel { target, condition } => {
                    let target_label = match target {
                        JumpTarget::Absolute { label: name } => match named_labels.get(name) {
                            Some(label) => label.to_owned(),
                            None => {
                                let new_label = Cell::new(assembler.create_label());
                                named_labels.insert(name.to_owned(), new_label.to_owned());
                                new_label
                            }
                        },
                        JumpTarget::Relative { label: _, forwards } => Cell::new(
                            (if forwards.to_owned() {
                                assembler.fwd()
                            } else {
                                assembler.bwd()
                            })?,
                        ),
                    };

                    match condition.to_owned() {
                        JumpCondition::None => {
                            assembler.jmp(target_label.get())?;
                        }
                        JumpCondition::Greater => {
                            assembler.jg(target_label.get())?;
                        }
                        JumpCondition::Less => {
                            assembler.jl(target_label.get())?;
                        }
                        JumpCondition::ZeroEqual => {
                            assembler.je(target_label.get())?;
                        }
                        _ => {
                            strerror(format!("unsupported jump condition: {:?}", condition))?;
                        }
                    }
                }

                instructions::Instruction::CALLlabel { label: target } => {
                    let target_label = match named_labels.get(target) {
                        Some(label) => label.to_owned(),
                        None => {
                            let new_label = Cell::new(assembler.create_label());
                            named_labels.insert(target.to_owned(), new_label.to_owned());
                            new_label
                        }
                    };

                    assembler.call(target_label.get())?;
                }

                instructions::Instruction::SYSCALL {} => {
                    assembler.syscall()?;
                }
                instructions::Instruction::RET {} => {
                    assembler.ret()?;
                }
                _ => {
                    strerror(format!("unsupported instruction: {:?}", i).to_string())?;
                }
            },
        }
    }

    let result = assembler.assemble(instr_start_address)?;

    return Ok(EncodeResult {
        code: result,
        first_instr_address: instr_start_address,
        data_start_address,
        data_section_size: 0,
    });
}

#[cfg(test)]
mod test_encoder {
    use crate::{
        encoder::encoder::encode_file,
        parser::{
            input::{self, LineType},
            instructions::{Instruction, JumpCondition, JumpTarget, ValueOperand},
            registers::{self, Register},
        },
    };

    fn assert_encoding(i: Vec<Instruction>, u: Vec<u8>) {
        let input_lines: Vec<input::LineType> = i
            .iter()
            .map(|instr: &Instruction| input::LineType::Instruction { i: instr.clone() })
            .collect();

        assert_encoding_lines(input_lines, u);
    }

    fn assert_encoding_lines(i: Vec<LineType>, u: Vec<u8>) {
        let encode_res = encode_file(
            crate::parser::input::InputFile { parsed_lines: i },
            0x2000,
            0x1000,
        );
        assert!(encode_res.is_ok(), "{}", encode_res.err().unwrap());

        assert_eq!(encode_res.unwrap().code, u);
    }

    #[test]
    fn jumps() {
        assert_encoding_lines(
            vec![
                LineType::Label {
                    l: JumpTarget::Absolute {
                        label: "label".to_owned(),
                    },
                },
                LineType::Instruction {
                    i: Instruction::JMPlabel {
                        target: JumpTarget::Absolute {
                            label: "label".to_owned(),
                        },
                        condition: JumpCondition::None,
                    },
                },
            ],
            vec![0xeb, 0xfe],
        );

        assert_encoding_lines(
            vec![
                LineType::Label {
                    l: JumpTarget::Absolute {
                        label: "label".to_owned(),
                    },
                },
                LineType::Instruction {
                    i: Instruction::MOV {
                        destination: ValueOperand::Register {
                            r: Register {
                                name: "RBX".to_string(),
                                size: 8,
                                part_of: registers::GPRegister::RBX,
                            },
                        },
                        source: ValueOperand::Register {
                            r: Register {
                                name: "RDI".to_string(),
                                size: 8,
                                part_of: registers::GPRegister::RDI,
                            },
                        },
                    },
                },
                LineType::Instruction {
                    i: Instruction::JMPlabel {
                        target: JumpTarget::Absolute {
                            label: "label".to_owned(),
                        },
                        condition: JumpCondition::None,
                    },
                },
            ],
            vec![
                0x48, 0x89, 0xf3, // MOV RBX, RDI
                0xeb, 0xfe, // JMP label
            ],
        );
    }

    #[test]
    fn ret_is_64_bit() {
        assert_encoding(vec![Instruction::RET {}], vec![0xc3]);
    }

    #[test]
    fn syscall() {
        assert_encoding(vec![Instruction::SYSCALL {}], vec![0x0f, 0x05]);
    }

    #[test]
    fn test() {
        assert_encoding(
            vec![Instruction::TEST {
                src1: registers::Register {
                    name: "RSP".to_string(),
                    size: 8,
                    part_of: registers::GPRegister::RSP,
                },
                src2: ValueOperand::Immediate { i: 0xf },
            }],
            vec![0x48, 0xf7, 0xc4, 0x0f, 00, 00, 00],
        )
    }

    #[test]
    fn mov_mem_imm8() {
        assert_encoding(
            vec![Instruction::MOV {
                destination: ValueOperand::Memory {
                    label: "foo".to_string(),
                    size: 1,
                },
                source: ValueOperand::Immediate { i: 0x42 },
            }],
            vec![0xc6, 0x4, 0x25, 0, 16, 0, 0, 66],
        );
    }

    #[test]
    fn alphabet() {
        let alphabet_assembly = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/parser/testdata/alphabet.S"
        ));
        let result = input::parse_gnu_as_input(alphabet_assembly.to_string());
        assert!(result.is_ok(), "{}", result.err().unwrap());

        let encode_res = encode_file(result.unwrap(), 0x2000, 0x1000);
        assert!(encode_res.is_ok(), "{}", encode_res.err().unwrap());
    }
}

lazy_static! {
    static ref REGISTERS: HashMap<&'static str, Register> = HashMap::from([
        ("AL", Register::AL),
        ("CL", Register::CL),
        ("DL", Register::DL),
        ("BL", Register::BL),
        ("AH", Register::AH),
        ("CH", Register::CH),
        ("DH", Register::DH),
        ("BH", Register::BH),
        ("SPL", Register::SPL),
        ("BPL", Register::BPL),
        ("SIL", Register::SIL),
        ("DIL", Register::DIL),
        ("R8B", Register::R8L),
        ("R9B", Register::R9L),
        ("R10B", Register::R10L),
        ("R11B", Register::R11L),
        ("R12B", Register::R12L),
        ("R13B", Register::R13L),
        ("R14B", Register::R14L),
        ("R15B", Register::R15L),
        ("AX", Register::AX),
        ("CX", Register::CX),
        ("DX", Register::DX),
        ("BX", Register::BX),
        ("SP", Register::SP),
        ("BP", Register::BP),
        ("SI", Register::SI),
        ("DI", Register::DI),
        ("R8W", Register::R8W),
        ("R9W", Register::R9W),
        ("R10W", Register::R10W),
        ("R11W", Register::R11W),
        ("R12W", Register::R12W),
        ("R13W", Register::R13W),
        ("R14W", Register::R14W),
        ("R15W", Register::R15W),
        ("EAX", Register::EAX),
        ("ECX", Register::ECX),
        ("EDX", Register::EDX),
        ("EBX", Register::EBX),
        ("ESP", Register::ESP),
        ("EBP", Register::EBP),
        ("ESI", Register::ESI),
        ("EDI", Register::EDI),
        ("R8D", Register::R8D),
        ("R9D", Register::R9D),
        ("R10D", Register::R10D),
        ("R11D", Register::R11D),
        ("R12D", Register::R12D),
        ("R13D", Register::R13D),
        ("R14D", Register::R14D),
        ("R15D", Register::R15D),
        ("RAX", Register::RAX),
        ("RCX", Register::RCX),
        ("RDX", Register::RDX),
        ("RBX", Register::RBX),
        ("RSP", Register::RSP),
        ("RBP", Register::RBP),
        ("RSI", Register::RSI),
        ("RDI", Register::RDI),
        ("R8", Register::R8),
        ("R9", Register::R9),
        ("R10", Register::R10),
        ("R11", Register::R11),
        ("R12", Register::R12),
        ("R13", Register::R13),
        ("R14", Register::R14),
        ("R15", Register::R15),
    ]);
}
