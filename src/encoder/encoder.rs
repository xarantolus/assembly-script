use serde::Serialize;
use std::{cell::Cell, collections::HashMap, fmt};
use wasm_bindgen::prelude::wasm_bindgen;

use iced_x86::{code_asm::*, BlockEncoderOptions, Register};
use lazy_static::lazy_static;

use crate::parser::{
    input::{InputFile, LineType},
    instructions,
    instructions::{JumpCondition, JumpTarget, ValueOperand},
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

#[derive(Debug, Clone, Serialize)]
pub struct EncodeResult {
    pub code: Vec<u8>,
    pub code_start_address: u64,
    pub entrypoint_address: u64,

    pub data_start_address: u64,
    pub data_section_size: u64,
    pub data_section: Vec<DataSectionEntry>,
}

#[derive(Debug, Clone, Serialize)]
pub struct DataSectionEntry {
    pub label: String,
    pub offset: u64,
    pub size: u8,
}

pub fn encode_file(
    input: InputFile,
    instr_start_address: u64,
    data_start_address: u64,
    entrypoint_name: Option<String>,
) -> Result<EncodeResult, EncodeError> {
    let mut assembler = CodeAssembler::new(64)?;

    // Maps a label line index to the instruction address
    let mut named_labels: HashMap<String, Cell<CodeLabel>> = HashMap::new();

    // data_section maps a data label name to its offset and data size
    let mut data_section: Vec<DataSectionEntry> = vec![];
    let mut next_data_section_offset = 0;

    let mut labeled_mem_operand = |label: String, mut size: u8| -> Result<u64, EncodeError> {
        match data_section.iter().find(|e| label.eq(e.label.as_str())) {
            Some(entry) => {
                if size == 0 || size == entry.size {
                    Ok(data_start_address + entry.offset.clone())
                } else {
                    Err(EncodeError::StrError {
                        e: format!(
                            "data label {} has size {} but expected {}",
                            label, entry.size, size
                        ),
                    })
                }
            }
            None => {
                // If no size is set, we just reserve 8 byte and hope for the best.
                // This *can* happen with lea instructions
                if size == 0 {
                    size = 8;
                }

                let next = data_start_address + next_data_section_offset as u64;
                data_section.push(DataSectionEntry {
                    label: label.clone(),
                    offset: next_data_section_offset,
                    size,
                });
                next_data_section_offset += u64::from(size);

                Ok(next)
            }
        }
    };

    let mut relative_labels_map: HashMap<usize, Cell<CodeLabel>> = HashMap::new();

    for (_idx, line) in input.parsed_lines.iter().enumerate() {
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
                    match relative_labels_map.get_mut(&_idx) {
                        Some(l) => {
                            assembler.set_label(l.get_mut())?;
                        }
                        None => {
                            let mut l = Cell::new(assembler.create_label());
                            assembler.set_label(l.get_mut())?;
                            relative_labels_map.insert(_idx, l);
                        }
                    }
                    // Add a zero-byte instruction to prevent conflicts on multiple labels after each other
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
                                    1 => {
                                        if r.name.eq_ignore_ascii_case("al") {
                                            // Add special case so this doesn't use the `MOV AL, moffs8` opcode, which the interpreter doesn't support
                                            assembler.add_instruction(
                                                iced_x86::Instruction::with2(
                                                    iced_x86::Code::Mov_rm8_r8,
                                                    iced_x86::MemoryOperand::with_displ(
                                                        labeled_mem_operand(label, size)?,
                                                        32,
                                                    ),
                                                    Register::AL,
                                                )?,
                                            )?;
                                        } else {
                                            assembler.mov(
                                                byte_ptr(labeled_mem_operand(label, size)?),
                                                gpr8::get_gpr8(
                                                    REGISTERS
                                                        .get(r.name.as_str())
                                                        .unwrap()
                                                        .to_owned(),
                                                )
                                                .ok_or(
                                                    format!("Could not get 8-bit register {:?}", r)
                                                        .to_string(),
                                                )?,
                                            )?;
                                        }
                                    }
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
                        ValueOperand::Register { r: destr } => match src.clone() {
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
                            ValueOperand::DirectMemory { i } => match destr.size.to_owned() {
                                1 => {
                                    assembler.mov(
                                        gpr8::get_gpr8(
                                            REGISTERS.get(destr.name.as_str()).unwrap().to_owned(),
                                        )
                                        .ok_or(
                                            format!("Could not get 8-bit register {:?}", destr)
                                                .to_string(),
                                        )?,
                                        byte_ptr(i),
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
                                        word_ptr(i),
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
                                        dword_ptr(i),
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
                                        qword_ptr(i),
                                    )?;
                                }
                                _ => {
                                    strerror(format!("Invalid register size {:?}", destr.size))?;
                                }
                            },
                            ValueOperand::Memory { label, size } => match destr.size.to_owned() {
                                1 => {
                                    if destr.name.eq_ignore_ascii_case("al") {
                                        // Add special case so this doesn't use the `MOV AL, moffs8` opcode, which the interpreter doesn't support
                                        assembler.add_instruction(iced_x86::Instruction::with2(
                                            iced_x86::Code::Mov_rm8_r8,
                                            Register::AL,
                                            iced_x86::MemoryOperand::with_displ(
                                                labeled_mem_operand(label, size)?,
                                                32,
                                            ),
                                        )?)?;
                                    } else {
                                        assembler.mov(
                                            gpr8::get_gpr8(
                                                REGISTERS
                                                    .get(destr.name.as_str())
                                                    .unwrap()
                                                    .to_owned(),
                                            )
                                            .ok_or(
                                                format!("Could not get 8-bit register {:?}", destr)
                                                    .to_string(),
                                            )?,
                                            byte_ptr(labeled_mem_operand(label, size)?),
                                        )?;
                                    }
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
                                        word_ptr(labeled_mem_operand(label, size)?),
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
                                        dword_ptr(labeled_mem_operand(label, size)?),
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
                                        qword_ptr(labeled_mem_operand(label, size)?),
                                    )?;
                                }
                                _ => {
                                    strerror(format!("Invalid register size {:?}", destr.size))?;
                                }
                            },
                            _ => {
                                strerror(format!(
                                    "mov source ({:?}) must be a register, memory or immediate operand",
                                    src,
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

                // SHL matching for register destination (1,2,4,8) and immediate value
                instructions::Instruction::SHL {
                    destination,
                    source,
                } => match source.clone() {
                    ValueOperand::Immediate { i } => match destination.size.to_owned() {
                        1 => {
                            assembler.shl(
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
                            assembler.shl(
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
                            assembler.shl(
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
                            assembler.shl(
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
                            "shl source {:?} is not an immediate operand",
                            source
                        ))?;
                    }
                },

                // SHR matching for register destination (1,2,4,8) and immediate value
                instructions::Instruction::SHR {
                    destination,
                    source,
                } => match source.clone() {
                    ValueOperand::Immediate { i } => match destination.size.to_owned() {
                        1 => {
                            assembler.shr(
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
                            assembler.shr(
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
                            assembler.shr(
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
                            assembler.shr(
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
                            "shr source {:?} is not an immediate operand",
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
                instructions::Instruction::NOTreg { destination } => {
                    match destination.size.clone() {
                        1 => {
                            assembler.not(
                                gpr8::get_gpr8(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        2 => {
                            assembler.not(
                                gpr16::get_gpr16(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        4 => {
                            assembler.not(
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        8 => {
                            assembler.not(
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", destination.size))?;
                        }
                    }
                }
                // AND instruction with two operands: register and immediate operand, e.g. and eax, 0xf
                instructions::Instruction::AND {
                    destination,
                    source,
                } => match source {
                    ValueOperand::Immediate { i } => match destination.size.clone() {
                        1 => {
                            assembler.and(
                                gpr8::get_gpr8(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        2 => {
                            assembler.and(
                                gpr16::get_gpr16(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        4 => {
                            assembler.and(
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        8 => {
                            assembler.and(
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", destination.size))?;
                        }
                    },
                    _ => {
                        strerror(format!("and: second source operand is not an immediate"))?;
                    }
                },
                instructions::Instruction::IMUL {
                    destination,
                    source,
                } => match source.clone() {
                    ValueOperand::Register { r } => match destination.size.clone() {
                        4 => {
                            assembler.imul_2(
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
                            assembler.imul_2(
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
                    ValueOperand::Immediate { i } => match destination.size.clone() {
                        4 => {
                            assembler.imul_3(
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        8 => {
                            assembler.imul_3(
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination.name)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", destination.size))?;
                        }
                    },
                    _ => {
                        strerror(format!("imul: second source operand is not a register"))?;
                    }
                },
                instructions::Instruction::IDIV { source } => match source.clone() {
                    ValueOperand::Memory { label, size } => match size.clone() {
                        8 => {
                            assembler.idiv(qword_ptr(labeled_mem_operand(label, size)?))?;
                        }
                        _ => {
                            strerror(format!("Invalid mem operand size {:?}", size))?;
                        }
                    },
                    _ => {
                        strerror(format!("idiv: source operand is not a memory operand"))?;
                    }
                },
                instructions::Instruction::CMP {
                    src1: register,
                    src2,
                } => match src2 {
                    ValueOperand::Immediate { i } => match register.size.clone() {
                        1 => {
                            assembler.cmp(
                                gpr8::get_gpr8(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        2 => {
                            assembler.cmp(
                                gpr16::get_gpr16(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        4 => {
                            assembler.cmp(
                                gpr32::get_gpr32(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        8 => {
                            assembler.cmp(
                                gpr64::get_gpr64(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                i.clone() as i32,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", register.size))?;
                        }
                    },
                    ValueOperand::Register { r } => match register.size.clone() {
                        1 => {
                            assembler.cmp(
                                gpr8::get_gpr8(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                gpr8::get_gpr8(REGISTERS.get(r.name.as_str()).unwrap().to_owned())
                                    .ok_or(
                                        format!("Could not get 8-bit register {:?}", r).to_string(),
                                    )?,
                            )?;
                        }
                        2 => {
                            assembler.cmp(
                                gpr16::get_gpr16(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", register)
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
                            assembler.cmp(
                                gpr32::get_gpr32(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", register)
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
                            assembler.cmp(
                                gpr64::get_gpr64(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", register)
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
                            strerror(format!("Invalid register size {:?}", register.size))?;
                        }
                    },
                    ValueOperand::Memory { label, size } => match register.size.clone() {
                        1 => {
                            assembler.cmp(
                                gpr8::get_gpr8(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                byte_ptr(labeled_mem_operand(label.clone(), size.clone())?),
                            )?;
                        }
                        2 => {
                            assembler.cmp(
                                gpr16::get_gpr16(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                word_ptr(labeled_mem_operand(label.clone(), size.clone())?),
                            )?;
                        }
                        4 => {
                            assembler.cmp(
                                gpr32::get_gpr32(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                dword_ptr(labeled_mem_operand(label.clone(), size.clone())?),
                            )?;
                        }
                        8 => {
                            assembler.cmp(
                                gpr64::get_gpr64(
                                    REGISTERS.get(register.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", register)
                                        .to_string(),
                                )?,
                                qword_ptr(labeled_mem_operand(label.clone(), size.clone())?),
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", register.size))?;
                        }
                    },
                    _ => {
                        strerror(format!(
                            "cmp: second source operand is not a register or immediate"
                        ))?;
                    }
                },
                instructions::Instruction::XORreg {
                    destination,
                    source,
                } => {
                    if destination.size != source.size {
                        strerror(format!("XOR: operand sizes do not match"))?;
                    }

                    match destination.size.clone() {
                        1 => {
                            assembler.xor(
                                gpr8::get_gpr8(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", destination)
                                        .to_string(),
                                )?,
                                gpr8::get_gpr8(
                                    REGISTERS.get(source.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 8-bit register {:?}", source)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        2 => {
                            assembler.xor(
                                gpr16::get_gpr16(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", destination)
                                        .to_string(),
                                )?,
                                gpr16::get_gpr16(
                                    REGISTERS.get(source.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", source)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        4 => {
                            assembler.xor(
                                gpr32::get_gpr32(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", destination)
                                        .to_string(),
                                )?,
                                gpr32::get_gpr32(
                                    REGISTERS.get(source.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", source)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        8 => {
                            assembler.xor(
                                gpr64::get_gpr64(
                                    REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", destination)
                                        .to_string(),
                                )?,
                                gpr64::get_gpr64(
                                    REGISTERS.get(source.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", source)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        _ => {
                            strerror(format!("Invalid register size {:?}", destination.size))?;
                        }
                    }
                }
                instructions::Instruction::PUSH { source } => match source {
                    ValueOperand::Register { r } => match r.size.clone() {
                        2 => {
                            assembler.push(
                                gpr16::get_gpr16(
                                    REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", r.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        4 => {
                            assembler.push(
                                gpr32::get_gpr32(
                                    REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", r.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        8 => {
                            assembler.push(
                                gpr64::get_gpr64(
                                    REGISTERS.get(r.name.as_str()).unwrap().to_owned(),
                                )
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", r.name)
                                        .to_string(),
                                )?,
                            )?;
                        }
                        _ => {
                            strerror(format!("push: Invalid register size {:?}", r.size))?;
                        }
                    },
                    // Maybe immediates should also be supported
                    _ => {
                        strerror(format!("push: source operand is not a register"))?;
                    }
                },
                instructions::Instruction::POP { destination: r } => match r.size.clone() {
                    2 => {
                        assembler.pop(
                            gpr16::get_gpr16(REGISTERS.get(r.name.as_str()).unwrap().to_owned())
                                .ok_or(
                                    format!("Could not get 16-bit register {:?}", r.name)
                                        .to_string(),
                                )?,
                        )?;
                    }
                    4 => {
                        assembler.pop(
                            gpr32::get_gpr32(REGISTERS.get(r.name.as_str()).unwrap().to_owned())
                                .ok_or(
                                    format!("Could not get 32-bit register {:?}", r.name)
                                        .to_string(),
                                )?,
                        )?;
                    }
                    8 => {
                        assembler.pop(
                            gpr64::get_gpr64(REGISTERS.get(r.name.as_str()).unwrap().to_owned())
                                .ok_or(
                                    format!("Could not get 64-bit register {:?}", r.name)
                                        .to_string(),
                                )?,
                        )?;
                    }
                    _ => {
                        strerror(format!("pop: Invalid register size {:?}", r.size))?;
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
                        JumpTarget::Relative { label, forwards } => {
                            if forwards.to_owned() {
                                // Find the index of the label in the instruction stream following after current index
                                let label_index = _idx
                                    + input
                                        .parsed_lines
                                        .iter()
                                        .skip(_idx)
                                        .position(|x| match x {
                                            LineType::Label {
                                                l:
                                                    JumpTarget::Relative {
                                                        forwards: _,
                                                        label: l,
                                                    },
                                            } => label.to_owned() == l.to_owned(),
                                            _ => false,
                                        })
                                        .ok_or(format!("Could not find relative forward jump label {:?}", label))?;

                                // Insert this into the map
                                let new_label = Cell::new(assembler.create_label());
                                relative_labels_map.insert(label_index, new_label.to_owned());

                                new_label
                            } else {
                                panic!("Backwards jumps not yet supported");
                            }
                        }
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
                instructions::Instruction::LEA {
                    destination,
                    source,
                } => match source {
                    ValueOperand::Memory { label, size: _ } => {
                        if destination.size != 8 {
                            strerror(format!(
                                "lea: Invalid register size {:?}, must be 8",
                                destination.size
                            ))?;
                        }
                        assembler.mov(
                            gpr64::get_gpr64(
                                REGISTERS.get(destination.name.as_str()).unwrap().to_owned(),
                            )
                            .ok_or(
                                format!("Could not get 64-bit register {:?}", destination)
                                    .to_string(),
                            )?,
                            labeled_mem_operand(label.to_owned(), 0)? as u64,
                        )?;
                    }
                    _ => {
                        strerror(format!("unsupported lea source operand: {:?}", source))?;
                    }
                },
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
                instructions::Instruction::NOP {} => {
                    assembler.nop()?;
                }
                instructions::Instruction::INT3 {} => {
                    assembler.int3()?;
                }
                instructions::Instruction::CQO {} => {
                    assembler.cqo()?;
                }
                _ => {
                    strerror(format!("unsupported instruction: {:?}", i).to_string())?;
                }
            },
        }
    }

    let result = assembler.assemble_options(
        instr_start_address,
        BlockEncoderOptions::RETURN_NEW_INSTRUCTION_OFFSETS,
    )?;

    // Now get the entry point start address
    let entrypoint = match entrypoint_name {
        Some(name) => match named_labels.get(&name) {
            Some(l) => result.label_ip(&l.get())?,
            None => {
                return Err(EncodeError::StrError {
                    e: format!("Could not find entrypoint label: {:?}", name).to_string(),
                });
            }
        },
        None => 0,
    };

    let size = data_section.iter().map(|x| u64::from(x.size)).sum();
    return Ok(EncodeResult {
        code: result.inner.code_buffer,
        code_start_address: result.inner.rip,
        entrypoint_address: entrypoint,
        data_start_address,
        data_section: data_section,
        data_section_size: size,
    });
}

#[cfg(test)]
mod test_sanity {
    // There is some kind of bug where this encoding isn't correct.
    // This test is here to make sure that it's a fault of my own code, not that of iced
    use iced_x86::{code_asm::*, Register};

    #[test]
    fn iced_returns_correct_bytes() {
        let mut assembler = CodeAssembler::new(64).unwrap();
        assembler
            .mov(
                gpr64::get_gpr64(Register::RBX).unwrap(),
                gpr64::get_gpr64(Register::RDI).unwrap(),
            )
            .unwrap();

        let result = assembler.assemble(0).unwrap();

        assert_eq!(result, vec![0x48, 0x89, 0xfb]);
    }

    #[test]
    fn iced_returns_correct_bytes2() {
        let mut assembler = CodeAssembler::new(64).unwrap();
        assembler
            .mov(
                gpr64::get_gpr64(Register::RBX).unwrap(),
                gpr64::get_gpr64(Register::RBX).unwrap(),
            )
            .unwrap();

        let result = assembler.assemble(0).unwrap();

        assert_eq!(result, vec![0x48, 0x89, 0xdb]);
    }
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
            None,
        );
        assert!(encode_res.is_ok(), "{}", encode_res.err().unwrap());

        assert_eq!(encode_res.unwrap().code, u);
    }

    #[test]
    fn entrypoint_addr() {
        let input_lines: Vec<input::LineType> = vec![
            input::LineType::Label {
                l: JumpTarget::Absolute {
                    label: "some_function".to_owned(),
                },
            },
            input::LineType::Instruction {
                i: Instruction::RET {},
            },
            input::LineType::Label {
                l: JumpTarget::Absolute {
                    label: "actual_entrypoint".to_owned(),
                },
            },
            input::LineType::Instruction {
                i: Instruction::RET {},
            },
        ];

        let encode_res = encode_file(
            crate::parser::input::InputFile {
                parsed_lines: input_lines,
            },
            0x2000,
            0x1000,
            Some("actual_entrypoint".to_owned()),
        );
        assert!(encode_res.is_ok(), "{}", encode_res.err().unwrap());

        let res = encode_res.unwrap();

        assert_eq!(res.code_start_address, 0x2000);
        // Offset by one because "ret" instruction is one byte
        assert_eq!(res.entrypoint_address, 0x2001);
        assert_eq!(res.data_start_address, 0x1000);
    }

    #[test]
    fn mov_deref_null() {
        assert_encoding(
            vec![Instruction::MOV {
                destination: ValueOperand::Register {
                    r: Register {
                        name: "RAX".to_string(),
                        size: 8,
                        part_of: registers::GPRegister::RAX,
                    },
                },
                source: ValueOperand::DirectMemory { i: 15 },
            }],
            vec![72, 161, 15, 0, 0, 0, 0, 0, 0, 0],
        );
    }

    #[test]
    fn mov_reg_encoding() {
        assert_encoding(
            vec![Instruction::MOV {
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
            }],
            vec![0x48, 0x89, 0xfb],
        );
    }

    #[test]
    fn reg_imm_shifts() {
        assert_encoding_lines(
            vec![
                // shr ebx, 1
                LineType::Instruction {
                    i: Instruction::SHR {
                        destination: Register {
                            name: "EBX".to_string(),
                            size: 4,
                            part_of: registers::GPRegister::RBX,
                        },
                        source: ValueOperand::Immediate { i: 1 },
                    },
                },
            ],
            vec![0xd1, 0xeb],
        );

        assert_encoding_lines(
            vec![
                // shl rax, 1
                LineType::Instruction {
                    i: Instruction::SHL {
                        destination: Register {
                            name: "RAX".to_string(),
                            size: 8,
                            part_of: registers::GPRegister::RAX,
                        },
                        source: ValueOperand::Immediate { i: 1 },
                    },
                },
            ],
            vec![0x48, 0xd1, 0xe0],
        );
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
                0x48, 0x89, 0xfb, // MOV RBX, RDI
                0xeb, 0xfb, // JMP label
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
    fn and_reg_imm() {
        assert_encoding(
            vec![Instruction::AND {
                destination: Register {
                    name: "RAX".to_string(),
                    size: 8,
                    part_of: registers::GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 0x42 },
            }],
            vec![0x48, 37, 66, 0, 0, 0],
        );

        assert_encoding(
            vec![Instruction::AND {
                destination: Register {
                    name: "EAX".to_string(),
                    size: 4,
                    part_of: registers::GPRegister::RAX,
                },
                source: ValueOperand::Immediate { i: 15 },
            }],
            vec![37, 15, 0, 0, 0],
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

        let encode_res = encode_file(result.unwrap(), 0x2000, 0x1000, None);
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
