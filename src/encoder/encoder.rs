use phf::phf_map;
use std::{cell::Cell, collections::HashMap};

use iced_x86::{
    BlockEncoder, BlockEncoderOptions, Code, IcedError, Instruction, InstructionBlock,
    MemoryOperand, Register,
};
use lazy_static::lazy_static;

use crate::parser::{
    input::{InputFile, LineType},
    instructions,
    instructions::ValueOperand,
    registers,
};

fn iced_err_to_string(e: IcedError) -> String {
    return format!("{}", e).to_string();
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
) -> Result<EncodeResult, String> {
    let instructions: &mut Vec<Instruction> = &mut vec![];

    // Maps a label name to an instruction index
    let mut label_map: HashMap<String, usize> = HashMap::new();

    let mut add = |i: Result<Instruction, IcedError>| -> Result<(), String> {
        instructions.push(i.map_err(iced_err_to_string)?);
        Ok(())
    };

    // data_section maps a data label name to its offset and data size
    let mut data_section: Vec<(String, i64, i8)> = vec![];
    let mut next_data_section_offset = 0;

    let mut labeled_mem_operand = |label: String, size: i8| -> Result<MemoryOperand, String> {
        match data_section.iter().find(|(l, _, _)| label.eq(l)) {
            Some((_, offset, s)) => {
                if size.eq(s) {
                    Ok(MemoryOperand::with_displ(
                        data_start_address + offset.clone() as u64,
                        64,
                    ))
                } else {
                    Err(
                        format!("data label {} has size {} but expected {}", label, s, size)
                            .to_string(),
                    )
                }
            }
            None => {
                let next = MemoryOperand::with_displ(
                    data_start_address + next_data_section_offset as u64,
                    64,
                );
                data_section.push((label, next_data_section_offset, size));
                next_data_section_offset += i64::from(size);

                Ok(next)
            }
        }
    };

    for (idx, line) in input.parsed_lines.iter().enumerate() {
        match line {
            LineType::Label { name } => match label_map.get(name) {
                Some(_) => {
                    return Err(format!("Label {} defined twice", name).to_string());
                }
                None => {
                    // We haven't seen this label before, so we create a new one (pointing to the next line)
                    label_map.insert(name.clone(), idx + 1);
                }
            },
            LineType::Instruction { i } => match i {
                instructions::Instruction::MOV {
                    destination,
                    source,
                } => match (destination, source) {
                    (ValueOperand::Register { r: dst }, ValueOperand::Register { r: src }) => {
                        add(Instruction::with2(
                            Code::Mov_r64_rm64,
                            REGISTERS.get(&dst.name.as_str()).unwrap().clone(),
                            REGISTERS.get(&src.name.as_str()).unwrap().clone(),
                        ))?;
                    }
                    (ValueOperand::Register { r: dst }, ValueOperand::Immediate { i }) => {
                        add(Instruction::with2(
                            Code::Mov_r64_imm64,
                            REGISTERS.get(&dst.name.as_str()).unwrap().clone(),
                            i.clone(),
                        ))?;
                    }
                    (ValueOperand::Memory { label, size }, ValueOperand::Immediate { i }) => {
                        match size.clone() {
                            1 => add(Instruction::with2(
                                Code::Mov_rm8_imm8,
                                labeled_mem_operand(label.clone(), size.clone())?,
                                *i as i32,
                            ))?,
                            2 => add(Instruction::with2(
                                Code::Mov_rm16_imm16,
                                labeled_mem_operand(label.clone(), size.clone())?,
                                *i as i32,
                            ))?,
                            4 => add(Instruction::with2(
                                Code::Mov_rm32_imm32,
                                labeled_mem_operand(label.clone(), size.clone())?,
                                *i as i32,
                            ))?,
                            _ => {
                                return Err(format!(
                                    "Unsupported immediate size {} for MOV",
                                    size.clone()
                                )
                                .to_string());
                            }
                        }
                    }
                    (ValueOperand::Memory { label, size }, ValueOperand::Register { r }) => {
                        match r.size {
                            1 => {
                                add(Instruction::with2(
                                    Code::Mov_rm8_r8,
                                    labeled_mem_operand(label.clone(), size.clone())?,
                                    REGISTERS.get(&r.name.as_str()).unwrap().clone(),
                                ))?;
                            }
                            2 => {
                                add(Instruction::with2(
                                    Code::Mov_rm16_r16,
                                    labeled_mem_operand(label.clone(), size.clone())?,
                                    REGISTERS.get(&r.name.as_str()).unwrap().clone(),
                                ))?;
                            }
                            4 => {
                                add(Instruction::with2(
                                    Code::Mov_rm32_r32,
                                    labeled_mem_operand(label.clone(), size.clone())?,
                                    REGISTERS.get(&r.name.as_str()).unwrap().clone(),
                                ))?;
                            }
                            8 => add(Instruction::with2(
                                Code::Mov_rm64_r64,
                                labeled_mem_operand(label.clone(), size.clone())?,
                                REGISTERS.get(&r.name.as_str()).unwrap().clone(),
                            ))?,
                            _ => {
                                return Err(format!(
                                    "Unsupported register size {} for MOV",
                                    r.size
                                )
                                .to_string());
                            }
                        };
                    }
                    _ => {
                        return Err(format!(
                            "invalid combination of operands for mov: {:?} and {:?}",
                            destination, source
                        )
                        .to_string());
                    }
                },
                // Test instruction for r64, imm64
                instructions::Instruction::TEST { src1, src2 } => match (src1, src2) {
                    (dst, ValueOperand::Register { r: src }) => {
                        add(Instruction::with2(
                            Code::Test_rm64_r64,
                            REGISTERS.get(&dst.name.as_str()).unwrap().clone(),
                            REGISTERS.get(&src.name.as_str()).unwrap().clone(),
                        ))?;
                    }
                    (dst, ValueOperand::Immediate { i }) => {
                        add(Instruction::with2(
                            Code::Test_rm64_imm32,
                            REGISTERS.get(&dst.name.as_str()).unwrap().clone(),
                            i.clone(),
                        ))?;
                    }
                    _ => {
                        return Err(format!(
                            "invalid combination of operands for test: {:?} and {:?}",
                            src1, src2
                        )
                        .to_string());
                    }
                },

                // TODO: Figure out how to reference labels, so they only get turned into addresses at the end
                // instructions::Instruction::CALLlabel { label } => {
                //     Instruction::with1(Code::Call, target)
                // }
                instructions::Instruction::SYSCALL {} => {
                    add(Ok(Instruction::with(Code::Syscall)))?;
                }
                instructions::Instruction::RET {} => {
                    add(Ok(Instruction::with(Code::Retnq)))?;
                }
                _ => {
                    panic!("unimplemented instruction {:?}", line)
                }
            },
        }
    }

    let block = InstructionBlock::new(&instructions, instr_start_address);

    let result =
        BlockEncoder::encode(64, block, BlockEncoderOptions::NONE).map_err(iced_err_to_string)?;

    return Ok(EncodeResult {
        code: result.code_buffer,
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
            input,
            instructions::{Instruction, ValueOperand},
            registers,
        },
    };

    fn assert_encoding(i: Vec<Instruction>, u: Vec<u8>) {
        let input_lines: Vec<input::LineType> = i
            .iter()
            .map(|instr: &Instruction| input::LineType::Instruction { i: instr.clone() })
            .collect();

        let encode_res = encode_file(
            crate::parser::input::InputFile {
                parsed_lines: input_lines,
            },
            0x2000,
            0x1000,
        );
        assert!(encode_res.is_ok(), "{}", encode_res.err().unwrap());

        assert_eq!(encode_res.unwrap().code, u);
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
