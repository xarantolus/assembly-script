use std::collections::HashMap;

use crate::parser::{
    input::{InputFile, LineType::Label},
    registers::GPRegister,
};

extern crate rand;

pub struct VM {
    // The program to run and the index of the next instruction to run
    program: InputFile,
    current_program_ptr: usize,

    // The stack starts at max_stack_size; any push will decrement the stack pointer and any pop will increment the stack pointer
    stack: Vec<u8>,
    max_stack_size: i64,
    stack_start_addr: i64,

    // For memory accesses of the form [rip+.Llabel]
    data: HashMap<String, MemItem>,

    // Register values
    registers: HashMap<GPRegister, i64>,

    syscall_handler: SyscallHandler,
}

pub struct SyscallHandler {
    pub write_stdout: fn(&str),
    pub write_stderr: fn(&str),
}

struct MemItem {
    data: i64,
    size: i8,
}

impl VM {
    pub fn new(
        program: InputFile,
        entrypoint: String,
        stack_size: i64,
        syscalls: SyscallHandler,
    ) -> Result<Self, String> {
        let start_ptr = program
            .parsed_lines
            .iter()
            .position(|l| match &l {
                Label { name: ep } => entrypoint.eq(ep),
                _ => false,
            })
            .ok_or(format!("could not find entry symbol {}", entrypoint).to_string())?;

        // Choose a page (4096 byte) aligned stack start address
        let mut stack_start_addr: i64 = rand::random();
        stack_start_addr &= 0xfffff000;

        let mut registers: HashMap<GPRegister, i64> = HashMap::new();
        registers.insert(GPRegister::RSP, stack_start_addr + 8);

        Ok(VM {
            current_program_ptr: start_ptr,
            program: program,

            stack: vec![
                // This is the initial return address on the stack of the program;
                // If this gets popped from the stack (by a ret instruction) the program ends;
                0, 0, 0, 0, 0, 0, 0, 0,
            ],
            max_stack_size: stack_size,
            stack_start_addr: stack_start_addr,

            data: HashMap::new(),

            registers: registers,
            syscall_handler: syscalls,
        })
    }
}
