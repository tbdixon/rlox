use crate::chunk::{Chunk, OpCode};
use crate::{debug, debugln};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    debugln!("== {} ==", name);
    debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
    let mut offset: usize = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(&chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    debug!("{:04}\t{}\t", offset, chunk.lines[offset]);
    match OpCode::from(chunk.code[offset]) {
        OpCode::OP_RETURN => simple_instruction(OpCode::OP_RETURN, offset),
        OpCode::OP_CONSTANT => constant_instruction(OpCode::OP_CONSTANT, chunk, offset),
        OpCode::OP_UNKNOWN => {
            debugln!("Unknown Opcode Encountered");
            offset + 1
        }
    }
}

pub fn simple_instruction(op_code: OpCode, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    debugln!("{:?} ({:#04X?})", op_code, binary_code);
    offset + 1
}

pub fn constant_instruction(op_code: OpCode, chunk: &Chunk, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    let constant_addr: usize = chunk.code[offset+1] as usize;
    let constant_val: f64 = chunk.constant_pool[constant_addr];
    debugln!("{:?} ({:#04X?})\t{:#04X?}\t{} ", op_code, binary_code, constant_addr, constant_val);
    offset + 2
}
