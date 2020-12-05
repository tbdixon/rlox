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
        OpCode::OP_NEGATE => simple_instruction(OpCode::OP_NEGATE, offset),
        OpCode::OP_ADD => simple_instruction(OpCode::OP_ADD, offset),
        OpCode::OP_SUBTRACT => simple_instruction(OpCode::OP_SUBTRACT, offset),
        OpCode::OP_MULTIPLY => simple_instruction(OpCode::OP_MULTIPLY, offset),
        OpCode::OP_DIVIDE => simple_instruction(OpCode::OP_DIVIDE, offset),
        OpCode::OP_NIL => simple_instruction(OpCode::OP_NIL, offset),
        OpCode::OP_TRUE => simple_instruction(OpCode::OP_TRUE, offset),
        OpCode::OP_FALSE => simple_instruction(OpCode::OP_FALSE, offset),
        OpCode::OP_NOT => simple_instruction(OpCode::OP_NOT, offset),
        OpCode::OP_EQUAL => simple_instruction(OpCode::OP_EQUAL, offset),
        OpCode::OP_GREATER => simple_instruction(OpCode::OP_GREATER, offset),
        OpCode::OP_LESS => simple_instruction(OpCode::OP_LESS, offset),
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
    let constant_addr: usize = chunk.code[offset + 1] as usize;
    let constant_val = &chunk.constant_pool[constant_addr];
    debugln!(
        "{:?} ({:#04X?})\t{:#04X?}\t{:?} ",
        op_code,
        binary_code,
        constant_addr,
        constant_val
    );
    offset + 2
}
