use crate::chunk::Chunk;
use crate::chunk::OpCode::{self, *};
use crate::{debug, debugln};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    debugln!("== {} ==", name);
    debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
    let mut offset: usize = 0;
    println!("{:?}", chunk.code);
    while offset < chunk.code.len() {
        offset = disassemble_instruction(&chunk, offset);
    }
    debugln!("== {} ==", name);
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    debug!("{:04}\t{}\t", offset, chunk.lines[offset]);
    match OpCode::from(chunk.code[offset]) {
        OP_RETURN => simple_instruction(OP_RETURN, offset),
        OP_CONSTANT => constant_instruction(OP_CONSTANT, chunk, offset),
        OP_NEGATE => simple_instruction(OP_NEGATE, offset),
        OP_ADD => simple_instruction(OP_ADD, offset),
        OP_SUBTRACT => simple_instruction(OP_SUBTRACT, offset),
        OP_MULTIPLY => simple_instruction(OP_MULTIPLY, offset),
        OP_DIVIDE => simple_instruction(OP_DIVIDE, offset),
        OP_NIL => simple_instruction(OP_NIL, offset),
        OP_TRUE => simple_instruction(OP_TRUE, offset),
        OP_FALSE => simple_instruction(OP_FALSE, offset),
        OP_NOT => simple_instruction(OP_NOT, offset),
        OP_EQUAL => simple_instruction(OP_EQUAL, offset),
        OP_GREATER => simple_instruction(OP_GREATER, offset),
        OP_LESS => simple_instruction(OP_LESS, offset),
        OP_PRINT => simple_instruction(OP_PRINT, offset),
        OP_GET_GLOBAL => simple_instruction(OP_GET_GLOBAL, offset),
        OP_SET_GLOBAL => simple_instruction(OP_SET_GLOBAL, offset),
        OP_DEFINE_GLOBAL => simple_instruction(OP_DEFINE_GLOBAL, offset),
        OP_POP => simple_instruction(OP_POP, offset),
        OP_GET_LOCAL => byte_instruction(OP_GET_LOCAL, chunk, offset),
        OP_SET_LOCAL => byte_instruction(OP_SET_LOCAL, chunk, offset),
        OP_UNKNOWN => {
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

pub fn byte_instruction(op_code: OpCode, chunk: &Chunk, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    let byte_instr: usize = chunk.code[offset + 1] as usize;
    debugln!(
        "{:?} ({:#04X?})\t{:#04X?} ",
        op_code,
        binary_code,
        byte_instr,
    );
    offset + 2
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
