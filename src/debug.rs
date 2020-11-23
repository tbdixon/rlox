use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset: usize = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(&chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:#04X?}\t", offset);
    match OpCode::from(chunk.code[offset]) {
        OpCode::OP_RETURN => simple_instruction("OP_RETURN", offset),
        _ => offset + 1
    }
}

pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}
