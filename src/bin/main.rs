use rlox::debug::disassemble_chunk;
use rlox::chunk::{OpCode, Chunk};
fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::OP_CONSTANT.into(), 2);
    chunk.write(constant as u8, 2);
    chunk.write(OpCode::OP_RETURN.into(), 3);
    disassemble_chunk(&chunk, "Test");
}
