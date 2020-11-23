use rlox::debug::disassemble_chunk;
use rlox::chunk::{OpCode, Chunk};
fn main() {
    let mut chunk = Chunk::new();
    chunk.code.push(OpCode::OP_RETURN.into());
    chunk.lines.push(1);
    disassemble_chunk(&chunk, "Test");
}
