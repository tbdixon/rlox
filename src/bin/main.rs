use rlox::chunk::{OpCode, Chunk};
use rlox::vm::VM;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::OP_CONSTANT.into(), 2);
    chunk.write(constant as u8, 2);
    chunk.write(OpCode::OP_RETURN.into(), 3);
    vm.chunk = chunk;
    vm.run();
}
