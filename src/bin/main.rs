use rlox::chunk::{Chunk, OpCode};
use rlox::vm::VM;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::OP_CONSTANT.into(), 1);
    chunk.write(constant as u8, 1);
    let constant = chunk.add_constant(3.4);
    chunk.write(OpCode::OP_CONSTANT.into(), 2);
    chunk.write(constant as u8, 2);
    chunk.write(OpCode::OP_ADD.into(), 3);
    let constant = chunk.add_constant(5.6);
    chunk.write(OpCode::OP_CONSTANT.into(), 4);
    chunk.write(constant as u8, 4);
    chunk.write(OpCode::OP_DIVIDE.into(), 5);
    chunk.write(OpCode::OP_NEGATE.into(), 5);
    chunk.write(OpCode::OP_RETURN.into(), 4);
    vm.chunk = chunk;
    vm.run();
}
