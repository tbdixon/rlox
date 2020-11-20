use rlox::operand;
use rlox::chunk::{OpCode, Instruction};
use std::mem::size_of;
fn main() {
    let instruction = Instruction::new(OpCode::OP_CONSTANT, operand![23], 0);
    let s = size_of::<Instruction>();
    println!("Hello, instruction! {:#04X?} of size {}", instruction, s);
}
