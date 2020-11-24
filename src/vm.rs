use crate::chunk::{Chunk, OpCode};
use crate::debug::disassemble_instruction;
use crate::debugln;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: [u8;256],
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM{ chunk: Chunk::new(), ip: 0, stack: [0; 256] }
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    pub fn run(&mut self) -> InterpretResult {
        let mut result: Option<InterpretResult> = None;
        debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
        loop {
            disassemble_instruction(&self.chunk, self.ip);
            let instruction = self.read_byte(); 
            match OpCode::from(instruction) {
                OpCode::OP_RETURN => {
                    result = Some(INTERPRET_OK);
                },
                OpCode::OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte() as usize;
                    debugln!("===VM EXECUTING CONSTANT===::{}", self.chunk.constant_pool[constant_addr]);
                },
                OpCode::OP_UNKNOWN => {
                    result = Some(INTERPRET_COMPILE_ERROR);
                }
            }
            if let Some(r) = result {
                return r;
            }
        }
    }
}
