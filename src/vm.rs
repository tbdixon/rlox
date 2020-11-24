use crate::chunk::{Chunk, OpCode};
use crate::debug::disassemble_instruction;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM{ chunk: Chunk::new(), ip: 0 }
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    pub fn run(&mut self) -> InterpretResult {
        let mut result: Option<InterpretResult> = None;
        println!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
        loop {
            disassemble_instruction(&self.chunk, self.ip);
            let instruction = self.read_byte(); 
            match OpCode::from(instruction) {
                OpCode::OP_RETURN => {
                    result = Some(INTERPRET_OK);
                },
                OpCode::OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte() as usize;
                    println!("===VM EXECUTING CONSTANT===\n{}", self.chunk.constant_pool[constant_addr]);
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
