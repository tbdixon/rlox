use crate::chunk::{Chunk, OpCode, Value};
use crate::debug::disassemble_instruction;
use crate::debugln;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

const MAX_STACK_SIZE: usize = 256;
pub struct Stack {
    pub stack: [Value; MAX_STACK_SIZE],
    top: usize,
}

impl Stack {
    fn new() -> Stack {
        Stack {
            stack: [0.0; MAX_STACK_SIZE],
            top: 0,
        }
    }

    fn get(&self, idx: usize) -> Value {
        self.stack[idx]
    }

    fn push(&mut self, val: Value) {
        self.stack[self.top] = val;
        self.top += 1;
    }

    fn pop(&mut self) -> Value {
        self.top -= 1;
        self.stack[self.top]
    }
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Stack,
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Stack::new(),
        }
    }

    fn print_stack(&self) {
        for idx in 0..self.stack.top {
            print!("[");
            print!("{}", self.stack.get(idx));
            print!("]");
        }
        println!();
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    fn binary_op(&mut self, operator: &dyn Fn(Value, Value) -> Value) {
        let right = self.stack.pop();
        let left = self.stack.pop();
        self.stack.push(operator(left, right));
    }

    pub fn run(&mut self) -> InterpretResult {
        let mut result: Option<InterpretResult> = None;
        debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
        loop {
            if crate::DEBUG {
                self.print_stack();
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match OpCode::from(instruction) {
                OpCode::OP_RETURN => {
                    println!("{}", self.stack.pop());
                    result = Some(INTERPRET_OK);
                }
                OpCode::OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte() as usize;
                    self.stack.push(self.chunk.constant_pool[constant_addr]);
                }
                OpCode::OP_NEGATE => {
                    let val = -self.stack.pop();
                    self.stack.push(val);
                }
                OpCode::OP_ADD => {
                    self.binary_op(&std::ops::Add::add);
                }
                OpCode::OP_MULTIPLY => {
                    self.binary_op(&std::ops::Mul::mul);
                }
                OpCode::OP_SUBTRACT => {
                    self.binary_op(&std::ops::Sub::sub);
                }
                OpCode::OP_DIVIDE => {
                    self.binary_op(&std::ops::Div::div);
                }
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
