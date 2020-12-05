use crate::chunk::{Chunk, OpCode, Value};
use crate::compiler::compile;
use crate::debug::disassemble_instruction;
use crate::debugln;

type Result<T> = std::result::Result<T, InterpretResult>;

#[derive(PartialEq, Debug)]
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR(&'static str),
}
impl InterpretResult {
    //TODO: Implement std::fmt for InterpretResult
    fn runtime_error(&self, msg: &str) {
        println!("Error {:?}! {}", self, msg);
    }
}

const MAX_STACK_SIZE: usize = 256;
pub struct Stack {
    pub stack: [Value; MAX_STACK_SIZE],
    top: usize,
}

impl Stack {
    fn new() -> Stack {
        Stack {
            stack: [Value::Nil(); MAX_STACK_SIZE],
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

    pub fn reset(&mut self) {
        self.chunk = Chunk::new();
        self.ip = 0;
        self.stack = Stack::new();
    }
    fn print_stack(&self) {
        print!("Current Stack: ");
        for idx in 0..self.stack.top {
            print!("[");
            print!("{:?}", self.stack.get(idx));
            print!("]");
        }
        println!();
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left + right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_sub(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left - right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_mult(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left * right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_div(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left / right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn negate(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop();
        match val {
            Value::Number(val) => self.stack.push(Value::Number(-val)),
            Value::Nil() => self.stack.push(val),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Operator must be a number.")),
        };
        Ok(INTERPRET_OK)
    }

    fn not(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop();
        self.push(match val {
            Value::Nil() => Value::Bool(true),
            Value::Bool(bool) => Value::Bool(!bool),
            _ => Value::Bool(false),
        })
    }

    fn push(&mut self, val: Value) -> Result<InterpretResult> {
        self.stack.push(val);
        Ok(INTERPRET_OK)
    }

    pub fn interpret(&mut self, source: &str) {
        // Print parsing errors during compilation, so if the result is not Ok
        // just return and finish the VM process.
        if let Ok(_) = compile(source, &mut self.chunk) {
            match self.run() {
                Ok(_) => {}
                Err(e) => e.runtime_error(
                    &format!("[line {}] in script\n", self.chunk.lines[self.ip - 1])[..],
                ),
            }
        }
    }

    fn run(&mut self) -> Result<InterpretResult> {
        debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
        loop {
            if crate::DEBUG {
                if self.stack.top > 0 {
                    self.print_stack();
                }
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match OpCode::from(instruction) {
                OpCode::OP_RETURN => {
                    if self.stack.top > 0 {
                        println!("{:?}", self.stack.pop());
                    }
                    return Ok(INTERPRET_OK);
                }
                OpCode::OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte() as usize;
                    self.stack.push(self.chunk.constant_pool[constant_addr]);
                    Ok(INTERPRET_OK)
                }
                OpCode::OP_NEGATE => self.negate(),
                OpCode::OP_ADD => self.binary_add(),
                OpCode::OP_MULTIPLY => self.binary_mult(),
                OpCode::OP_SUBTRACT => self.binary_sub(),
                OpCode::OP_DIVIDE => self.binary_div(),
                OpCode::OP_NIL => self.push(Value::Nil()),
                OpCode::OP_TRUE => self.push(Value::Bool(true)),
                OpCode::OP_FALSE => self.push(Value::Bool(false)),
                OpCode::OP_NOT => self.not(),
                //                OpCode::OP_EQUAL => simple_instruction(OpCode::OP_EQUAL, offset),
                //                OpCode::OP_GREATER => simple_instruction(OpCode::OP_GREATER, offset),
                //                OpCode::OP_LESS => simple_instruction(OpCode::OP_LESS, offset),
                OpCode::OP_UNKNOWN | _ => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
    }
}
