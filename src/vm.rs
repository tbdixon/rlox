use crate::chunk::{Chunk, OpCode, Value};
use crate::compiler::compile;
use crate::debug::disassemble_instruction;
use crate::debugln;
use std::mem::discriminant;

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

// TODO
//const MAX_STACK_SIZE: usize = 256;
// 1/ Sort out how to limit size of stack when it is a Vec
// 2/ Is using unwrap() acceptable on Vec operations here since a None
// would imply stack over/underflow or other such error? What's the best way
// to more gracefully handle this result and return a runtime error?
// 3/ Do I need to manually call drop() ever on elements of the stack?
pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Vec<Value>,
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            stack: Vec::new(), 
            ip: 0,
        }
    }

    fn print_stack(&self) {
        print!("Current Stack: ");
        for val in &self.stack {
            print!("[");
            print!("{:?}", val);
            print!("]");
        }
        println!();
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left + right))
            }
            (Value::Str(left), Value::Str(right)) => {
                self.stack.push(Value::Str(left+&right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_sub(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left - right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_mult(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left * right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_div(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left / right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn negate(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop().unwrap();
        match val {
            Value::Number(val) => self.stack.push(Value::Number(-val)),
            Value::Nil() => self.stack.push(val),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Operator must be a number.")),
        };
        Ok(INTERPRET_OK)
    }

    fn not(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop().unwrap();
        self.push(match val {
            Value::Nil() => Value::Bool(true),
            Value::Bool(bool) => Value::Bool(!bool),
            _ => Value::Bool(false),
        })
    }

    fn equal(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.push(Value::Bool(
            discriminant(&left) == discriminant(&right) && left == right,
        ))  
    }

    fn greater(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.push(Value::Bool(left > right)),
            _ => {
                return Err(INTERPRET_RUNTIME_ERROR(
                    "Comparison operands must be numbers.",
                ))
            }
        }
    }
    fn less(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.push(Value::Bool(left < right)),
            _ => {
                return Err(INTERPRET_RUNTIME_ERROR(
                    "Comparison operands must be numbers.",
                ))
            }
        }
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
                if !self.stack.is_empty() {
                    self.print_stack();
                }
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match OpCode::from(instruction) {
                OpCode::OP_RETURN => {
                    if !self.stack.is_empty() {
                        println!("{:?}", self.stack.pop());
                    }
                    return Ok(INTERPRET_OK);
                }
                OpCode::OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte() as usize;
                    let constant = self.chunk.constant_pool[constant_addr].clone();
                    self.stack.push(constant);
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
                OpCode::OP_EQUAL => self.equal(),
                OpCode::OP_GREATER => self.greater(),
                OpCode::OP_LESS => self.less(),
                OpCode::OP_UNKNOWN => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
    }
}
