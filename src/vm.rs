use crate::chunk::OpCode::{self, *};
use crate::chunk::{Chunk, Value};
use crate::compiler::compile;
use crate::debug::disassemble_instruction;
use crate::debugln;
use std::collections::HashMap;
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
    pub globals: HashMap<String, Value>,
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            ip: 0,
        }
    }

    fn print_stack(&self) {
        if !self.stack.is_empty() {
            print!("Current Stack: ");
            for val in &self.stack {
                print!("[");
                print!("{:?}", val);
                print!("]");
            }
            println!();
        }
    }

    fn print_globals(&self) {
        println!("{:?}", self.globals);
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    // Simple helper to easily use the '?' throughout the VM for some clutter cleanup
    // and error consolidation.
    fn pop_stack(&mut self) -> Result<Value> {
        self.stack
            .pop()
            .ok_or_else(|| INTERPRET_RUNTIME_ERROR("Empty stack"))
    }

    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left + right))
            }
            (Value::Str(left), Value::Str(right)) => self.stack.push(Value::Str(left + &right)),
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_sub(&mut self) -> Result<InterpretResult> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left - right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_mult(&mut self) -> Result<InterpretResult> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left * right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_div(&mut self) -> Result<InterpretResult> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left / right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn negate(&mut self) -> Result<InterpretResult> {
        let val = self.pop_stack()?;
        match val {
            Value::Number(val) => self.stack.push(Value::Number(-val)),
            Value::Nil() => self.stack.push(val),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Operator must be a number.")),
        };
        Ok(INTERPRET_OK)
    }

    fn not(&mut self) -> Result<InterpretResult> {
        let val = self.pop_stack()?;
        self.push(match val {
            Value::Nil() => Value::Bool(true),
            Value::Bool(bool) => Value::Bool(!bool),
            _ => Value::Bool(false),
        })
    }

    fn equal(&mut self) -> Result<InterpretResult> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        self.push(Value::Bool(
            discriminant(&left) == discriminant(&right) && left == right,
        ))
    }

    fn greater(&mut self) -> Result<InterpretResult> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
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
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
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

    fn print(&mut self) -> Result<InterpretResult> {
        println!("{}", self.pop_stack()?);
        Ok(INTERPRET_OK)
    }

    fn get_variable_name(&mut self) -> Result<String> {
        match self.pop_stack()? {
            Value::Str(v) => Ok(v),
            Value::Nil() | Value::Number(_) | Value::Bool(_) => {
                return Err(INTERPRET_RUNTIME_ERROR("Variable name must be a Str"));
            }
        }
    }

    fn get_global(&mut self) -> Result<InterpretResult> {
        let var_name = self.get_variable_name()?;
        let val = self
            .globals
            .get(&var_name)
            .ok_or_else(|| INTERPRET_RUNTIME_ERROR("Variable not defined"))?;
        self.stack.push(val.clone());
        Ok(INTERPRET_OK)
    }

    fn define_global(&mut self) -> Result<InterpretResult> {
        let init_val = self.pop_stack()?;
        let var_name = self.get_variable_name()?;
        self.globals.insert(var_name, init_val);
        Ok(INTERPRET_OK)
    }

    fn set_global(&mut self) -> Result<InterpretResult> {
        //TODO: Verify order of pop operations here.
        let val = self.pop_stack()?;
        let var_name = self.get_variable_name()?;
        if self.globals.contains_key(&var_name) {
            self.globals.insert(var_name, val);
            Ok(INTERPRET_OK)
        } else {
            Err(INTERPRET_RUNTIME_ERROR("Variable not definied"))
        }
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
                self.print_stack();
                self.print_globals();
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match OpCode::from(instruction) {
                OP_RETURN => {
                    return Ok(INTERPRET_OK);
                }
                OP_POP => {
                    let _ = self.stack.pop();
                    Ok(INTERPRET_OK)
                }
                OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte() as usize;
                    let constant = self.chunk.constant_pool[constant_addr].clone();
                    self.stack.push(constant);
                    Ok(INTERPRET_OK)
                }
                OP_NEGATE => self.negate(),
                OP_ADD => self.binary_add(),
                OP_MULTIPLY => self.binary_mult(),
                OP_SUBTRACT => self.binary_sub(),
                OP_DIVIDE => self.binary_div(),
                OP_NIL => self.push(Value::Nil()),
                OP_TRUE => self.push(Value::Bool(true)),
                OP_FALSE => self.push(Value::Bool(false)),
                OP_NOT => self.not(),
                OP_EQUAL => self.equal(),
                OP_GREATER => self.greater(),
                OP_LESS => self.less(),
                OP_PRINT => self.print(),
                OP_GET_GLOBAL => self.get_global(),
                OP_DEFINE_GLOBAL => self.define_global(),
                OP_SET_GLOBAL => self.set_global(),
                OP_UNKNOWN => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
    }
}
