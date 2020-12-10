use crate::chunk::OpCode::{self, *};
use crate::chunk::{Chunk, Value};
use crate::compiler::compile;
use crate::debug::disassemble_instruction;
use crate::debugln;
use crate::stack::*;
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

impl From<&'static str> for InterpretResult {
    fn from(msg: &'static str) -> Self {
        INTERPRET_RUNTIME_ERROR(msg)
    }
}

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Stack<Value>,
    pub globals: HashMap<String, Value>,
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            stack: Stack::new(),
            globals: HashMap::new(),
            ip: 0,
        }
    }

    // TODO: Do I need to manually call drop() ever on elements of the stack?
    pub fn reset_repl(&mut self) {
        self.chunk = Chunk::new();
        self.stack = Stack::new();
        self.ip = 0;
    }

    fn print_globals(&self) {
        println!("Globals: {:?}", self.globals);
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.code[self.ip - 1]
    }

    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
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
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left - right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_mult(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left * right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary_div(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left / right))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Operators must be numbers")),
        };
        Ok(INTERPRET_OK)
    }

    fn negate(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop()?;
        match val {
            Value::Number(val) => self.stack.push(Value::Number(-val)),
            Value::Nil() => self.stack.push(val),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Operator must be a number.")),
        };
        Ok(INTERPRET_OK)
    }

    fn not(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop()?;
        self.push(match val {
            Value::Nil() => Value::Bool(true),
            Value::Bool(bool) => Value::Bool(!bool),
            _ => Value::Bool(false),
        })
    }

    fn equal(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        self.push(Value::Bool(
            discriminant(&left) == discriminant(&right) && left == right,
        ))
    }

    fn greater(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
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
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
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
        println!("=={}==", self.stack.pop()?);
        Ok(INTERPRET_OK)
    }

    fn get_variable_name(&mut self) -> Result<String> {
        let constant_addr: usize = self.read_byte() as usize;
        let constant = self.chunk.constant_pool[constant_addr].clone();
        match constant {
            Value::Str(v) => Ok(v.clone()),
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
        let init_val = self.stack.pop()?;
        let var_name = self.stack.pop()?;
        self.globals.insert(var_name.to_string(), init_val);
        Ok(INTERPRET_OK)
    }

    fn set_global(&mut self) -> Result<InterpretResult> {
        let var_name = self.get_variable_name()?;
        if let Some(val) = self.stack.peek() {
            if self.globals.contains_key(&var_name) {
                self.globals.insert(var_name, val);
                Ok(INTERPRET_OK)
            } else {
                Err(INTERPRET_RUNTIME_ERROR("Variable not definied"))
            }
        } else {
            Err(INTERPRET_RUNTIME_ERROR(
                "Invalid expression to assign global",
            ))
        }
    }

    fn get_jump_offset(&mut self) -> usize {
        let high_bits = self.read_byte() as u16;
        let low_bits = self.read_byte() as u16;
        ((high_bits << 8) | low_bits) as usize
    }

    fn is_falsey(&self) -> bool {
        if let Some(v) = self.stack.peek() {
            match v {
                Value::Bool(b) if !b => true,
                Value::Nil() => true,
                _ => false,
            }
        } else {
            false
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
                println!("-----------------------------------------------------------------");
                self.stack.print_stack();
                self.print_globals();
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match OpCode::from(instruction) {
                OP_RETURN => {
                    return Ok(INTERPRET_OK);
                }
                OP_POP => {
                    let _ = self.stack.pop()?;
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
                OP_GET_LOCAL => {
                    // Getting a local simply entails reading it from local section (front)
                    // of stack and then pushing that onto the stack.
                    let local_index: usize = self.read_byte() as usize;
                    let val = self.stack.get(local_index)?.clone();
                    self.stack.push(val);
                    Ok(INTERPRET_OK)
                }
                OP_SET_LOCAL => {
                    // Setting a local updates the local section with what is on top of the
                    // stack but *leaves* that on the stack since assignment is an expression
                    // in Lox.
                    let local_index: usize = self.read_byte() as usize;
                    if let Some(new_val) = self.stack.peek() {
                        self.stack.update(local_index, new_val);
                        Ok(INTERPRET_OK)
                    } else {
                        Err(INTERPRET_RUNTIME_ERROR("No value on stack to assign"))
                    }
                }
                // Control flow is all similar--grab the jump offset and move the ip based on that.
                OP_JUMP_IF_FALSE => {
                    let offset = self.get_jump_offset();
                    if self.is_falsey() {
                        self.ip += offset;
                    }
                    Ok(INTERPRET_OK)
                }
                OP_JUMP => {
                    let offset = self.get_jump_offset();
                    self.ip += offset;
                    Ok(INTERPRET_OK)
                }
                OP_LOOP => {
                    let offset = self.get_jump_offset();
                    self.ip -= offset;
                    Ok(INTERPRET_OK)
                }
                OP_UNKNOWN => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
    }
}
