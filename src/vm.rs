use crate::chunk::OpCode::{self, *};
use crate::chunk::{Chunk};
use crate::value::{Value, LoxFn};
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
impl std::error::Error for InterpretResult {}
impl std::fmt::Display for InterpretResult {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            INTERPRET_RUNTIME_ERROR(e) => write!(f, "Error encountered during execution: {}", e),
            _ => write!(f, "Error encountered during execution"),
        }
    }
}
impl From<&'static str> for InterpretResult {
    fn from(msg: &'static str) -> Self {
        INTERPRET_RUNTIME_ERROR(msg)
    }
}
impl From<crate::compiler::CompilerError> for InterpretResult {
    fn from(_: crate::compiler::CompilerError) -> Self {
        INTERPRET_COMPILE_ERROR
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallFrame {
    function: LoxFn,
    slot: usize,
    ip: usize,
}

// A stack-based VM. IP points into the current executing instruction. Every expression should have
// a net stack effect of 1 (leaving the result on top of the stack) and every statement should have no
// net stack effect (leaving stack same as before).
pub struct VM {
    frames: Stack<CallFrame>,
    stack: Stack<Value>,
    globals: HashMap<String, Value>,
}

// Two functions to use as pointers for comparisions to avoid some duplication later.
fn gt(left: f64, right: f64) -> bool {
    left > right
}
fn lt(left: f64, right: f64) -> bool {
    left < right
}

use crate::vm::InterpretResult::*;
impl VM {
    pub fn new() -> VM {
        VM {
            frames: Stack::new(),
            stack: Stack::new(),
            globals: HashMap::new(),
        }
    }

    fn ip(&self) -> usize {
        self.frames.peek().unwrap().ip
    }

    fn frame(&mut self) -> &mut CallFrame {
        self.frames.peek_mut().unwrap()
    }

    fn increment_ip(&mut self) {
        self.frames.peek_mut().unwrap().ip += 1;
    }
    
    fn chunk(&self) -> &Chunk {
        &self.frames.peek().unwrap().function.chunk
    }

    fn add_ip(&mut self, offset: usize) {
        self.frames.peek_mut().unwrap().ip += offset;
    }

    fn sub_ip(&mut self, offset: usize) {
        self.frames.peek_mut().unwrap().ip -= offset;
    }
   
    fn get_constant(&self, constant_addr: usize) -> Value {
        self.chunk().constant_pool[constant_addr].clone()
    }

    fn frame_slot(&self) -> usize {
        self.frames.peek().unwrap().slot
    }

    // When running in REPL mode, we want a clean code chunk and stack. Globals are persistent
    // across each line though, so you can do
    // var a = 5; <ENTER>
    // print(a); <ENTER>
    pub fn reset_repl(&mut self) -> Result<()> {
//        self.frame().function.chunk = Chunk::new();
//        self.stack = Stack::new();
//        self.frame().ip = 0;
        Ok(())
    }

    fn print_globals(&self) {
        if !self.globals.is_empty() {
            println!("Globals: {:?}", self.globals);
        }
    }

    // Reads the next byte and moves the instruction pointer.
    fn read_byte(&mut self) -> Result<u8> {
        self.increment_ip();
        let ip = self.ip();
        Ok(self.frame().function.chunk.code[ip])
    }

    // Very duplicated code, but as of yet I can't figure out a way to pass in a generic
    // function that will take either f64, f64 -> f64 and String, String -> String.
    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(left + right))
            }
            (Value::Str(left), Value::Str(right)) => self.stack.push(Value::Str(left + &right)),
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Invalid operand types")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary(&mut self, operator: &dyn Fn(f64, f64) -> f64) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.stack.push(Value::Number(operator(left, right)))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Invalid operand types")),
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

    fn cmp(&mut self, op: &dyn Fn(f64, f64) -> bool) -> Result<InterpretResult> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.push(Value::Bool(op(left, right))),
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
        let constant_addr: usize = self.read_byte()? as usize;
        let constant = &self.frame().function.chunk.constant_pool[constant_addr];
        match constant {
            Value::Str(v) => Ok(v.clone()),
            _ => Err(INTERPRET_RUNTIME_ERROR("Variable name must be a Str")),
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
        // Peek the value off since assignment is an expression so it should leave the expression
        // output (the assignment value) on the stack.
        //
        // a = b = 5
        let val = self
            .stack
            .peek()
            .ok_or_else(|| INTERPRET_RUNTIME_ERROR("Invalid assigment"))?;
        if self.globals.contains_key(&var_name) {
            self.globals.insert(var_name, val.clone());
            Ok(INTERPRET_OK)
        } else {
            Err(INTERPRET_RUNTIME_ERROR("Variable not definied"))
        }
    }

    // Pulls the two bytes that make up the 16 bit jump offset and return this
    // offset (to be used to increment / decrement the instruction pointer).
    fn get_jump_offset(&mut self) -> Result<usize> {
        let high_bits = self.read_byte()? as u16;
        let low_bits = self.read_byte()? as u16;
        Ok(((high_bits << 8) | low_bits) as usize)
    }

    // Definition of 'false' in rlox world
    fn is_falsey(&self) -> Result<bool> {
        let val = self.stack.peek().unwrap();
        match val {
           Value::Bool(b) if !b => Ok(true),
            Value::Nil() => Ok(true),
            _ => Ok(false),
        }
    }

    // Main entry point into the VM -- compile and run.
    pub fn interpret(&mut self, source: &str) -> Result<InterpretResult> {
        let function = compile(source)?;
        let frame = CallFrame { function, slot: 1, ip: 0 };
        self.frames.push(frame);
        self.run()?;
        Ok(INTERPRET_OK)
    }

    fn run(&mut self) -> Result<InterpretResult> {
        debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
        loop {
            if crate::debug() {
                println!("-----------------------------------------------------------------");
                self.stack.print_stack();
                self.print_globals();
                disassemble_instruction(self.chunk(), self.ip());
            }
            let instruction = self.read_byte()?;
            match OpCode::from(instruction) {
                OP_RETURN => {
                    return Ok(INTERPRET_OK);
                }
                OP_POP => {
                    let _ = self.stack.pop()?;
                    Ok(INTERPRET_OK)
                }
                OP_CONSTANT => {
                    let constant_addr: usize = self.read_byte()? as usize;
                    let constant = self.get_constant(constant_addr); 
                    self.stack.push(constant);
                    Ok(INTERPRET_OK)
                }
                OP_NEGATE => self.negate(),
                OP_ADD => self.binary_add(),
                OP_MULTIPLY => self.binary(&std::ops::Mul::mul),
                OP_SUBTRACT => self.binary(&std::ops::Sub::sub),
                OP_DIVIDE => self.binary(&std::ops::Div::div),
                OP_NIL => self.push(Value::Nil()),
                OP_TRUE => self.push(Value::Bool(true)),
                OP_FALSE => self.push(Value::Bool(false)),
                OP_NOT => self.not(),
                OP_EQUAL => self.equal(),
                OP_GREATER => self.cmp(&gt),
                OP_LESS => self.cmp(&lt),
                OP_PRINT => self.print(),
                OP_GET_GLOBAL => self.get_global(),
                OP_DEFINE_GLOBAL => self.define_global(),
                OP_SET_GLOBAL => self.set_global(),
                OP_GET_LOCAL => {
                    // Getting a local simply entails reading it from local section (front)
                    // of stack and then pushing that onto the stack.
                    let local_index: usize = self.read_byte()? as usize;
                    let val = self.stack.get(self.frame_slot() + local_index)?.clone();
                    self.stack.push(val);
                    Ok(INTERPRET_OK)
                }
                OP_SET_LOCAL => {
                    // Setting a local updates the local section with what is on top of the
                    // stack but *leaves* that on the stack since assignment is an expression
                    // in Lox.
                    let local_index: usize = self.read_byte()? as usize;
                    if let Some(new_val) = self.stack.peek() {
                        self.stack.update(self.frame_slot() + local_index, new_val.clone());
                        Ok(INTERPRET_OK)
                    } else {
                        Err(INTERPRET_RUNTIME_ERROR("No value on stack to assign"))
                    }
                }
                // Control flow is all similar--grab the jump offset and move the ip based on that.
                OP_JUMP_IF_FALSE => {
                    let offset = self.get_jump_offset()?;
                    if self.is_falsey()? {
                        self.add_ip(offset);
                    }
                    Ok(INTERPRET_OK)
                }
                OP_JUMP => {
                    let offset = self.get_jump_offset()?;
                    self.add_ip(offset);
                    Ok(INTERPRET_OK)
                }
                OP_LOOP => {
                    let offset = self.get_jump_offset()?;
                    self.sub_ip(offset);
                    Ok(INTERPRET_OK)
                }
                OP_UNKNOWN => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
    }
}
