use crate::chunk::Chunk;
use crate::chunk::OpCode::{self, *};
use crate::compiler::compile;
use crate::debug::disassemble_instruction;
use crate::debugln;
use crate::natives;
use crate::value::{LoxClosure, LoxFn, NativeFn, Value};
use std::collections::HashMap;
use std::mem::discriminant;
use std::rc::Rc;

type Result<T> = std::result::Result<T, InterpretResult>;

#[derive(PartialEq, Debug)]
pub enum InterpretResult {
    INTERPRET_DONE,
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
    closure: Rc<LoxClosure>,
    slot: usize,
    ip: usize,
}

// A stack-based VM. IP points into the current executing instruction. Every expression should have
// a net stack effect of 1 (leaving the result on top of the stack) and every statement should have no
// net stack effect (leaving stack same as before).
//
// This operates with a stack of Call Frame structs that contain the function being executed (where
// top level script is a function) that has the chunk of code. Slot is the offset into the *shared*
// VM stack across call frames. Each call frame maintains its own ip, so after returning there is
// no jump / return address required: the frame is popped and the next frame picks up where the ip
// left off.
const MAX_FRAME_COUNT: usize = 64;
const STACK_SIZE: usize = MAX_FRAME_COUNT * 256;
pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Rc<Value>>,
    globals: HashMap<String, Rc<Value>>,
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
        let mut vm = VM {
            frames: Vec::with_capacity(MAX_FRAME_COUNT),
            stack: Vec::with_capacity(STACK_SIZE),
            globals: HashMap::new(),
        };
        vm.define_native(NativeFn {
            name: "clock".to_string(),
            arity: 0,
            func: Box::new(&natives::clock),
        })
        .unwrap();
        vm.define_native(NativeFn {
            name: "println".to_string(),
            arity: 1,
            func: Box::new(&natives::println),
        })
        .unwrap();
        vm.define_native(NativeFn {
            name: "print".to_string(),
            arity: 1,
            func: Box::new(&natives::print),
        })
        .unwrap();
        vm
    }

    // A number of functions that help with the indirection around a vector of stacks to ensure
    // consistent handling of Option / Results and be granular with mutable and immutable borrows
    // going into the functions as required.
    fn ip(&self) -> Result<usize> {
        Ok(self.frames.last().ok_or(INTERPRET_RUNTIME_ERROR("Empty frame stack"))?.ip)
    }

    fn chunk(&self) -> Result<&Chunk> {
        Ok(&self.frames.last().ok_or(INTERPRET_RUNTIME_ERROR("Empty frame stack"))?.closure.func.chunk)
    }

    fn increment_ip(&mut self) -> Result<()> {
        self.frames.last_mut().ok_or(INTERPRET_RUNTIME_ERROR("Empty frame stack"))?.ip += 1;
        Ok(())
    }

    fn add_ip(&mut self, offset: usize) -> Result<()> {
        self.frames.last_mut().ok_or(INTERPRET_RUNTIME_ERROR("Empty frame stack"))?.ip += offset;
        Ok(())
    }

    fn sub_ip(&mut self, offset: usize) -> Result<()> {
        self.frames.last_mut().ok_or(INTERPRET_RUNTIME_ERROR("Empty frame stack"))?.ip -= offset;
        Ok(())
    }

    fn get_constant(&self, constant_addr: usize) -> Result<Rc<Value>> {
        Ok(self.chunk()?.constant_pool[constant_addr].clone())
    }

    fn frame_slot(&self) -> Result<usize> {
        Ok(self.frames.last().ok_or(INTERPRET_RUNTIME_ERROR("Empty frame stack"))?.slot)
    }

    #[allow(dead_code)]
    fn print_globals(&self) {
        if !self.globals.is_empty() {
            println!("Globals: {:?}", self.globals);
        }
    }

    fn stack_trace(&mut self, source: &str) -> Result<()> {
        let source: Vec<&str> = source.split("\n").collect();
        while !self.frames.is_empty() {
            let source_line = self.chunk()?.lines[self.ip()?] - 1;
            println!("[line {}] in {}", source_line + 1, source[source_line as usize]);
            self.frames.pop().unwrap();
        }
        Ok(())
    }

    // Reads the next byte and moves the instruction pointer.
    fn read_byte(&mut self) -> Result<u8> {
        self.increment_ip()?;
        let ip = self.ip()?;
        Ok(self.chunk()?.code[ip - 1])
    }

    // When a call starts the code / stack looks like below. The function name and
    // argument values are already pushed onto the stack.
    //
    // First we read the argument count (the operand to OP_CALL), then grab the function
    // stored before the arg values and call that function.
    //
    // Calling creates a new call frame which, through indirection of functions
    // like ip() on the VM makes the execution jump into the chunk of code on that
    // function.
    //
    // Code:  [...|OP_CALL|Arg Count|...]
    // --------------^
    //               IP
    // Stack: [VM_SLOT|LoxFn|Arg_0|...|Arg_N|...]
    // --------------------^------------------^
    //         Top - Arg - 1                Top
    //
    fn setup_call(&mut self) -> Result<InterpretResult> {
        let arg_count = self.read_byte()? as usize;
        let function_idx = self.stack.len() - arg_count - 1;
        let function = self.stack[function_idx].clone();
        match &*function {
            Value::Closure(c) => {
                let f = &c.func;
                if f.arity != arg_count as u8 {
                    println!("Expected {} arguments in {} but got {}", f.arity, f, arg_count);
                    return Err(INTERPRET_RUNTIME_ERROR("Wrong number of arguments"));
                } else {
                    self.call(c.clone(), function_idx)
                }
            }
            Value::NativeFunction(f) => {
                let arg_start = function_idx + 1;
                let func = &f.func;
                let result = func(&self.stack[arg_start..]);
                self.stack.truncate(function_idx);
                self.stack.push(Rc::new(result));
                Ok(INTERPRET_OK)
            }
            _ => Err(INTERPRET_RUNTIME_ERROR("Unable to find function definition")),
        }
    }

    fn ret(&mut self) -> Result<InterpretResult> {
        // If we are at the outer frame (<script>) then the final return will exit the
        // program execution
        if self.frames.len() == 1 {
            self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow ret"))?;
            self.frames.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow ret"))?;
            return Ok(INTERPRET_DONE);
        }
        let ret = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow ret"))?;
        self.discard_frame()?;
        self.stack.push(ret);
        Ok(INTERPRET_OK)
    }

    fn call(&mut self, closure: Rc<LoxClosure>, slot: usize) -> Result<InterpretResult> {
        if self.frames.len() == 255 {
            return Err(INTERPRET_RUNTIME_ERROR("Stack overflow"));
        }
        let frame = CallFrame { closure, slot, ip: 0 };
        self.frames.push(frame);
        Ok(INTERPRET_OK)
    }

    fn make_closure(&mut self, func: Rc<LoxFn>) -> Result<Rc<LoxClosure>> {
        Ok(Rc::new(LoxClosure { func }))
    }

    fn discard_frame(&mut self) -> Result<InterpretResult> {
        let frame = self.frames.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow discarding frame"))?;
        for _ in 0..self.stack.len() - frame.slot {
            self.stack.pop();
        }
        Ok(INTERPRET_OK)
    }

    // Very duplicated code, but as of yet I can't figure out a way to pass in a generic
    // function that will take either f64, f64 -> f64 and String, String -> String.
    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow binary add"))?;
        let left = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow binary add"))?;
        match (&*left, &*right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Rc::new(Value::Number(left + right))),
            (Value::Str(left), Value::Str(right)) => {
                let new_string = left.clone() + right;
                self.stack.push(Rc::new(Value::Str(new_string)))
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Invalid operand types")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary(&mut self, operator: &dyn Fn(f64, f64) -> f64) -> Result<InterpretResult> {
        let right = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow binary"))?;
        let left = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow binary"))?;
        match (&*left, &*right) {
            (Value::Number(left), Value::Number(right)) => self.stack.push(Rc::new(Value::Number(operator(*left, *right)))),
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Invalid operand types")),
        };
        Ok(INTERPRET_OK)
    }

    fn negate(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow negate"))?;
        match *val {
            Value::Number(val) => self.stack.push(Rc::new(Value::Number(-val))),
            Value::Nil() => self.stack.push(val),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Operator must be a number.")),
        };
        Ok(INTERPRET_OK)
    }

    fn not(&mut self) -> Result<InterpretResult> {
        let val = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow not"))?;
        self.push(match *val {
            Value::Nil() => Rc::new(Value::Bool(true)),
            Value::Bool(bool) => Rc::new(Value::Bool(!bool)),
            _ => Rc::new(Value::Bool(false)),
        })
    }

    fn equal(&mut self) -> Result<InterpretResult> {
        let right = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow equal"))?;
        let left = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow equal"))?;
        self.push(Rc::new(Value::Bool(discriminant(&left) == discriminant(&right) && left == right)))
    }

    fn cmp(&mut self, op: &dyn Fn(f64, f64) -> bool) -> Result<InterpretResult> {
        let right = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow cmp"))?;
        let left = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow cmp"))?;
        match (&*left, &*right) {
            (Value::Number(left), Value::Number(right)) => self.push(Rc::new(Value::Bool(op(*left, *right)))),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Comparison operands must be numbers.")),
        }
    }

    fn push(&mut self, val: Rc<Value>) -> Result<InterpretResult> {
        self.stack.push(val);
        Ok(INTERPRET_OK)
    }

    fn print(&mut self) -> Result<InterpretResult> {
        print!("{}", self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow print"))?);
        Ok(INTERPRET_OK)
    }

    fn get_variable_name(&mut self) -> Result<String> {
        let constant_addr: usize = self.read_byte()? as usize;
        let constant = &self.get_constant(constant_addr)?;
        match &**constant {
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
        let init_val = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow def global"))?;
        let var_name = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow def global"))?;
        self.globals.insert(var_name.to_string(), init_val);
        Ok(INTERPRET_OK)
    }

    fn define_native(&mut self, function: NativeFn) -> Result<InterpretResult> {
        let name = function.name.to_string();
        let native = Value::NativeFunction(function);
        self.globals.insert(name, Rc::new(native));
        Ok(INTERPRET_OK)
    }

    fn set_global(&mut self) -> Result<InterpretResult> {
        let var_name = self.get_variable_name()?;
        // Peek the value off since assignment is an expression so it should leave the expression
        // output (the assignment value) on the stack.
        //
        // a = b = 5
        let val = self.stack.last().ok_or_else(|| INTERPRET_RUNTIME_ERROR("Invalid assigment"))?;
        if self.globals.contains_key(&var_name) {
            self.globals.insert(var_name.to_string(), val.clone());
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
        let val = self.stack.last().ok_or(INTERPRET_RUNTIME_ERROR("Empty stack"))?;
        match **val {
            Value::Bool(b) if !b => Ok(true),
            Value::Nil() => Ok(true),
            _ => Ok(false),
        }
    }

    // Main entry point into the VM -- compile and run.
    pub fn interpret(&mut self, source: &str) -> Result<InterpretResult> {
        let closure = Rc::new(LoxClosure {
            func: Rc::new(compile(source)?),
        });
        self.stack.push(Rc::new(Value::Str(String::from("script"))));
        self.call(closure, 0)?;
        match self.run() {
            Ok(_) => Ok(INTERPRET_OK),
            Err(e) => {
                println!("{:?}", e);
                self.stack_trace(source)?;
                Err(e)
            }
        }
    }

    fn run(&mut self) -> Result<InterpretResult> {
        debugln!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
        let mut status = INTERPRET_OK;
        while status == INTERPRET_OK {
            if crate::debug() {
                println!("-----------------------------------------------------------------");
                println!("Stack top {}: {:?}", self.stack.len(), self.stack);
                //self.print_globals();
                disassemble_instruction(self.chunk()?, self.ip()?);
            }
            let instruction = self.read_byte()?;
            status = match OpCode::from(instruction) {
                OP_CALL => self.setup_call(),
                OP_RETURN => self.ret(),
                OP_NEGATE => self.negate(),
                OP_ADD => self.binary_add(),
                OP_MULTIPLY => self.binary(&std::ops::Mul::mul),
                OP_SUBTRACT => self.binary(&std::ops::Sub::sub),
                OP_DIVIDE => self.binary(&std::ops::Div::div),
                OP_NIL => self.push(Rc::new(Value::Nil())),
                OP_TRUE => self.push(Rc::new(Value::Bool(true))),
                OP_FALSE => self.push(Rc::new(Value::Bool(false))),
                OP_NOT => self.not(),
                OP_EQUAL => self.equal(),
                OP_GREATER => self.cmp(&gt),
                OP_LESS => self.cmp(&lt),
                OP_PRINT => self.print(),
                OP_GET_GLOBAL => self.get_global(),
                OP_DEFINE_GLOBAL => self.define_global(),
                OP_SET_GLOBAL => self.set_global(),
                OP_POP => {
                    self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow pop"))?;
                    Ok(INTERPRET_OK)
                }
                OP_CLOSURE => {
                    // Closure sets up a runtime representation of a LoxFn
                    let func_addr: usize = self.read_byte()? as usize;
                    let func = self.get_constant(func_addr)?;
                    let func = match &*func {
                        Value::Function(f) => self.make_closure(f.clone())?,
                        _ => return Err(INTERPRET_RUNTIME_ERROR("Invalid closure argument, function required")),
                    };
                    self.stack.push(Rc::new(Value::Closure(func)));
                    Ok(INTERPRET_OK)
                }
                OP_CONSTANT => {
                    // Puts a constant defined in the code chunk constant pool onto the stack
                    let constant_addr: usize = self.read_byte()? as usize;
                    let constant = self.get_constant(constant_addr)?.clone();
                    self.stack.push(constant);
                    Ok(INTERPRET_OK)
                }
                OP_GET_LOCAL => {
                    // Getting a local simply entails reading it from local section (front)
                    // of stack and then pushing that onto the stack. The first spot is reservered
                    // for use by the VM so indices start at 1 for user definined locals.
                    //
                    // The offset of frame_slot is since the stack is shared across all call
                    // frames
                    //
                    // [F1|..|F2|..|F3|Local1|Local2
                    // --------------^----------^
                    //      frame_slot    frame_slot + 2
                    let local_index: usize = self.read_byte()? as usize;
                    let val = self
                        .stack
                        .get(self.frame_slot()? + local_index)
                        .ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow getting local"))?
                        .clone();
                    self.stack.push(val);
                    Ok(INTERPRET_OK)
                }
                OP_SET_LOCAL => {
                    // Setting a local updates the local section with what is on top of the
                    // stack but *leaves* that on the stack since assignment is an expression
                    // in Lox.
                    let local_index: usize = self.read_byte()? as usize;
                    let new_val = self.stack.last().ok_or(INTERPRET_RUNTIME_ERROR("No value on stack to assign"))?.clone();
                    let slot = self.frame_slot()?;
                    self.stack[slot + local_index] = new_val;
                    Ok(INTERPRET_OK)
                }
                // Control flow is all similar--grab the jump offset and move the ip based on that.
                OP_JUMP_IF_FALSE => {
                    let offset = self.get_jump_offset()?;
                    if self.is_falsey()? {
                        self.add_ip(offset)?;
                    }
                    Ok(INTERPRET_OK)
                }
                OP_JUMP => {
                    let offset = self.get_jump_offset()?;
                    self.add_ip(offset)?;
                    Ok(INTERPRET_OK)
                }
                OP_LOOP => {
                    let offset = self.get_jump_offset()?;
                    self.sub_ip(offset)?;
                    Ok(INTERPRET_OK)
                }
                OP_UNKNOWN => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
        Ok(status)
    }
}
