use crate::chunk::Chunk;
use crate::compiler::compile;
use crate::debug::disassemble_instruction;
use crate::debugln;
use crate::memory;
use crate::memory::ValuePtr;
use crate::natives;
use crate::opcode::OpCode::{self, *};
use crate::value::{BoundMethod, Class, Closure, Instance, NativeFn, Upvalue, Value};
use std::collections::BTreeMap;
use std::mem::discriminant;

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

#[derive(Debug, PartialEq)]
pub struct CallFrame {
    closure: ValuePtr<Closure>,
    slot: usize,
    prior_chunk: *const Chunk,
    prior_ip: usize,
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
    stack: Vec<Value>,
    ip: usize,
    chunk: *const Chunk,
    upvalues: Vec<Upvalue>,
    globals: BTreeMap<String, Value>,
    gray_vals: Vec<Value>,
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
            frames: Vec::with_capacity(MAX_FRAME_COUNT),
            stack: Vec::with_capacity(STACK_SIZE),
            ip: 0,
            chunk: std::ptr::null(),
            upvalues: Vec::with_capacity(u8::MAX as usize),
            gray_vals: Vec::with_capacity(STACK_SIZE),
            globals: BTreeMap::new(),
        }
    }

    pub fn define_natives(&mut self) {
        self.define_native(NativeFn {
            name: "clock".to_string(),
            arity: 0,
            func: Box::new(natives::clock),
        })
        .unwrap();
        self.define_native(NativeFn {
            name: "println".to_string(),
            arity: 1,
            func: Box::new(natives::println),
        })
        .unwrap();
        self.define_native(NativeFn {
            name: "print".to_string(),
            arity: 1,
            func: Box::new(natives::print),
        })
        .unwrap();
    }

    pub fn mark_objects(&mut self) {
        for obj in &self.stack {
            obj.mark();
            self.gray_vals.push(*obj);
        }
        for obj in &self.upvalues {
            if let Some(closed) = obj.closed {
                closed.mark();
                self.gray_vals.push(closed);
            }
        }
        for (_, obj) in &self.globals {
            obj.mark();
            self.gray_vals.push(*obj);
        }
        for frame in &self.frames {
            frame.closure.mark();
            self.gray_vals.push(Value::Closure(frame.closure));
        }
    }

    pub fn trace_refs(&mut self) {
        while self.gray_vals.len() > 0 {
            match self.gray_vals.pop().unwrap() {
                Value::Closure(ptr) => {
                    for upvalue in &ptr.upvalues {
                        unsafe {
                            if let Some(closed) = (*(*upvalue)).closed {
                                closed.mark();
                            }
                        }
                    }
                    if !ptr.func().is_marked() {
                        ptr.func().mark();
                        self.gray_vals.push(Value::LoxFn(ptr.func()));
                    }
                }
                Value::Class(ptr) => {
                    for (_, method) in &ptr.methods {
                        if !method.is_marked() {
                            method.mark();
                            self.gray_vals.push(Value::Closure(*method));
                        }
                    }
                }
                Value::Instance(ptr) => {
                    for (_, field) in &ptr.fields {
                        field.mark();
                    }
                    if !ptr.class.is_marked() {
                        ptr.class.mark();
                        self.gray_vals.push(Value::Class(ptr.class));
                    }
                }
                Value::BoundMethod(ptr) => {
                    if !ptr.receiver.is_marked() {
                        ptr.receiver.mark();
                        self.gray_vals.push(Value::Instance(ptr.receiver));
                    }
                    if !ptr.method.is_marked() {
                        ptr.method.mark();
                        self.gray_vals.push(Value::Closure(ptr.method));
                    }
                }
                Value::LoxFn(_) => {}    // Functions are static allocated during compilation
                Value::NativeFn(_) => {} // Native functions are static allocated at startup
                Value::Str(_) => {}      // Strings have no references
                _ => {}
            }
        }
    }

    // A number of functions that help with the indirection around the stacks
    #[inline]
    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    #[inline]
    fn peek_mut(&mut self) -> &mut Value {
        self.stack.last_mut().unwrap()
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    #[inline]
    fn get_constant(&self, constant_addr: usize) -> &Value {
        unsafe { &(*self.chunk).constant_pool[constant_addr] }
    }

    #[inline]
    fn slot(&self) -> usize {
        self.frames.last().unwrap().slot
    }

    #[inline]
    fn closure(&mut self) -> ValuePtr<Closure> {
        self.frames.last_mut().unwrap().closure
    }

    #[allow(dead_code)]
    fn print_globals(&self) {
        if !self.globals.is_empty() {
            println!("Globals: {:?}", self.globals);
        }
    }

    fn print_stack(&self) {
        print!("Stack top {}: ", self.stack.len());
        print!("[");
        for val in &self.stack {
            print!(" [{}], ", val);
        }
        println!("]");
    }

    #[allow(dead_code)]
    fn print_upvals(&self) {
        if !self.upvalues.is_empty() {
            println!("Upvalues: {:?}", self.upvalues);
        }
    }

    fn stack_trace(&mut self, source: &str) -> Result<()> {
        let source: Vec<&str> = source.split("\n").collect();
        while !self.frames.is_empty() {
            unsafe {
                let source_line = (*self.chunk).lines[self.ip] - 1;
                println!("[line {}] in {}", source_line + 1, source[source_line as usize]);
                let frame = self.frames.pop().unwrap();
                self.ip = frame.prior_ip;
                self.chunk = frame.prior_chunk;
            }
        }
        Ok(())
    }

    // Reads the next byte and moves the instruction pointer.
    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        unsafe { (*self.chunk).code[self.ip - 1] }
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
        let arg_count = self.read_byte() as usize;
        let function_idx = self.stack.len() - arg_count - 1;
        let function = self.stack[function_idx];
        match function {
            Value::Class(ptr) => {
                let instance = Value::Instance(memory::allocate(Instance::new(ptr)));
                self.stack[function_idx] = instance;
                if function.methods().contains_key("init") {
                    self.call(*function.methods().get("init").unwrap(), function_idx)?;
                }
                Ok(INTERPRET_OK)
            }
            Value::BoundMethod(ptr) => {
                self.stack[function_idx] = Value::Instance(ptr.receiver);
                self.call(ptr.method, function_idx)
            }
            Value::Closure(ptr) => {
                if (*ptr).arity() != arg_count as u8 {
                    println!("Expected {} arguments in {} but got {}", (*ptr).arity(), (*ptr), arg_count);
                    return Err(INTERPRET_RUNTIME_ERROR("Wrong number of arguments"));
                }
                self.call(ptr, function_idx)
            }
            Value::NativeFn(ptr) => {
                let arg_start = function_idx + 1;
                let result = ((*ptr).func)(&self.stack[arg_start..]);
                self.stack.truncate(function_idx);
                self.push(result);
                Ok(INTERPRET_OK)
            }
            _ => Err(INTERPRET_RUNTIME_ERROR("Unable to find function definition")),
        }
    }

    fn ret(&mut self) -> Result<InterpretResult> {
        // If we are at the outer frame (<script>) then the final return will exit the
        // program execution
        if self.frames.len() == 1 {
            self.pop();
            self.frames.pop();
            return Ok(INTERPRET_DONE);
        }
        let ret = self.pop();
        self.discard_frame()?;
        self.push(ret);
        Ok(INTERPRET_OK)
    }

    fn call(&mut self, closure: ValuePtr<Closure>, slot: usize) -> Result<InterpretResult> {
        if self.frames.len() == 255 {
            return Err(INTERPRET_RUNTIME_ERROR("Stack overflow"));
        }
        let frame = CallFrame {
            closure: closure,
            slot,
            prior_ip: self.ip,
            prior_chunk: self.chunk,
        };
        self.ip = 0;
        self.chunk = closure.chunk() as *const Chunk;
        self.frames.push(frame);
        Ok(INTERPRET_OK)
    }

    fn close_upvalue(&mut self) -> Result<InterpretResult> {
        let val = self.pop();
        let location = self.stack.len();
        for upvalue in &mut self.upvalues {
            if upvalue.location == location {
                upvalue.closed = Some(val);
                return Ok(INTERPRET_OK);
            }
        }
        return Ok(INTERPRET_OK);
    }

    fn capture_upvalue(&mut self, location: usize) -> *mut Upvalue {
        for upvalue in &mut self.upvalues {
            if upvalue.closed == None && upvalue.location == location {
                return upvalue;
            }
        }
        self.upvalues.push(Upvalue { location, closed: None });
        let idx = self.upvalues.len() - 1;
        &mut self.upvalues[idx]
    }

    fn make_closure(&mut self, mut closure: Closure) -> Closure {
        for _ in 0..closure.upvalue_count() {
            let is_local = self.read_byte();
            let idx = self.read_byte();
            let upvalue: *mut Upvalue = if is_local == 0x1 {
                let location = self.slot() + idx as usize;
                self.capture_upvalue(location)
            } else {
                (*self.closure()).get_upvalue(idx as usize)
            };
            closure.add_upvalue(upvalue);
        }
        closure
    }

    fn discard_frame(&mut self) -> Result<InterpretResult> {
        let frame = self.frames.pop().ok_or(INTERPRET_RUNTIME_ERROR("Error discarding call frame"))?;
        self.ip = frame.prior_ip;
        self.chunk = frame.prior_chunk;
        if self.upvalues.len() == 0 {
            self.stack.truncate(frame.slot + 1);
        } else {
            for _ in 0..self.stack.len() - frame.slot - 1 {
                self.close_upvalue()?;
            }
        }
        self.pop();
        Ok(INTERPRET_OK)
    }

    // Very duplicated code, but as of yet I can't figure out a way to pass in a generic
    // function that will take either f64, f64 -> f64 and String, String -> String.
    fn binary_add(&mut self) -> Result<InterpretResult> {
        let right = &self.stack[self.stack.len() - 1];
        let left = &self.stack[self.stack.len() - 2];
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                let sum = left + right;
                self.pop();
                self.pop();
                self.push(Value::Number(sum));
            }
            (Value::Str(left), Value::Str(right)) => {
                let val = memory::allocate((**left).clone() + &(**right));
                self.pop();
                self.pop();
                self.push(Value::Str(val));
            }
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Invalid operand types")),
        };
        Ok(INTERPRET_OK)
    }

    fn binary(&mut self, operator: &dyn Fn(f64, f64) -> f64) -> Result<InterpretResult> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.push(Value::Number(operator(left, right))),
            (_, _) => return Err(INTERPRET_RUNTIME_ERROR("Invalid operand types")),
        };
        Ok(INTERPRET_OK)
    }

    fn negate(&mut self) -> Result<InterpretResult> {
        let val = self.pop();
        match val {
            Value::Number(val) => self.push(Value::Number(-val)),
            Value::Nil() => self.push(val),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Operator must be a number.")),
        };
        Ok(INTERPRET_OK)
    }

    fn not(&mut self) -> Result<InterpretResult> {
        let val = self.pop();
        self.push(match val {
            Value::Nil() => Value::Bool(true),
            Value::Bool(bool) => Value::Bool(!bool),
            _ => Value::Bool(false),
        });
        Ok(INTERPRET_OK)
    }

    fn equal(&mut self) -> Result<InterpretResult> {
        let right = self.pop();
        let left = self.pop();
        self.push(Value::Bool(discriminant(&left) == discriminant(&right) && left == right));
        Ok(INTERPRET_OK)
    }

    fn cmp(&mut self, op: &dyn Fn(f64, f64) -> bool) -> Result<InterpretResult> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => self.push(Value::Bool(op(left, right))),
            _ => return Err(INTERPRET_RUNTIME_ERROR("Comparison operands must be numbers.")),
        };
        Ok(INTERPRET_OK)
    }

    #[inline]
    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn get_variable_name(&self, addr: usize) -> &str {
        match self.get_constant(addr) {
            Value::Str(ptr) => &(*ptr),
            _ => unreachable!(),
        }
    }

    fn get_global(&mut self) -> Result<InterpretResult> {
        let constant_addr: usize = self.read_byte() as usize;
        let var_name = self.get_variable_name(constant_addr);
        let val = *self
            .globals
            .get(var_name)
            .ok_or_else(|| INTERPRET_RUNTIME_ERROR("Variable not defined"))?;
        self.push(val);
        Ok(INTERPRET_OK)
    }

    fn define_global(&mut self) -> Result<InterpretResult> {
        let init_val = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow def global"))?;
        let var_name = self.stack.pop().ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow def global"))?;
        self.globals.insert(var_name.str().to_string(), init_val);
        Ok(INTERPRET_OK)
    }

    fn define_native(&mut self, function: NativeFn) -> Result<InterpretResult> {
        let name = function.name.to_string();
        let native = Value::NativeFn(memory::staticallocate(function));
        self.globals.insert(name, native);
        Ok(INTERPRET_OK)
    }

    fn set_global(&mut self) -> Result<InterpretResult> {
        let constant_addr: usize = self.read_byte() as usize;
        let var_name = self.get_variable_name(constant_addr).to_string();
        // Peek the value off since assignment is an expression so it should leave the expression
        // output (the assignment value) on the stack.
        // a = b = 5
        let val = *self.peek();
        if self.globals.contains_key(&var_name) {
            self.globals.insert(var_name, val);
            Ok(INTERPRET_OK)
        } else {
            Err(INTERPRET_RUNTIME_ERROR("Variable not definied"))
        }
    }

    fn method(&mut self) -> Result<InterpretResult> {
        let method = self.pop();
        let class: &mut Value = self.peek_mut();
        class.add_method(method);
        Ok(INTERPRET_OK)
    }

    fn closure_op(&mut self) -> Result<InterpretResult> {
        // Closure sets up a runtime representation of a LoxFn
        let func_addr: usize = self.read_byte() as usize;
        let closure = Closure::new(*self.get_constant(func_addr));
        let closure = self.make_closure(closure);
        let closure = memory::allocate(closure);
        self.push(Value::Closure(closure));
        Ok(INTERPRET_OK)
    }

    fn super_(&mut self) -> Result<InterpretResult> {
        let method_idx = self.read_byte();
        let method_name = self.get_constant(method_idx as usize).str().to_string();
        let super_class = *self.peek();
        if let Value::Class(ptr) = super_class {
            let instance = Value::Instance(memory::allocate(Instance::new(ptr)));
            self.push(instance);
            if super_class.methods().contains_key(&method_name) {
                let method = super_class
                    .methods()
                    .get(&method_name)
                    .ok_or_else(|| INTERPRET_RUNTIME_ERROR("Super method not defined"))?;
                let bound_method = Value::BoundMethod(memory::allocate(BoundMethod::new(instance, *method)));
                self.pop();
                self.pop();
                self.push(bound_method);
                return Ok(INTERPRET_OK);
            }
        }
        Err(INTERPRET_RUNTIME_ERROR("Method not found on Super"))
    }

    fn inherit(&mut self) -> Result<InterpretResult> {
        let mut sub_ = match self.pop() {
            Value::Class(ptr) => ptr,
            _ => return Err(INTERPRET_RUNTIME_ERROR("Not a valid subclass")),
        };
        let super_ = self.peek();
        if let Value::Class(_) = super_ {
            for (_, method) in super_.methods() {
                sub_.add_method(*method);
            }
        } else {
            return Err(INTERPRET_RUNTIME_ERROR("Not a valid superclass"));
        }
        Ok(INTERPRET_OK)
    }

    fn class(&mut self) -> Result<InterpretResult> {
        let class_name = (*self.peek()).str();
        let class = Class::new(class_name);
        let class = Value::Class(memory::allocate(class));
        self.push(class);
        Ok(INTERPRET_OK)
    }

    fn constant(&mut self) -> Result<InterpretResult> {
        // Puts a constant defined in the code chunk constant pool onto the stack
        let constant_addr: usize = self.read_byte() as usize;
        let constant = *self.get_constant(constant_addr);
        self.push(constant);
        Ok(INTERPRET_OK)
    }

    fn get_local(&mut self) -> Result<InterpretResult> {
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
        let local_index: usize = self.read_byte() as usize;
        let val = *self
            .stack
            .get(self.slot() + local_index)
            .ok_or(INTERPRET_RUNTIME_ERROR("Stack underflow getting local"))?;
        self.push(val);
        Ok(INTERPRET_OK)
    }

    fn set_local(&mut self) -> Result<InterpretResult> {
        // Setting a local updates the local section with what is on top of the
        // stack but *leaves* that on the stack since assignment is an expression
        // in Lox.
        let local_index: usize = self.read_byte() as usize;
        let new_val = self.stack.last().ok_or(INTERPRET_RUNTIME_ERROR("No value on stack to assign"))?;
        let slot = self.slot();
        self.stack[slot + local_index] = *new_val;
        Ok(INTERPRET_OK)
    }

    fn set_property(&mut self) -> Result<InterpretResult> {
        let field_addr = self.read_byte() as usize;
        let field_name = self.get_constant(field_addr).str().to_string();
        let val = self.pop();
        let mut instance = self.pop();
        instance.set_field(&field_name, val);
        self.push(val);
        Ok(INTERPRET_OK)
    }

    fn get_property(&mut self) -> Result<InterpretResult> {
        let field_addr = self.read_byte() as usize;
        let field_name = *self.get_constant(field_addr);
        let instance = self.pop();
        let fields = instance.fields();
        let methods = instance.methods();
        if fields.contains_key(field_name.str()) {
            let val = fields.get(field_name.str()).ok_or_else(|| INTERPRET_RUNTIME_ERROR("Field not defined"))?;
            self.push(*val);
            return Ok(INTERPRET_OK);
        } else if methods.contains_key(field_name.str()) {
            let val = methods
                .get(field_name.str())
                .ok_or_else(|| INTERPRET_RUNTIME_ERROR("Field not defined"))?;
            self.push(Value::BoundMethod(memory::allocate(BoundMethod::new(instance, *val))));
            return Ok(INTERPRET_OK);
        }
        Err(INTERPRET_RUNTIME_ERROR("Property not found on Class"))
    }

    fn get_upvalue(&mut self) -> Result<InterpretResult> {
        let upvalue_idx = self.read_byte() as usize;
        unsafe {
            let upvalue = (*self.closure()).get_upvalue(upvalue_idx);
            let val = match &(*upvalue).closed {
                Some(val) => val.clone(),
                None => self.stack[(*upvalue).location].clone(),
            };
            self.push(val);
        }
        Ok(INTERPRET_OK)
    }

    fn set_upvalue(&mut self) -> Result<InterpretResult> {
        let new_val = self.peek().clone();
        let upvalue_idx = self.read_byte() as usize;
        unsafe {
            let upvalue = (*self.closure()).get_upvalue(upvalue_idx);
            match (*upvalue).closed {
                Some(_) => {
                    (*upvalue).closed = Some(new_val);
                }
                None => {
                    self.stack[(*upvalue).location] = new_val;
                }
            }
        }
        Ok(INTERPRET_OK)
    }

    #[inline]
    fn jump_false(&mut self) -> Result<InterpretResult> {
        let offset = self.get_jump_offset();
        if self.peek().is_falsey() {
            self.ip += offset;
        }
        Ok(INTERPRET_OK)
    }

    #[inline]
    fn jump(&mut self) -> Result<InterpretResult> {
        let offset = self.get_jump_offset();
        self.ip += offset;
        Ok(INTERPRET_OK)
    }

    #[inline]
    fn loop_(&mut self) -> Result<InterpretResult> {
        let offset = self.get_jump_offset();
        self.ip -= offset;
        Ok(INTERPRET_OK)
    }

    // Pulls the two bytes that make up the 16 bit jump offset and return this
    // offset (to be used to increment / decrement the instruction pointer).
    #[inline]
    fn get_jump_offset(&mut self) -> usize {
        let high_bits = self.read_byte() as u16;
        let low_bits = self.read_byte() as u16;
        ((high_bits << 8) | low_bits) as usize
    }

    fn nil(&mut self) -> Result<InterpretResult> {
        self.push(Value::Nil());
        Ok(INTERPRET_OK)
    }
    fn true_(&mut self) -> Result<InterpretResult> {
        self.push(Value::Bool(true));
        Ok(INTERPRET_OK)
    }
    fn false_(&mut self) -> Result<InterpretResult> {
        self.push(Value::Bool(false));
        Ok(INTERPRET_OK)
    }

    fn pop_op(&mut self) -> Result<InterpretResult> {
        self.pop();
        Ok(INTERPRET_OK)
    }

    // Main entry point into the VM -- compile and run.
    pub fn interpret(&mut self, source: &str) -> Result<InterpretResult> {
        let main = memory::staticallocate(compile(source)?);
        let closure = memory::staticallocate(Closure::new(Value::LoxFn(main)));
        self.push(Value::Str(memory::staticallocate("script".to_string())));
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
            if crate::trace_execution() {
                println!("-----------------------------------------------------------------");
                self.print_stack();
                //self.print_globals();
                //self.print_upvals();
                unsafe {
                    disassemble_instruction(&(*self.chunk), self.ip);
                }
            }
            let instruction = OpCode::from(self.read_byte());
            status = match instruction {
                OP_GET_GLOBAL => self.get_global(),
                OP_CLOSURE => self.closure_op(),
                OP_CALL => self.setup_call(),
                OP_RETURN => self.ret(),
                OP_CONSTANT => self.constant(),
                OP_GET_LOCAL => self.get_local(),
                OP_LOOP => self.loop_(),
                OP_JUMP_IF_FALSE => self.jump_false(),
                OP_JUMP => self.jump(),
                OP_EQUAL => self.equal(),
                OP_GREATER => self.cmp(&gt),
                OP_LESS => self.cmp(&lt),
                OP_ADD => self.binary_add(),
                OP_MULTIPLY => self.binary(&std::ops::Mul::mul),
                OP_SUBTRACT => self.binary(&std::ops::Sub::sub),
                OP_DIVIDE => self.binary(&std::ops::Div::div),
                OP_POP => self.pop_op(),
                OP_SET_LOCAL => self.set_local(),
                OP_GET_UPVALUE => self.get_upvalue(),
                OP_NEGATE => self.negate(),
                OP_NIL => self.nil(),
                OP_TRUE => self.true_(),
                OP_FALSE => self.false_(),
                OP_NOT => self.not(),
                OP_CLASS => self.class(),
                OP_SET_PROPERTY => self.set_property(),
                OP_GET_PROPERTY => self.get_property(),
                OP_DEFINE_GLOBAL => self.define_global(),
                OP_SET_GLOBAL => self.set_global(),
                OP_SET_UPVALUE => self.set_upvalue(),
                OP_CLOSE_UPVALUE => self.close_upvalue(),
                OP_METHOD => self.method(),
                OP_INHERIT => self.inherit(),
                OP_GET_SUPER => self.super_(),
                OP_PRINT => unreachable!(),
                OP_UNKNOWN => Err(INTERPRET_COMPILE_ERROR),
            }?;
        }
        Ok(status)
    }
}
