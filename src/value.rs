use crate::chunk::Chunk;
use crate::memory::ValuePtr;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Upvalue {
    pub location: usize,
    pub closed: Option<Value>,
}
/*---------------------------------------------------------------------*/
#[derive(Debug, PartialEq)]
pub struct Closure {
    func: ValuePtr<LoxFn>,
    upvalues: Vec<*mut Upvalue>,
}
impl Closure {
    pub fn new(value: Value) -> Self {
        let func = match value { 
            Value::LoxFn(ptr) => ptr,
            _ => unreachable!(),
        };
        Self {
            func,
            upvalues: Vec::with_capacity(u8::MAX as usize),
        }
    }

    pub fn get_upvalue(&self, idx: usize) -> *mut Upvalue {
        self.upvalues[idx]
    }
    pub fn add_upvalue(&mut self, upvalue: *mut Upvalue) {
        self.upvalues.push(upvalue);
    }
    pub fn upvalue_count(&self) -> u8 {
        (*self.func).upvalue_count
    }
    pub fn chunk(&self) -> &Chunk {
        &(*self.func).chunk
    }
    pub fn arity(&self) -> u8 {
        (*self.func).arity
    }
}
impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", (*self.func))
    }
}
/*---------------------------------------------------------------------*/
#[derive(PartialEq, Clone)]
pub struct LoxFn {
    pub name: Option<String>,
    pub arity: u8,
    pub upvalue_count: u8,
    pub chunk: Chunk,
}
impl LoxFn {
    pub fn new() -> Self {
        LoxFn {
            name: None,
            arity: 0,
            chunk: Chunk::new(),
            upvalue_count: 0,
        }
    }
}
impl fmt::Display for LoxFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let default = String::from("script");
        let name = self.name.as_ref().unwrap_or(&default);
        write!(f, "<fn {}>", name)
    }
}
impl fmt::Debug for LoxFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LoxFn *{}", self)
    }
}
/*---------------------------------------------------------------------*/
pub struct NativeFn {
    pub name: String,
    pub arity: u8,
    pub func: Box<dyn Fn(&[Value]) -> Value>,
}

impl fmt::Display for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", &self.name)
    }
}
impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Native *{}", self)
    }
}
impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}
/*---------------------------------------------------------------------*/
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Value {
    Bool(bool),
    Nil(),
    Number(f64),
    Str(ValuePtr<String>),
    LoxFn(ValuePtr<LoxFn>),
    Closure(ValuePtr<Closure>),
    NativeFn(ValuePtr<NativeFn>),
}

impl Value {
    pub fn mark(&mut self) {
         match self {
            Value::Str(p) => p.mark(),
            Value::LoxFn(p) => p.mark(),
            Value::Closure(p) => p.mark(),
            Value::NativeFn(p) => p.mark(),
            _ => unreachable!(),
        }
    }
    pub fn str(&self) -> &str {
        match self {
            Value::Str(p) => p.str(),
            _ => unreachable!(),
        }
    }
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(b) if !b => true,
            Value::Nil() => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil() => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Str(p) => write!(f, "{}", p),
            Value::LoxFn(p) => write!(f, "{}", p),
            Value::NativeFn(p) => write!(f, "{}", p),
            Value::Closure(p) => write!(f, "{}", p),
        }
    }
}
