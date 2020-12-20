use crate::chunk::Chunk;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Upvalue {
    pub location: usize,
    pub closed: Option<Value>
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoxClosure {
    pub func: *const LoxFn,
    pub upvalues: Vec<*mut Upvalue>,
}

impl LoxClosure {
    pub fn new(func: *const LoxFn) -> Self {
        Self {
            func,
            upvalues: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl fmt::Display for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { write!(f, "{:?}", (*self.func)) }
    }
}

#[derive(PartialEq, Clone)]
pub struct LoxFn {
    pub name: Option<String>,
    pub arity: u8,
    pub chunk: Chunk,
    pub upvalue_count: u8,
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

#[derive(Clone)]
pub struct NativeFn {
    pub name: String,
    pub arity: u8,
    pub func: *const (), // *const () to enable derive(Clone). These need to be dyn Fn(&[Value]) -> Value
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

pub enum FunctionType {
    FUNCTION,
    SCRIPT,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil(),
    Number(f64),
    Str(String),
    Function(LoxFn),
    Closure(LoxClosure),
    NativeFunction(NativeFn),
}

impl Value {
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
            Value::Str(s) => write!(f, "{}", s),
            Value::Function(s) => write!(f, "{}", s),
            Value::NativeFunction(s) => write!(f, "{}", s),
            Value::Closure(s) => write!(f, "{}", s),
        }
    }
}
