use std::fmt;
use crate::chunk::Chunk;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct LoxClosure {
    pub func: Rc<LoxFn>,
}
impl fmt::Display for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure {}>", self.func)
    }
}

#[derive(PartialEq)]
pub struct LoxFn {
    pub name: Option<String>,
    pub arity: u8,
    pub chunk: Chunk,
}
impl LoxFn {
    pub fn new() -> Self {
        LoxFn { name: None, arity: 0, chunk: Chunk::new() }
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

pub struct NativeFn {
    pub name: String,
    pub arity: u8,
    pub func: Box<dyn Fn(&[Rc<Value>]) -> Value>
}
impl fmt::Display for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", &self.name)
    }
}impl fmt::Debug for NativeFn {
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
    SCRIPT
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil(),
    Number(f64),
    Str(String),
    Function(Rc<LoxFn>),
    NativeFunction(NativeFn),
    Closure(Rc<LoxClosure>),
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


