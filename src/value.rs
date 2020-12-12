use std::fmt;
use crate::chunk::Chunk;
#[derive(Debug, PartialEq, Clone)]
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
pub enum FunctionType {
    FUNCTION,
    SCRIPT
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    Nil(),
    Number(f64),
    Str(String),
    Function(LoxFn),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil() => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Function(s) => write!(f, "{}", s),
        }
    }
}


