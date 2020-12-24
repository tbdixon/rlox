use crate::chunk::Chunk;
use crate::memory::{LoxObjectType, LoxObject, LoxHeap};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Upvalue {
    pub location: usize,
    pub closed: Option<Value>,
}
/*---------------------------------------------------------------------*/
#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub func: *const LoxFn,
    pub upvalues: Vec<*mut Upvalue>,
}
impl Closure {
    pub fn new(func: *const LoxFn) -> Self {
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
        unsafe { (*self.func).upvalue_count }
    }
    pub fn chunk(&self) -> &Chunk {
        unsafe { &(*self.func).chunk }
    }
}
impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { write!(f, "{:?}", (*self.func)) }
    }
}
/*---------------------------------------------------------------------*/
#[derive(PartialEq, Clone)]
pub struct LoxFn {
    pub name: Option<String>,
    pub arity: u8,
    pub upvalue_count: u8,
    pub chunk: Chunk,
    pub marked: bool,
}
impl LoxFn {
    pub fn new() -> Self {
        LoxFn {
            name: None,
            arity: 0,
            chunk: Chunk::new(),
            upvalue_count: 0,
            marked: false,
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil(),
    Number(f64),
    Str(LoxObject),
    LoxFn(LoxObject),
    Closure(LoxObject),
    NativeFn(LoxObject),
}
//TODO impl equal => ptr same means same, easy. 

impl Value {
    pub fn mark(&mut self) {
         match self {
            Value::Str(p) => p.mark(),
            Value::Function(p) => p.mark(),
            Value::Closure(p) => p.mark(),
            _ => {}
        }
    }
    pub fn to_str(&self) -> &str {
        match self {
            Value::Str(p) => p.r#ref(),
            _ => unreachable!(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Str(p) => p.r#ref().to_string(),
            _ => unreachable!(),
        }
    }

    pub fn function(&self) -> *const LoxFn {
        match self {
            Value::Closure(p) => unsafe { (*p.ptr()).func },
            _ => unreachable!(),
        }
    }

    pub fn closure(&self) -> &LoxClosure {
        match self {
            Value::Closure(p) => p.r#ref(),
            _ => unreachable!(),
        }
    }

    pub fn native(&self) -> &Box<dyn Fn(&[Value]) -> Value> {
        match self {
            Value::NativeFunction(p) => &p.r#ref().func,
            _ => unreachable!(),
        }
    }

    pub fn upvalue_count(&self) -> u8 {
        match self {
            Value::Function(p) => unsafe { (*p.ptr()).upvalue_count },
            _ => unreachable!(),
        }
    }

    pub fn arity(&self) -> u8 {
        match self {
            Value::Closure(_) => unsafe { (*self.function()).arity },
            _ => unreachable!(),
        }
    }

    pub fn chunk(&self) -> &Chunk {
        match self {
            Value::Closure(_) => unsafe { &(*self.function()).chunk },
            _ => unreachable!(),
        }
    }

    pub fn to_closure(&self) -> LoxClosure {
        match self {
            Value::Function(p) => LoxClosure::new(p.ptr()),
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

/* Direct methods to create heap allocated objects, these will not be tied to the
 * VM memory management and hence never garbage collected. For appropriate GC coverage
 * instantiate a LoxHeap struct and create values through those APIs*/
impl From<LoxObject> for Value {
    fn from(obj: LoxObject) -> Self {
        match obj.kind {
            LoxObjectType::Str => Value::Str(obj),
            LoxObjectType::LoxFn => Value::LoxFn(obj),
            LoxObjectType::Closure => Value::Closure(obj),
            LoxObjectType::NativeFn => Value::NativeFn(obj),
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
            Value::Function(p) => write!(f, "{}", p),
            Value::NativeFunction(p) => write!(f, "{}", p),
            Value::Closure(p) => write!(f, "{}", p),
        }
    }
}
