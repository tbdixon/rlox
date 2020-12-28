use crate::chunk::Chunk;
use crate::memory::ValuePtr;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum FunctionType {
    Function,
    Method,
    Initializer,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Upvalue {
    pub location: usize,
    pub closed: Option<Value>,
}
/*---------------------------------------------------------------------*/
#[derive(Debug, PartialEq)]
pub struct Closure {
    func: ValuePtr<LoxFn>,
    pub upvalues: Vec<*mut Upvalue>,
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
    pub fn func(&self) -> ValuePtr<LoxFn> {
        self.func
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
    pub kind: FunctionType,
}
impl LoxFn {
    pub fn new() -> Self {
        LoxFn {
            name: None,
            arity: 0,
            chunk: Chunk::new(),
            upvalue_count: 0,
            kind: FunctionType::Function,
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
        write!(f, "<fn {}>", self.name)
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
#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, ValuePtr<Closure>>,
}

impl Class {
    pub fn new(name: &str) -> Self {
        Class {
            name: name.to_string(),
            methods: HashMap::new(),
        }
    }
    pub fn add_method(&mut self, method: ValuePtr<Closure>) {
        self.methods.insert((*method).func().name.as_ref().unwrap().to_string(), method);
    }
}
impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {:?}", self.name, self.methods)
    }
}
/*---------------------------------------------------------------------*/
#[derive(Debug, PartialEq)]
pub struct Instance {
    pub class: ValuePtr<Class>,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: ValuePtr<Class>) -> Self {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }
    pub fn set_field(&mut self, field_name: &str, val: Value) {
        self.fields.insert(field_name.to_string(), val);
    }
}
impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} instance:{:?}", self.class.name, self.fields)
    }
}
/*---------------------------------------------------------------------*/
#[derive(Debug, PartialEq)]
pub struct BoundMethod {
    pub receiver: ValuePtr<Instance>,
    pub method: ValuePtr<Closure>,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: ValuePtr<Closure>) -> Self {
        let receiver = match receiver {
            Value::Instance(ptr) => ptr,
            _ => unreachable!(),
        };
        BoundMethod { receiver, method }
    }
}

impl fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "method {}", self.method)
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
    Class(ValuePtr<Class>),
    Instance(ValuePtr<Instance>),
    BoundMethod(ValuePtr<BoundMethod>),
}

impl Value {
    pub fn mark(&self) {
        match self {
            Value::Str(p) => {
                if crate::trace_gc() {
                    println!("marking Str {} @ {:p}", p, p)
                }
                p.mark();
            }
            Value::LoxFn(p) => {
                if crate::trace_gc() {
                    println!("marking LoxFn {} @ {:p}", p, p)
                }
                p.mark();
            }
            Value::Closure(p) => {
                if crate::trace_gc() {
                    println!("marking Closure {} @ {:p}", p, p)
                }
                p.mark();
            }
            Value::NativeFn(p) => {
                if crate::trace_gc() {
                    println!("marking NativeFn {} @ {:p}", p, p)
                }
                p.mark();
            }
            Value::Class(p) => {
                if crate::trace_gc() {
                    println!("marking Class {} @ {:p}", p, p)
                }
                p.mark();
            }
            Value::Instance(p) => {
                if crate::trace_gc() {
                    println!("marking Instance {} @ {:p}", p, p)
                }
                p.mark();
            }
            _ => (),
        }
    }
    pub fn str(&self) -> &str {
        match self {
            Value::Str(p) => p.str(),
            _ => unreachable!(),
        }
    }
    pub fn fields(&self) -> &HashMap<String, Value> {
        match self {
            Value::Instance(p) => &p.fields,
            _ => unreachable!(),
        }
    }
    pub fn methods(&self) -> &HashMap<String, ValuePtr<Closure>> {
        match self {
            Value::Instance(p) => &(p.class.methods),
            Value::Class(p) => &(p.methods),
            _ => unreachable!(),
        }
    }
    pub fn set_field(&mut self, field_name: &str, val: Value) {
        match self {
            Value::Instance(p) => (*p).set_field(field_name, val),
            _ => unreachable!(),
        }
    }
    pub fn add_method(&mut self, method: Value) {
        let method = match method {
            Value::Closure(ptr) => ptr,
            _ => unreachable!(),
        };
        match self {
            Value::Class(p) => (*p).add_method(method),
            _ => unreachable!(),
        }
    }

    pub fn print(&self) -> String {
        match self {
            Value::Str(p) => p.str().to_string(),
            _ => self.to_string(),
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
            Value::Class(p) => write!(f, "{}", p),
            Value::Instance(p) => write!(f, "{}", p),
            Value::BoundMethod(p) => write!(f, "{}", p),
        }
    }
}
