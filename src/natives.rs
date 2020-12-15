use crate::value::Value;
use std::rc::Rc;

pub fn clock(_: &[Rc<Value>]) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};
    Value::Number(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as f64)
}

pub fn println(arg: &[Rc<Value>]) -> Value {
    println!("{}", arg[0]);
    Value::Nil()
}

pub fn print(arg: &[Rc<Value>]) -> Value {
    print!("{}", arg[0]);
    Value::Nil()
}
