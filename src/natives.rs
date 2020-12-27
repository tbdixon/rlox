use crate::value::Value;

pub fn clock(_: &[Value]) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};
    Value::Number(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as f64)
}

pub fn println(arg: &[Value]) -> Value {
    println!("{}", arg[0].print());
    Value::Nil()
}

pub fn print(arg: &[Value]) -> Value {
    print!("{}", arg[0].print());
    Value::Nil()
}
