use crate::value::*;

type ConstantPool = Vec<Value>;
#[derive(Debug, PartialEq, Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<i32>,
    pub constant_pool: ConstantPool,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::with_capacity(u8::MAX as usize),
            lines: Vec::with_capacity(u8::MAX as usize),
            constant_pool: Vec::with_capacity(u8::MAX as usize),
        }
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn write(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn update(&mut self, location: usize, byte: u8) {
        self.code[location] = byte;
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constant_pool.push(value);
        self.constant_pool.len() - 1
    }

    pub fn find_constant(&self, value: &Value) -> Option<usize> {
        for (idx, constant) in self.constant_pool.iter().enumerate() {
            if *constant == *value {
                return Some(idx);
            }
        }
        None
    }
}
