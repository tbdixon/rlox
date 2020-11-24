#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
    OP_UNKNOWN = 0xFF,
}

impl From<OpCode> for u8 {
    fn from(byte: OpCode) -> u8 {
        match byte {
            OpCode::OP_RETURN => 0x0,
            OpCode::OP_CONSTANT => 0x1,
            _ => 0xFF,
        }
    }
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0x0 => OpCode::OP_RETURN,
            0x1 => OpCode::OP_CONSTANT,
            _ => OpCode::OP_UNKNOWN,
        }
    }
}

type Value = f64;
type ConstantPool = Vec<Value>;
#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<i32>,
    pub constant_pool: ConstantPool,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk{ code:  Vec::new(), lines: Vec::new(), constant_pool: Vec::new() }
    }

    pub fn write(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constant_pool.push(value);
        self.constant_pool.len() - 1
    }
}
