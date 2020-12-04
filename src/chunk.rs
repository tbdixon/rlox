use strum_macros::EnumIter;

// Type checking makes it (appropriately) trickier to switch between consider an
// OpCode as an enum and a generic byte. Having From implemented solves this in a
// relatively safe way except for the possibility that the conversion logic is missed
// since it is manually included. Try to guard against this with an appropriate test.
#[derive(Debug, Copy, Clone, EnumIter, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_UNKNOWN = 0xFF,
}

impl From<OpCode> for u8 {
    fn from(byte: OpCode) -> u8 {
        match byte {
            OpCode::OP_RETURN => 0x0,
            OpCode::OP_CONSTANT => 0x1,
            OpCode::OP_NEGATE => 0x2,
            OpCode::OP_ADD => 0x3,
            OpCode::OP_SUBTRACT => 0x4,
            OpCode::OP_MULTIPLY => 0x5,
            OpCode::OP_DIVIDE => 0x6,
            _ => 0xFF,
        }
    }
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0x0 => OpCode::OP_RETURN,
            0x1 => OpCode::OP_CONSTANT,
            0x2 => OpCode::OP_NEGATE,
            0x3 => OpCode::OP_ADD,
            0x4 => OpCode::OP_SUBTRACT,
            0x5 => OpCode::OP_MULTIPLY,
            0x6 => OpCode::OP_DIVIDE,
            _ => OpCode::OP_UNKNOWN,
        }
    }
}

pub type Value = f64;
type ConstantPool = Vec<Value>;
#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<i32>,
    pub constant_pool: ConstantPool,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            lines: Vec::new(),
            constant_pool: Vec::new(),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn test_opcode_translation() {
        for op_code in OpCode::iter() {
            let byte_repr: u8 = op_code.into();
            let opcode_repr = OpCode::from(byte_repr);
            assert_eq!(opcode_repr, op_code);
        }
    }
}
