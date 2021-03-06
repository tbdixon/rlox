use strum_macros::EnumIter;

// Type checking makes it trickier to switch between consider an
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
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_POP,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_PRINT,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_SET_UPVALUE,
    OP_GET_UPVALUE,
    OP_CLOSE_UPVALUE,
    OP_CLASS,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_METHOD,
    OP_INHERIT,
    OP_GET_SUPER,
    OP_UNKNOWN = 0xFF,
}
use crate::opcode::OpCode::*;

impl From<OpCode> for u8 {
    fn from(byte: OpCode) -> u8 {
        match byte {
            OP_RETURN => 0x0,
            OP_CONSTANT => 0x1,
            OP_NEGATE => 0x2,
            OP_ADD => 0x3,
            OP_SUBTRACT => 0x4,
            OP_MULTIPLY => 0x5,
            OP_DIVIDE => 0x6,
            OP_NIL => 0x7,
            OP_TRUE => 0x8,
            OP_FALSE => 0x9,
            OP_NOT => 0xA,
            OP_EQUAL => 0xB,
            OP_GREATER => 0xC,
            OP_LESS => 0xD,
            OP_POP => 0xE,
            OP_GET_GLOBAL => 0xF,
            OP_DEFINE_GLOBAL => 0x10,
            OP_SET_GLOBAL => 0x11,
            OP_PRINT => 0x12,
            OP_GET_LOCAL => 0x13,
            OP_SET_LOCAL => 0x14,
            OP_JUMP => 0x15,
            OP_JUMP_IF_FALSE => 0x16,
            OP_LOOP => 0x17,
            OP_CALL => 0x18,
            OP_CLOSURE => 0x19,
            OP_SET_UPVALUE => 0x1A,
            OP_GET_UPVALUE => 0x1B,
            OP_CLOSE_UPVALUE => 0x1C,
            OP_CLASS => 0x1D,
            OP_GET_PROPERTY => 0x1E,
            OP_SET_PROPERTY => 0x1F,
            OP_METHOD => 0x20,
            OP_INHERIT => 0x21,
            OP_GET_SUPER => 0x22,
            _ => 0xFF,
        }
    }
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0x0 => OP_RETURN,
            0x1 => OP_CONSTANT,
            0x2 => OP_NEGATE,
            0x3 => OP_ADD,
            0x4 => OP_SUBTRACT,
            0x5 => OP_MULTIPLY,
            0x6 => OP_DIVIDE,
            0x7 => OP_NIL,
            0x8 => OP_TRUE,
            0x9 => OP_FALSE,
            0xA => OP_NOT,
            0xB => OP_EQUAL,
            0xC => OP_GREATER,
            0xD => OP_LESS,
            0xE => OP_POP,
            0xF => OP_GET_GLOBAL,
            0x10 => OP_DEFINE_GLOBAL,
            0x11 => OP_SET_GLOBAL,
            0x12 => OP_PRINT,
            0x13 => OP_GET_LOCAL,
            0x14 => OP_SET_LOCAL,
            0x15 => OP_JUMP,
            0x16 => OP_JUMP_IF_FALSE,
            0x17 => OP_LOOP,
            0x18 => OP_CALL,
            0x19 => OP_CLOSURE,
            0x1A => OP_SET_UPVALUE,
            0x1B => OP_GET_UPVALUE,
            0x1C => OP_CLOSE_UPVALUE,
            0x1D => OP_CLASS,
            0x1E => OP_GET_PROPERTY,
            0x1F => OP_SET_PROPERTY,
            0x20 => OP_METHOD,
            0x21 => OP_INHERIT,
            0x22 => OP_GET_SUPER,
            _ => OP_UNKNOWN,
        }
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
