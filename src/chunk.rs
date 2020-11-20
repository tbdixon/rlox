#[derive(Debug)]
// C-like enum that will start from 0x00 and is stored in a single byte
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
}

type Operands = [u8;8];
#[derive(Debug)]
pub struct Instruction {
    code: OpCode,
    line: i16,
    // A Vec<u8> adds 24 bytes to the Instruction structure, so having a constant
    // array of u8 seems more efficient.
    operands: Operands, 
}
type Chunk = Vec<Instruction>;

impl Instruction {
    pub fn new(code: OpCode, operands: Operands, line: i16) -> Instruction {
        Instruction {
            code,
            operands,
            line,
        }
    }
}
