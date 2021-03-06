use crate::chunk::Chunk;
use crate::opcode::OpCode::{self, *};
use crate::value::Value;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    println!("ADDRESS\t|LINE\t|OP_CODE\t|OPERANDS\t|VALUES");
    let mut offset: usize = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(&chunk, offset);
    }
    println!("== {} ==", name);
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04}\t{}\t", offset, chunk.lines[offset]);
    match OpCode::from(chunk.code[offset]) {
        OP_RETURN => simple_instruction(OP_RETURN, offset),
        OP_CONSTANT => constant_instruction(OP_CONSTANT, chunk, offset),
        OP_NEGATE => simple_instruction(OP_NEGATE, offset),
        OP_ADD => simple_instruction(OP_ADD, offset),
        OP_SUBTRACT => simple_instruction(OP_SUBTRACT, offset),
        OP_MULTIPLY => simple_instruction(OP_MULTIPLY, offset),
        OP_DIVIDE => simple_instruction(OP_DIVIDE, offset),
        OP_NIL => simple_instruction(OP_NIL, offset),
        OP_TRUE => simple_instruction(OP_TRUE, offset),
        OP_FALSE => simple_instruction(OP_FALSE, offset),
        OP_NOT => simple_instruction(OP_NOT, offset),
        OP_EQUAL => simple_instruction(OP_EQUAL, offset),
        OP_GREATER => simple_instruction(OP_GREATER, offset),
        OP_LESS => simple_instruction(OP_LESS, offset),
        OP_PRINT => simple_instruction(OP_PRINT, offset),
        OP_GET_GLOBAL => byte_instruction(OP_GET_GLOBAL, chunk, offset),
        OP_SET_GLOBAL => byte_instruction(OP_SET_GLOBAL, chunk, offset),
        OP_DEFINE_GLOBAL => simple_instruction(OP_DEFINE_GLOBAL, offset),
        OP_POP => simple_instruction(OP_POP, offset),
        OP_GET_LOCAL => byte_instruction(OP_GET_LOCAL, chunk, offset),
        OP_SET_LOCAL => byte_instruction(OP_SET_LOCAL, chunk, offset),
        OP_JUMP_IF_FALSE => jump_instruction(OP_JUMP_IF_FALSE, 1, chunk, offset),
        OP_JUMP => jump_instruction(OP_JUMP, 1, chunk, offset),
        OP_LOOP => jump_instruction(OP_LOOP, -1, chunk, offset),
        OP_CALL => byte_instruction(OP_CALL, chunk, offset),
        OP_CLASS => simple_instruction(OP_CLASS, offset),
        OP_CLOSURE => closure_instruction(OP_CLOSURE, chunk, offset),
        OP_GET_UPVALUE => byte_instruction(OP_GET_UPVALUE, chunk, offset),
        OP_SET_UPVALUE => byte_instruction(OP_SET_UPVALUE, chunk, offset),
        OP_GET_PROPERTY => constant_instruction(OP_GET_PROPERTY, chunk, offset),
        OP_SET_PROPERTY => constant_instruction(OP_SET_PROPERTY, chunk, offset),
        OP_CLOSE_UPVALUE => byte_instruction(OP_CLOSE_UPVALUE, chunk, offset),
        OP_METHOD => simple_instruction(OP_METHOD, offset),
        OP_INHERIT => simple_instruction(OP_INHERIT, offset),
        OP_GET_SUPER => byte_instruction(OP_GET_SUPER, chunk, offset),
        OP_UNKNOWN => {
            println!("Unknown Opcode Encountered");
            offset + 1
        }
    }
}

pub fn simple_instruction(op_code: OpCode, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    println!("{:?} ({:#04X?})", op_code, binary_code);
    offset + 1
}

pub fn jump_instruction(op_code: OpCode, sign: i8, chunk: &Chunk, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    let mut jump: u16 = chunk.code[offset + 1] as u16;
    jump <<= 8;
    jump |= chunk.code[offset + 2] as u16;
    let mut end_location = (offset as usize) + 3;
    if sign < 0 {
        end_location -= jump as usize;
    } else {
        end_location += jump as usize;
    }
    println!("{:?} ({:#04X?})\t{:04} -> {:04} ", op_code, binary_code, offset, end_location);
    offset + 3
}

pub fn byte_instruction(op_code: OpCode, chunk: &Chunk, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    let byte_instr: usize = chunk.code[offset + 1] as usize;
    println!("{:?} ({:#04X?})\t{:#04X?} ", op_code, binary_code, byte_instr,);
    offset + 2
}

pub fn closure_instruction(op_code: OpCode, chunk: &Chunk, mut offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    offset = offset + 1;
    let constant_addr: usize = chunk.code[offset] as usize;
    let constant_val = &chunk.constant_pool[constant_addr];
    println!("{:?} ({:#04X?})\t{:#04X?}\t{:?} ", op_code, binary_code, constant_addr, constant_val);
    let func = match constant_val {
        Value::LoxFn(ptr) => *ptr,
        _ => panic!("Error debugging"),
    };
    offset += 2;
    for _ in 0..func.upvalue_count {
        print!("{:04}\t\t", offset);
        let upval_type = if chunk.code[offset - 1] == 0x1 { "local" } else { "upvalue" };
        let upval_idx = chunk.code[offset];
        println!("|\t\t\t\t{}\t{} ", upval_type, upval_idx);
        offset += 2;
    }
    offset
}

pub fn constant_instruction(op_code: OpCode, chunk: &Chunk, offset: usize) -> usize {
    let binary_code: u8 = op_code.into();
    let constant_addr: usize = chunk.code[offset + 1] as usize;
    let constant_val = &chunk.constant_pool[constant_addr];
    println!("{:?} ({:#04X?})\t{:#04X?}\t{}", op_code, binary_code, constant_addr, constant_val);
    offset + 2
}
