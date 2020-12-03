use std::error::Error;
use std::fmt;
use std::collections::HashMap;
use crate::Result;
use crate::chunk::{Chunk, OpCode, Value};
use crate::scanner::{Scanner, TokenType, Token};

enum Precedence {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} 

#[derive(Debug)]
struct ParserError();
impl Error for ParserError {}
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error parsing input source")
    }
}

struct Parser<'a> {
    previous_token: Token,
    current_token: Token,
    chunk: &'a mut Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl Parser<'_> {
    pub fn new(chunk: &mut Chunk) -> Parser {
        Parser{ previous_token: Token::empty(), current_token: Token::empty(), chunk, had_error: false, panic_mode: false }
    }

    fn parse_error(&mut self, msg: &'static str) {
        self.had_error = true;
        if !self.panic_mode {
            let token = &self.current_token;
            let location = if token.is_eof() { " at end".to_string() } else { format!("at {}", token.lexeme) };
            println!("[line {}] Error {}: {}", self.current_token.line_num, location, msg);
            self.panic_mode = true;
        }
    }

    fn emit_byte(&mut self, byte: u8, line: i32) {
        self.chunk.write(byte, line);
    }
    
    fn emit_bytes(&mut self, b1: u8, b2: u8, line: i32) {
        self.chunk.write(b1, line);
        self.chunk.write(b2, line);
    }

    fn emit_constant(&mut self) {
        match self.current_token.lexeme.parse::<Value>() {
            Ok(constant_value) => {
                let const_idx = self.chunk.add_constant(constant_value);
                self.emit_bytes(OpCode::OP_CONSTANT as u8, const_idx as u8, self.current_token.line_num);
            }
            Err(_) => {
                self.parse_error("Error parsing number");
            }
        };
    }
    fn number(&mut self) {
        self.emit_constant();
    }

    fn unary(&mut self) {
        let op = self.previous_token.lexeme.to_string();
        let op_line = self.previous_token.line_num;
        self.number();
        self.emit_byte(OpCode::OP_NEGATE as u8, op_line);
    }
}

struct ParseRule {
    precedence: Precedence,
    prefix: &'static dyn Fn(),
    infix: &'static dyn Fn(),
}

struct ParseRules(HashMap<TokenType, ParseRule>);
impl ParseRules {
    pub fn new() -> ParseRules {
        let rules = ParseRules(HashMap::new());
        rules
    }
}

pub fn compile(source: &str, chunk: &mut Chunk) -> Result<()> {
    let mut scanner = Scanner::new(source.trim());
    let mut parser = Parser::new(chunk); 
    parser.previous_token = scanner.scan_token();
    parser.current_token = scanner.scan_token();
    parser.unary();
    parser.emit_byte(OpCode::OP_RETURN as u8, parser.current_token.line_num);
    if parser.had_error {
        Err(Box::new(ParserError()))
    }
    else {
        Ok(())
    }
}
