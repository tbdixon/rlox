use crate::chunk::{Chunk, OpCode, Value};
use crate::scanner::TokenType::{self, *};
use crate::scanner::{Scanner, Token};
use crate::Result;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::mem::take;

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq)]
enum Precedence {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY,
}

use crate::compiler::Precedence::*;
impl Precedence {
    fn from_u8(precedence: u8) -> Precedence {
        match precedence {
            0 => PREC_NONE,
            1 => PREC_ASSIGNMENT, // =
            2 => PREC_OR,         // or
            3 => PREC_AND,        // and
            4 => PREC_EQUALITY,   // == !=
            5 => PREC_COMPARISON, // < > <= >=
            6 => PREC_TERM,       // + -
            7 => PREC_FACTOR,     // * /
            8 => PREC_UNARY,      // ! -
            9 => PREC_CALL,       // . ()
            10 => PREC_PRIMARY,
            _ => panic!("Unknown precedence: {}", precedence),
        }
    }
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
    parse_rules: ParseRules,
    previous: Token,
    current: Token,
    tokens: Vec<Token>,
    chunk: &'a mut Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl Parser<'_> {
    pub fn new(tokens: Vec<Token>, chunk: &mut Chunk, parse_rules: ParseRules) -> Parser {
        Parser {
            parse_rules,
            previous: Token::empty(),
            current: Token::empty(),
            tokens,
            chunk,
            had_error: false,
            panic_mode: false,
        }
    }

    // Helper to emit an error message for user consumption
    fn parse_error(&mut self, msg: &'static str) {
        self.had_error = true;
        if !self.panic_mode {
            let token = &self.previous;
            let location = if token.is_eof() {
                " at end".to_string()
            } else {
                format!("at {}", token.lexeme)
            };
            println!(
                "[line {}] Error {}: {}",
                self.previous.line_num, location, msg
            );
            self.panic_mode = true;
        }
    }

    // This handles looping through the list of scanned tokens.
    // If we are already at the end this function does nothing
    // so that previous and current tokens are effectively frozen
    // as the last two tokens in the input being parsed.
    //
    // Previous is the token that will next emit bytes, current is
    // the token that is next to be considered.
    fn advance(&mut self) {
        if self.tokens.len() > 0 {
            self.previous = take(&mut self.current);
            self.current = self.tokens.pop().unwrap_or(Token::error(
                self.previous.line_num,
                "Error encountered while parsing".to_string(),
            ));
            if self.current.is_error() {
                self.parse_error("Error encountered while parsing");
            }
        }
    }

    // Helper that consumes and advances if there is a specific type of
    // token encountered e.g. to consume the closing ")" in a grouping
    // this will ensure that token is found and then advance beyond it.
    fn consume(&mut self, expected: TokenType, msg: &'static str) {
        if self.current.kind == expected {
            self.advance();
        } else {
            self.parse_error(msg);
        }
    }

    // Functions to handle "emitting" values which writes them to the chunk
    // being borrowed by the parser.
    fn emit_byte(&mut self, byte: u8, line: i32) {
        self.chunk.write(byte, line);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8, line: i32) {
        self.chunk.write(b1, line);
        self.chunk.write(b2, line);
    }

    fn emit_constant(&mut self, value: Value) {
        let const_idx = self.chunk.add_constant(value);
        self.emit_bytes(
            OpCode::OP_CONSTANT as u8,
            const_idx as u8,
            self.current.line_num,
        );
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        if let Some(prefix_fn) = self.parse_rules.get_prefix(self.previous.kind) {
            prefix_fn(self);
        } else {
            self.parse_error("Expect expression");
        }
/*        println!(
            "Precedence: {:?}. Previous: {:?}. Current: {:?}",
            precedence, self.previous, self.current
        );
*/
        while precedence <= self.parse_rules.get_precedence(self.current.kind).unwrap() {
            self.advance();
            if let Some(infix_fn) = self.parse_rules.get_infix(self.previous.kind) {
                infix_fn(self);
            } else {
                self.parse_error("Expect expression");
            }
        }
    }
}

fn grouping(parser: &mut Parser) {
    expression(parser);
    parser.consume(TOKEN_RIGHT_PAREN, "Expect closing ')'");
}

fn binary(parser: &mut Parser) {
    let op = parser.previous.kind;
    let op_line = parser.previous.line_num;
    // We explicitly call this function based on the below operators being found.
    // Covering the _ case for exhausting but will never hit.
    let precedence = match op {
        TOKEN_PLUS | TOKEN_MINUS => PREC_TERM,
        TOKEN_STAR | TOKEN_SLASH => PREC_FACTOR,
        _ => PREC_NONE,
    };
    parser.parse_precedence(Precedence::from_u8(precedence as u8 + 1));
    match op {
        TOKEN_PLUS => parser.emit_byte(OpCode::OP_ADD as u8, op_line),
        TOKEN_MINUS => parser.emit_byte(OpCode::OP_SUBTRACT as u8, op_line),
        TOKEN_STAR => parser.emit_byte(OpCode::OP_MULTIPLY as u8, op_line),
        TOKEN_SLASH => parser.emit_byte(OpCode::OP_DIVIDE as u8, op_line),
        _ => parser.parse_error("Invalid binary operator"),
    }
}
fn number(parser: &mut Parser) {
    match parser.previous.lexeme.parse::<Value>() {
        Ok(constant_value) => {
            parser.emit_constant(constant_value);
        }
        Err(_) => {
            parser.parse_error("Error parsing number");
        }
    };
}

fn expression(parser: &mut Parser) {
    parser.parse_precedence(PREC_ASSIGNMENT);
}

fn unary(parser: &mut Parser) {
    let op = parser.previous.kind;
    let op_line = parser.previous.line_num;
    parser.parse_precedence(PREC_UNARY);
    match op {
        TOKEN_MINUS => parser.emit_byte(OpCode::OP_NEGATE as u8, op_line),
        _ => parser.parse_error("Invalid unary operator"),
    }
}

struct ParseRule {
    precedence: Precedence,
    prefix: Option<&'static dyn Fn(&mut Parser)>,
    infix: Option<&'static dyn Fn(&mut Parser)>,
}

struct ParseRules(HashMap<TokenType, ParseRule>);
impl ParseRules {
    pub fn new() -> ParseRules {
        let mut rules = ParseRules(HashMap::new());
        rules.0.insert(
            TOKEN_EOF,
            ParseRule {
                precedence: PREC_NONE,
                prefix: None,
                infix: None,
            },
        );
        rules.0.insert(
            TOKEN_NUMBER,
            ParseRule {
                precedence: PREC_NONE,
                prefix: Some(&number),
                infix: None,
            },
        );
        rules.0.insert(
            TOKEN_MINUS,
            ParseRule {
                precedence: PREC_TERM,
                prefix: Some(&unary),
                infix: Some(&binary),
            },
        );
        rules.0.insert(
            TOKEN_PLUS,
            ParseRule {
                precedence: PREC_TERM,
                prefix: None,
                infix: Some(&binary),
            },
        );
        rules.0.insert(
            TOKEN_STAR,
            ParseRule {
                precedence: PREC_FACTOR,
                prefix: None,
                infix: Some(&binary),
            },
        );
        rules.0.insert(
            TOKEN_SLASH,
            ParseRule {
                precedence: PREC_FACTOR,
                prefix: None,
                infix: Some(&binary),
            },
        );
        rules.0.insert(
            TOKEN_LEFT_PAREN,
            ParseRule {
                precedence: PREC_NONE,
                prefix: Some(&grouping),
                infix: None,
            },
        );
        rules.0.insert(
            TOKEN_RIGHT_PAREN,
            ParseRule {
                precedence: PREC_NONE,
                prefix: None,
                infix: None,
            },
        );
        rules
    }

    pub fn get(&self, kind: TokenType) -> Option<&ParseRule> {
        self.0.get(&kind)
    }

    pub fn get_prefix(&self, kind: TokenType) -> Option<&'static dyn Fn(&mut Parser)> {
        if let Some(parse_rule) = self.get(kind) {
            parse_rule.prefix
        } else {
            None
        }
    }
    pub fn get_infix(&self, kind: TokenType) -> Option<&'static dyn Fn(&mut Parser)> {
        if let Some(parse_rule) = self.get(kind) {
            parse_rule.infix
        } else {
            None
        }
    }

    pub fn get_precedence(&self, kind: TokenType) -> Option<Precedence> {
        if let Some(parse_rule) = self.get(kind) {
            Some(parse_rule.precedence)
        } else {
            None
        }
    }
}

// Entry point to compile source code.
// 1. Scan the code and create a vector of Tokens
// 2. Compile the tokens and emit bytecode onto the Chunk
pub fn compile(source: &str, chunk: &mut Chunk) -> Result<()> {
    let tokens = Scanner::new(source.trim()).scan();
    let mut parser = Parser::new(tokens, chunk, ParseRules::new());
    parser.advance();
    expression(&mut parser);
    parser.consume(TOKEN_EOF, "Expect end of expression");
    parser.emit_byte(OpCode::OP_RETURN as u8, parser.current.line_num);
    if parser.had_error {
        Err(Box::new(ParserError()))
    } else {
        Ok(())
    }
}
