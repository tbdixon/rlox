use crate::scanner::TokenType::{self, *};

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq)]
pub enum Precedence {
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

pub fn get_precedence(kind: TokenType) -> Precedence {
    match kind {
        TOKEN_EOF => PREC_NONE,
        TOKEN_NUMBER => PREC_NONE,
        TOKEN_LEFT_PAREN => PREC_CALL,
        TOKEN_STRING => PREC_NONE,
        TOKEN_EQUAL => PREC_NONE,
        TOKEN_VAR => PREC_NONE,
        TOKEN_IDENTIFIER => PREC_NONE,
        TOKEN_FALSE => PREC_NONE,
        TOKEN_TRUE => PREC_NONE,
        TOKEN_BANG => PREC_NONE,
        TOKEN_NIL => PREC_NONE,
        TOKEN_RIGHT_PAREN => PREC_NONE,
        TOKEN_SEMICOLON => PREC_NONE,
        TOKEN_COMMA => PREC_NONE,
        TOKEN_MINUS => PREC_TERM,
        TOKEN_PLUS => PREC_TERM,
        TOKEN_STAR => PREC_FACTOR,
        TOKEN_SLASH => PREC_FACTOR,
        TOKEN_EQUAL_EQUAL => PREC_EQUALITY,
        TOKEN_BANG_EQUAL => PREC_EQUALITY,
        TOKEN_GREATER => PREC_COMPARISON,
        TOKEN_LESS => PREC_COMPARISON,
        TOKEN_GREATER_EQUAL => PREC_COMPARISON,
        TOKEN_LESS_EQUAL => PREC_COMPARISON,
        TOKEN_OR => PREC_OR,
        TOKEN_AND => PREC_AND,
        _ => PREC_NONE,
    }
}

use self::Precedence::*;
impl Precedence {
    pub fn from_u8(precedence: u8) -> Self {
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


