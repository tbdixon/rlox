const EOF_CHAR: &str = "\0";

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    start_idx: usize,
    current_idx: usize,
    line_num: i32,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
  // Single-character tokens.
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,

  // One or two character tokens.
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,

  // Literals.
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

  // Keywords.
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

  TOKEN_ERROR,
  TOKEN_EOF_CHAR
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub line_num: i32,
}

use crate::scanner::TokenType::*;
impl Token<'_> {
    pub fn error(line_num: i32, err_msg: &str) -> Token {
        Token { kind: TOKEN_ERROR, lexeme: err_msg, line_num }
    }

    pub fn eof() -> Token<'static> {
        Token { kind: TOKEN_EOF_CHAR, lexeme: "", line_num: -1 }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TOKEN_EOF_CHAR
    }
}

impl Scanner<'_> {
    pub fn new(source: &str) -> Scanner {
        Scanner { source, start_idx: 0, current_idx: 0, line_num: 0}
    }

    fn advance(&mut self) -> &str {
        if self.current_idx < self.source.len() {
            self.current_idx += 1;
            &self.source[self.current_idx-1..self.current_idx]
        }
        else {
            EOF_CHAR
        }
    }

    pub fn scan_token(&mut self) -> Token {
        let c = self.advance();
        match c {
            EOF_CHAR => Token::eof(),
            _ => Token::error(1, "Hello World")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eof() {
        let source = "Hello World";
        let mut scanner = Scanner::new(source);
        let mut c = "";
        for (idx, val) in source.chars().enumerate() {
            c = scanner.advance();
            assert_eq!(c, val.to_string());
        }
        c = scanner.advance();
        assert_eq!(c, EOF_CHAR);
    }
}
