use itertools::multipeek;
use itertools::structs::MultiPeek;
use std::iter::Enumerate;
use std::str::Chars;
const EOF_CHAR: char = '\0';

pub struct Scanner<'a> {
    source: &'a str,
    source_itr: MultiPeek<Enumerate<Chars<'a>>>,
    start_idx: usize,
    line_num: i32,
}

trait MultiPeekExt {
    fn peek_nth(&mut self, n: usize) -> Option<(usize, char)>;
    fn peek_next(&mut self) -> Option<(usize, char)>;
    fn peek_two(&mut self) -> Option<(usize, char)>;
}

impl<I: Iterator<Item = (usize, char)>> MultiPeekExt for MultiPeek<I> {
    fn peek_nth(&mut self, n: usize) -> Option<(usize, char)> {
        let mut nth = self.peek();
        for _ in 0..n {
            nth = self.peek();
        }
        let nth = nth.copied();
        self.reset_peek();
        nth
    }

    // Returns the value *after* the one pointed
    // at by Iterator but does not consume
    fn peek_two(&mut self) -> Option<(usize, char)> {
        self.peek_nth(1)
    }

    // Returns the value pointed at by Iterator but
    // does not consume
    fn peek_next(&mut self) -> Option<(usize, char)> {
        self.peek_nth(0)
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,

    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF_CHAR,
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
        Token {
            kind: TOKEN_ERROR,
            lexeme: err_msg,
            line_num,
        }
    }

    pub fn eof() -> Token<'static> {
        Token {
            kind: TOKEN_EOF_CHAR,
            lexeme: "",
            line_num: -1,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TOKEN_EOF_CHAR
    }
}

impl Scanner<'_> {
    pub fn new(source: &str) -> Scanner {
        let source_itr = multipeek(source.chars().enumerate());
        Scanner {
            source,
            source_itr,
            start_idx: 0,
            line_num: 1,
        }
    }

    fn advance(&mut self) -> char {
        match self.source_itr.next() {
            Some(c) => c.1,
            None => EOF_CHAR,
        }
    }

    // Churns through whitespace and comments until next valid character
    // After this function the iterator will point to the first
    // character *after* whitespace.
    //
    // a =  // 5;
    //    x-----
    //   fn()
    // --^
    // Starting at x, this will move the iterator to 'f' on the next line.
    fn eat_whitespace_and_comments(&mut self) {
        while self.peek_char().is_whitespace() {
            if self.advance() == '\n' {
                self.line_num += 1;
            }
            if self.peek_char() == '/' && self.peek_two_chars() == '/' {
                self.source_itr
                    .find(|c| c.1 == '\n')
                    .unwrap_or((usize::MAX, EOF_CHAR))
                    .1;
                self.line_num += 1;
            }
        }
    }

    // A set of helper functions that handle unwrapping the Options that
    // come back from peek functionality.
    fn peek_two_chars(&mut self) -> char {
        match self.source_itr.peek_two() {
            Some(c) => c.1,
            None => EOF_CHAR,
        }
    }

    fn peek_char(&mut self) -> char {
        match self.source_itr.peek_next() {
            Some(c) => c.1,
            None => EOF_CHAR,
        }
    }

    // Returns usize::MAX for index if at the end of the iterator.
    // Presumably using usize::MAX is a safe sentinal value since
    // source code probably isn't 2^64 - 1 characters long.
    fn peek_idx(&mut self) -> usize {
        match self.source_itr.peek_next() {
            Some(c) => c.0,
            None => usize::MAX,
        }
    }

    fn current_lexeme(&mut self) -> &str {
        &self.source[self.start_idx..self.peek_idx()]
    }

    pub fn scan_token(&mut self) -> Token {
        self.eat_whitespace_and_comments();
        let c = self.advance();
        match c {
            EOF_CHAR => Token::eof(),
            _ => Token::error(self.line_num, "Invalid character"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_advance() {
        let source = "Hello World";
        let mut scanner = Scanner::new(source);
        let mut c: char;
        for (_, val) in source.chars().enumerate() {
            c = scanner.advance();
            assert_eq!(c, val);
        }
        c = scanner.advance();
        assert_eq!(c, EOF_CHAR);
    }

    #[test]
    fn test_eat_whitespace() {
        let source = "Hi  //Comment
                          Hello";
        let mut scanner = Scanner::new(source);
        scanner.advance();
        scanner.advance();
        scanner.eat_whitespace_and_comments();
        assert_eq!(scanner.peek_char(), 'H');
        assert_eq!(scanner.advance(), 'H');
        assert_eq!(scanner.line_num, 2);
    }

    #[test]
    fn test_peek() {
        let source = "C";
        let mut scanner = Scanner::new(source);
        assert_eq!(scanner.peek_char(), 'C');
        assert_eq!(scanner.peek_idx(), 0);
        scanner.advance();
        assert_eq!(scanner.peek_char(), EOF_CHAR);
        assert_eq!(scanner.peek_idx(), usize::MAX);
    }

    #[test]
    fn test_multi_peek() {
        let source = "Hello";
        let mut source_itr = multipeek(source.chars().enumerate());
        source_itr.next();
        assert_eq!(source_itr.peek_next().unwrap().1, 'e');
        assert_eq!(source_itr.peek_next().unwrap().1, 'e');
        assert_eq!(source_itr.peek_two().unwrap().1, 'l');
        assert_eq!(source_itr.peek_two().unwrap().1, 'l');
        assert_eq!(source_itr.peek_nth(3).unwrap().1, 'o');
        assert_eq!(source_itr.peek_nth(3).unwrap().1, 'o');
        assert_eq!(source_itr.peek_nth(10), None);
        assert_eq!(source_itr.peek_nth(3).unwrap().1, 'o');
    }
}
