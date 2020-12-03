use itertools::multipeek;
use itertools::structs::MultiPeek;
use std::iter::Enumerate;
use std::str::Chars;

type SourceEnumItem = (usize,char);
const EOF_CHAR: char = '\0';
const EOF_IDX: usize = usize::MAX;
const EOF_MARKER: SourceEnumItem = (EOF_IDX, EOF_CHAR);

pub struct Scanner<'a> {
    source: &'a str,
    source_itr: MultiPeek<Enumerate<Chars<'a>>>,
    start_idx: usize,
    line_num: i32,
}

trait CharExt {
    fn is_lox_alpha(&self) -> bool;
}

impl CharExt for char {
    fn is_lox_alpha(&self) -> bool {
        self.is_ascii_alphabetic() || *self == '_'
    }
}

trait TupleExt {
    fn ch(&self) -> char;
    fn idx(&self) -> usize;
}

impl TupleExt for SourceEnumItem {
    fn ch(&self) -> char {
        self.1
    }

    fn idx(&self) -> usize {
        self.0
    }
}

trait MultiPeekExt {
    fn peek_nth(&mut self, n: usize) -> Option<SourceEnumItem>;
    fn peek_next(&mut self) -> Option<SourceEnumItem>;
    fn peek_two(&mut self) -> Option<SourceEnumItem>;
}

impl<I: Iterator<Item = SourceEnumItem>> MultiPeekExt for MultiPeek<I> {
    fn peek_nth(&mut self, n: usize) -> Option<SourceEnumItem> {
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
    fn peek_two(&mut self) -> Option<SourceEnumItem> {
        self.peek_nth(1)
    }

    // Returns the value pointed at by Iterator but
    // does not consume
    fn peek_next(&mut self) -> Option<SourceEnumItem> {
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

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub line_num: i32,
}

use crate::scanner::TokenType::*;
impl Token<'_> {
    pub fn error(line_num: i32, lexeme: &str) -> Token {
        Token {
            kind: TOKEN_ERROR,
            lexeme,
            line_num,
        }
    }

    pub fn eof(line_num:i32) -> Token<'static> {
        Token {
            kind: TOKEN_EOF_CHAR,
            lexeme: "",
            line_num,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TOKEN_EOF_CHAR
    }
}

// Scanner works by iterating over the source file as a Character iterator.
// The iterator points to the character that will be scanned *next* hence peek_char
// returns the next up character as well. While scanning this keeps track of the 
// start of a lexeme and loops to the end of that lexeme, storing that as a 
// string slice in a token returned to the compiler.
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

    // Moved the iterator to point to the next element and returns the 
    // current element that is pointed to by the iterator. If the 
    // iterator is at the end of the source then return the EOF markers.
    fn advance(&mut self) -> SourceEnumItem {
        self.source_itr.next().unwrap_or(EOF_MARKER)
    }

    fn at_end(&mut self) -> bool {
        self.peek_char() == EOF_CHAR
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
            if self.advance().ch() == '\n' {
                self.line_num += 1;
            }
            if self.peek_char() == '/' && self.peek_two_chars() == '/' {
                self.source_itr
                    .find(|c| c.ch() == '\n')
                    .unwrap_or(EOF_MARKER)
                    .ch();
                self.line_num += 1;
            }
        }
    }

    // A set of helper functions that handle unwrapping the Options that
    // come back from peek functionality.
    fn peek_char(&mut self) -> char {
        self.source_itr.peek_next().unwrap_or(EOF_MARKER).ch()
    }

    fn peek_two_chars(&mut self) -> char {
        self.source_itr.peek_two().unwrap_or(EOF_MARKER).ch()
    }

    // Returns EOF_IDX for index if at the end of the iterator.
    // Presumably using EOF_IDX is a safe sentinal value since
    // source code probably isn't 2^64 - 1 characters long.
    fn peek_idx(&mut self) -> usize {
        self.source_itr.peek_next().unwrap_or(EOF_MARKER).idx()
    }

    fn current_lexeme(&mut self) -> &str {
        // If we're at the end, we lose the iterator index so take
        // the min of "infinity" and the actual length of the original
        // source string.
        let end = std::cmp::min(self.source.len(), self.peek_idx());
        &self.source[self.start_idx..end]
    }

    fn make_token(&mut self, kind: TokenType) -> Token {
        let line_num = self.line_num;
        let lexeme = self.current_lexeme();
        Token{ kind, lexeme, line_num }
    }
    
    // Includes the quotation marks in the lexeme
    fn string(&mut self) -> Token {
        while self.peek_char() != '"' {
            if self.at_end() {
                return Token::error(self.line_num, "Unterminated string.");
            }
            if self.advance().ch() == '\n' {
                self.line_num += 1;
            }
        }
        self.advance();
        self.make_token(TOKEN_STRING)
    }

    fn number(&mut self) -> Token {
        while self.peek_char().is_ascii_digit() {
            self.advance();
        }
        if self.peek_char() == '.' && self.peek_two_chars().is_ascii_digit() {
            self.advance();
            while self.peek_char().is_ascii_digit() {
                self.advance();
            }
        }
        self.make_token(TOKEN_NUMBER)
    }

    fn identifier(&mut self) -> Token {
        while self.peek_char().is_lox_alpha() || self.peek_char().is_ascii_digit() {
           self.advance();
        }
        let identifier_type = self.identifier_type();
        self.make_token(identifier_type)
    }

    fn identifier_type(&mut self) -> TokenType {
        match &self.current_lexeme().to_lowercase()[..] {
            "and" => TOKEN_AND,
            "class" => TOKEN_CLASS,
            "else" => TOKEN_ELSE,
            "if" => TOKEN_IF,
            "nil" => TOKEN_NIL,
            "or" => TOKEN_OR,
            "print" => TOKEN_PRINT,
            "return" => TOKEN_RETURN,
            "super" => TOKEN_SUPER,
            "var" => TOKEN_VAR,
            "while" => TOKEN_WHILE,
            "false" => TOKEN_FALSE,
            "for" => TOKEN_FOR,
            "fun" => TOKEN_FUN,
            "this" => TOKEN_THIS,
            "true" => TOKEN_TRUE,
            _ => TOKEN_IDENTIFIER
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.eat_whitespace_and_comments();
        let (start_idx, ch) = self.advance();
        self.start_idx = start_idx;
        match ch {
            '(' => self.make_token(TOKEN_LEFT_PAREN),
            ')' => self.make_token(TOKEN_RIGHT_PAREN),
            '{' => self.make_token(TOKEN_LEFT_BRACE),
            '}' => self.make_token(TOKEN_RIGHT_BRACE),
            ';' => self.make_token(TOKEN_SEMICOLON),
            ',' => self.make_token(TOKEN_COMMA),
            '.' => self.make_token(TOKEN_DOT),
            '-' => self.make_token(TOKEN_MINUS),
            '+' => self.make_token(TOKEN_PLUS),
            '/' => self.make_token(TOKEN_SLASH),
            '*' => self.make_token(TOKEN_STAR),
            '"' => self.string(),
            ch if ch.is_ascii_digit() => self.number(), 
            ch if ch.is_lox_alpha() => self.identifier(),
            EOF_CHAR => Token::eof(self.line_num),
            _ => Token::error(self.line_num, self.current_lexeme()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
   
    #[test]
    fn test_keywords() {
        let source = "and class else false for fun if nil or print return super this true var while";
        let mut scanner = Scanner::new(source);
        let res = vec![
            Token{ kind: TOKEN_AND, lexeme: "and", line_num: 1 },
            Token{ kind: TOKEN_CLASS, lexeme: "class", line_num: 1 },
            Token{ kind: TOKEN_ELSE,lexeme: "else", line_num: 1 },
            Token{ kind: TOKEN_FALSE,lexeme: "false", line_num: 1 },
            Token{ kind: TOKEN_FOR,lexeme: "for", line_num: 1 },
            Token{ kind: TOKEN_FUN,lexeme: "fun", line_num: 1 },
            Token{ kind: TOKEN_IF,lexeme: "if", line_num: 1 },
            Token{ kind: TOKEN_NIL,lexeme: "nil", line_num: 1 },
            Token{ kind: TOKEN_OR,lexeme: "or", line_num: 1 },
            Token{ kind: TOKEN_PRINT,lexeme: "print", line_num: 1 },
            Token{ kind: TOKEN_RETURN,lexeme: "return", line_num: 1 },
            Token{ kind: TOKEN_SUPER,lexeme: "super", line_num: 1 },
            Token{ kind: TOKEN_THIS,lexeme: "this", line_num: 1 },
            Token{ kind: TOKEN_TRUE,lexeme: "true", line_num: 1 },
            Token{ kind: TOKEN_VAR,lexeme: "var", line_num: 1 },
            Token{ kind: TOKEN_WHILE,lexeme: "while", line_num: 1 },
        ];
        for r in res {
            assert_eq!(r, scanner.scan_token());
        }
        assert_eq!(scanner.scan_token(), Token{ kind: TOKEN_EOF_CHAR,lexeme: "", line_num: 1 });
    }

    #[test]
    fn test_operators() {
        let source = "(){};.-+/*,";
        let mut scanner = Scanner::new(source);
        let res = vec![
            Token{ kind: TOKEN_LEFT_PAREN, lexeme: "(", line_num: 1 },
            Token{ kind: TOKEN_RIGHT_PAREN, lexeme: ")", line_num: 1 },
            Token{ kind: TOKEN_LEFT_BRACE, lexeme: "{", line_num: 1 },
            Token{ kind: TOKEN_RIGHT_BRACE, lexeme: "}", line_num: 1 },
            Token{ kind: TOKEN_SEMICOLON, lexeme: ";", line_num: 1 },
            Token{ kind: TOKEN_DOT, lexeme: ".", line_num: 1 },
            Token{ kind: TOKEN_MINUS, lexeme: "-", line_num: 1 },
            Token{ kind: TOKEN_PLUS, lexeme: "+", line_num: 1 },
            Token{ kind: TOKEN_SLASH, lexeme: "/", line_num: 1 },
            Token{ kind: TOKEN_STAR, lexeme: "*", line_num: 1 },
            Token{ kind: TOKEN_COMMA, lexeme: ",", line_num: 1 }
        ];
        for r in res {
            assert_eq!(r, scanner.scan_token());
        }
    }

    #[test]
    fn test_number() {
        let source = "123 123.123";
        let mut scanner = Scanner::new(source);
        assert_eq!(Token{ kind: TOKEN_NUMBER, lexeme: "123", line_num: 1}, scanner.scan_token());
        assert_eq!(Token{ kind: TOKEN_NUMBER, lexeme: "123.123", line_num: 1}, scanner.scan_token());
    }
    
    #[test]
    fn test_identifier() {
        let source = "abc abc_123";
        let mut scanner = Scanner::new(source);
        assert_eq!(Token{ kind: TOKEN_IDENTIFIER, lexeme: "abc", line_num: 1}, scanner.scan_token());
        assert_eq!(Token{ kind: TOKEN_IDENTIFIER, lexeme: "abc_123", line_num: 1}, scanner.scan_token());
     }

    #[test]
    fn test_string() {
        let source = "\"Hello\"";
        let mut scanner = Scanner::new(source);
        let token = scanner.scan_token();
        assert_eq!(token, Token{ kind: TOKEN_STRING, lexeme: "\"Hello\"", line_num: 1 });
    }

    #[test]
    fn test_advance() {
        let source = "Hello World";
        let mut scanner = Scanner::new(source);
        let mut c: char;
        for (_, val) in source.chars().enumerate() {
            c = scanner.advance().ch();
            assert_eq!(c, val);
        }
        c = scanner.advance().ch();
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
        assert_eq!(scanner.advance().ch(), 'H');
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
        assert_eq!(scanner.peek_idx(), EOF_IDX);
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
