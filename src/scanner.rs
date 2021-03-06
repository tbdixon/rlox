use itertools::multipeek;
use itertools::structs::MultiPeek;
use std::error::Error;
use std::fmt;
use std::iter::Enumerate;
use std::str::Chars;

const EOF_CHAR: char = '\0';
const EOF_IDX: usize = usize::MAX;
const EOF_MARKER: SourceEnumItem = (EOF_IDX, EOF_CHAR);

pub struct Scanner<'a> {
    source: &'a str,
    source_itr: MultiPeek<Enumerate<Chars<'a>>>,
    start_idx: usize,
    line_num: i32,
}

// Basic type wrapper and helper functions to get at the index and raw characters while iterating
// through the source code.
type SourceEnumItem = (usize, char);
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

// Small helper so that you can call 'a'.is_lox_alpha() on a native char type
trait CharExt {
    fn is_lox_alpha(&self) -> bool;
}
impl CharExt for char {
    fn is_lox_alpha(&self) -> bool {
        self.is_ascii_alphabetic() || *self == '_'
    }
}

// Implementing an extension on MultiPeek for easier iteration through the source code. Standard
// peek can only look ahead a single character, scanning requires slightly more look ahead.
trait MultiPeekExt {
    // Peek n ahead in the iterator
    fn peek_nth(&mut self, n: usize) -> Option<SourceEnumItem>;
    // Peek at the next item in iterator
    fn peek_next(&mut self) -> Option<SourceEnumItem>;
    // Peek 2 ahead in iterator -- wraps up nth but used frequently enough to create a function
    fn peek_two(&mut self) -> Option<SourceEnumItem>;
}

impl<I: Iterator<Item = SourceEnumItem>> MultiPeekExt for MultiPeek<I> {
    fn peek_nth(&mut self, n: usize) -> Option<SourceEnumItem> {
        // MultiPeek keeps a separate stack / pointer, to iterate through loop up to the nth spot
        // then return a copy of that element and reset the peek pointer.
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

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
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
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF,
    TOKEN_EMPTY, // Placeholder for Null token
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub line_num: i32,
}

use crate::scanner::TokenType::*;
impl Token {
    pub fn error(line_num: i32, lexeme: String) -> Token {
        Token {
            kind: TOKEN_ERROR,
            lexeme,
            line_num,
        }
    }

    pub fn empty() -> Token {
        Token {
            kind: TOKEN_EMPTY,
            lexeme: String::from(""),
            line_num: -1,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.kind == TOKEN_EMPTY
    }

    pub fn eof(line_num: i32) -> Token {
        Token {
            kind: TOKEN_EOF,
            lexeme: String::from(""),
            line_num,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TOKEN_EOF
    }

    pub fn is_error(&self) -> bool {
        self.kind == TOKEN_ERROR
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TOKEN_EMPTY,
            lexeme: String::from(""),
            line_num: -1,
        }
    }
}

pub struct TokenStream {
    tokens: Vec<Token>,
}

#[derive(Debug)]
pub struct TokenStreamError {
    msg: String,
}

impl Error for TokenStreamError {}
impl fmt::Display for TokenStreamError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error parsing: {}", self.msg)
    }
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> TokenStream {
        TokenStream { tokens }
    }

    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap()
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.last().unwrap_or(&Token::eof(-1)).clone()
    }

    pub fn match_(&mut self, expected: TokenType) -> bool {
        if expected == self.peek().kind {
            self.next();
            return true;
        }
        false
    }

    // Consumes and advances if there is a specific type of
    // token encountered e.g. to consume the closing ")" in a grouping
    // this will ensure that token is found and then advance beyond it.
    //
    // This requires the next token be expected type or else it is
    // an error. See match_advance for a less restrictive consumption.
    /*    fn consume(&mut self, expected: TokenType) -> crate::Result<()> {
        if self.peek().kind == expected {
            self.next();
        } else {
            Err("")
        }
    }*/

    pub fn expect(&mut self, expected: TokenType, msg: &str) -> crate::Result<Token> {
        match self.peek().kind {
            e if e == expected => Ok(self.next()),
            _ => Err(Box::new(TokenStreamError { msg: String::from(msg) })),
        }
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
                self.source_itr.find(|c| c.ch() == '\n').unwrap_or(EOF_MARKER).ch();
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
        Token {
            kind,
            lexeme: lexeme.to_string(),
            line_num,
        }
    }

    // Includes the quotation marks in the lexeme
    fn string(&mut self) -> Token {
        while self.peek_char() != '"' {
            if self.at_end() {
                return Token::error(self.line_num, "Unterminated string".to_string());
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
            "return" => TOKEN_RETURN,
            "super" => TOKEN_SUPER,
            "var" => TOKEN_VAR,
            "while" => TOKEN_WHILE,
            "false" => TOKEN_FALSE,
            "for" => TOKEN_FOR,
            "fun" => TOKEN_FUN,
            "this" => TOKEN_THIS,
            "true" => TOKEN_TRUE,
            _ => TOKEN_IDENTIFIER,
        }
    }

    pub fn scan(&mut self) -> TokenStream {
        let mut tokens = Vec::new();
        let mut token = self.scan_token();
        while !token.is_eof() {
            tokens.push(token);
            token = self.scan_token();
        }
        tokens.push(token);
        tokens.reverse();
        TokenStream::new(tokens)
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
            '!' => {
                if self.peek_char() == '=' {
                    self.advance();
                    self.make_token(TOKEN_BANG_EQUAL)
                } else {
                    self.make_token(TOKEN_BANG)
                }
            }
            '=' => {
                if self.peek_char() == '=' {
                    self.advance();
                    self.make_token(TOKEN_EQUAL_EQUAL)
                } else {
                    self.make_token(TOKEN_EQUAL)
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.advance();
                    self.make_token(TOKEN_GREATER_EQUAL)
                } else {
                    self.make_token(TOKEN_GREATER)
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.advance();
                    self.make_token(TOKEN_LESS_EQUAL)
                } else {
                    self.make_token(TOKEN_LESS)
                }
            }
            '"' => self.string(),
            ch if ch.is_ascii_digit() => self.number(),
            ch if ch.is_lox_alpha() => self.identifier(),
            EOF_CHAR => Token::eof(self.line_num),
            _ => self.make_token(TOKEN_ERROR),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let source = "and class else false for fun if nil or return super this true var while";
        let mut scanner = Scanner::new(source);
        let res = vec![
            Token {
                kind: TOKEN_AND,
                lexeme: "and".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_CLASS,
                lexeme: "class".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_ELSE,
                lexeme: "else".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_FALSE,
                lexeme: "false".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_FOR,
                lexeme: "for".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_FUN,
                lexeme: "fun".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_IF,
                lexeme: "if".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_NIL,
                lexeme: "nil".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_OR,
                lexeme: "or".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_RETURN,
                lexeme: "return".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_SUPER,
                lexeme: "super".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_THIS,
                lexeme: "this".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_TRUE,
                lexeme: "true".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_VAR,
                lexeme: "var".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_WHILE,
                lexeme: "while".to_string(),
                line_num: 1,
            },
        ];
        for r in res {
            assert_eq!(r, scanner.scan_token());
        }
        assert_eq!(
            scanner.scan_token(),
            Token {
                kind: TOKEN_EOF,
                lexeme: "".to_string(),
                line_num: 1
            }
        );
    }

    #[test]
    fn test_operators() {
        let source = "(){};.-+/*,";
        let mut scanner = Scanner::new(source);
        let res = vec![
            Token {
                kind: TOKEN_LEFT_PAREN,
                lexeme: "(".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_RIGHT_PAREN,
                lexeme: ")".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_LEFT_BRACE,
                lexeme: "{".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_RIGHT_BRACE,
                lexeme: "}".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_SEMICOLON,
                lexeme: ";".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_DOT,
                lexeme: ".".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_MINUS,
                lexeme: "-".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_PLUS,
                lexeme: "+".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_SLASH,
                lexeme: "/".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_STAR,
                lexeme: "*".to_string(),
                line_num: 1,
            },
            Token {
                kind: TOKEN_COMMA,
                lexeme: ",".to_string(),
                line_num: 1,
            },
        ];
        for r in res {
            assert_eq!(r, scanner.scan_token());
        }
    }

    #[test]
    fn test_number() {
        let source = "123 123.123";
        let mut scanner = Scanner::new(source);
        assert_eq!(
            Token {
                kind: TOKEN_NUMBER,
                lexeme: "123".to_string(),
                line_num: 1
            },
            scanner.scan_token()
        );
        assert_eq!(
            Token {
                kind: TOKEN_NUMBER,
                lexeme: "123.123".to_string(),
                line_num: 1
            },
            scanner.scan_token()
        );
    }

    #[test]
    fn test_identifier() {
        let source = "abc abc_123";
        let mut scanner = Scanner::new(source);
        assert_eq!(
            Token {
                kind: TOKEN_IDENTIFIER,
                lexeme: "abc".to_string(),
                line_num: 1
            },
            scanner.scan_token()
        );
        assert_eq!(
            Token {
                kind: TOKEN_IDENTIFIER,
                lexeme: "abc_123".to_string(),
                line_num: 1
            },
            scanner.scan_token()
        );
    }

    #[test]
    fn test_string() {
        let source = "\"Hello\"";
        let mut scanner = Scanner::new(source);
        let token = scanner.scan_token();
        assert_eq!(
            token,
            Token {
                kind: TOKEN_STRING,
                lexeme: "\"Hello\"".to_string(),
                line_num: 1
            }
        );
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
