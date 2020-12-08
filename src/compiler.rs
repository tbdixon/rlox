use crate::chunk::OpCode::*;
use crate::chunk::{Chunk, Value};
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
struct CompilerError();
impl Error for CompilerError {}
impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error compiling input source")
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
    //
    // If we are out of tokens but in the middle of compiling
    // an expression that is an error.
    fn advance(&mut self) {
        if self.tokens.len() > 0 {
            self.previous = take(&mut self.current);
            self.current = self.tokens.pop().unwrap_or(Token::error(
                self.previous.line_num,
                "Error encountered while compiling".to_string(),
            ));
            if self.current.is_error() {
                self.parse_error("Error encountered while compiling");
            }
        } else {
            self.parse_error("Unexpected EOF while compiling");
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

    // Similar to consume, but is not an error if the token does not 
    // match.
    fn match_token(&mut self, expected: TokenType) -> bool {
        if self.current.kind == expected {
            self.advance();
            return true;
        }
        return false;
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

    fn emit_constant(&mut self, value: Value) -> usize {
        let const_idx = self.chunk.add_constant(value);
        self.emit_bytes(OP_CONSTANT as u8, const_idx as u8, self.current.line_num);
        const_idx
    }

    // Play forward until a statement so we can try to being compilation again there.
    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.kind != TOKEN_EOF {
            if self.previous.kind == TOKEN_SEMICOLON {
                break;
            }
            else {
                self.advance();
            }
        }
    }
       
    // The magic function that handles everything. Parses the specific prefix function
    // for the previous token and then parses the following as infix expressions
    // if they are greater than or equal precedence. 
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        if let Some(prefix_fn) = self.parse_rules.get_prefix(self.previous.kind) {
            prefix_fn(self);
        } else {
            println!("{:?}", self.previous);
            self.parse_error("Expect expression for prefix");
        }
        //println!("Precedence: {:?}. Previous: {:?}. Current: {:?}",precedence, self.previous, self.current);
        while precedence <= self.parse_rules.get_precedence(self.current.kind).unwrap() {
            self.advance();
            if let Some(infix_fn) = self.parse_rules.get_infix(self.previous.kind) {
                infix_fn(self);
            } else {
                println!("{:?}", self.current);
                self.parse_error("Expect expression for infix");
            }
        }
    }
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* Functions that are used in the Parse Precedence map and called to 
 * actually compile the source code with a mutable refernce to the parser
 */
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
        TOKEN_EQUAL_EQUAL | TOKEN_BANG_EQUAL => PREC_EQUALITY,
        TOKEN_GREATER | TOKEN_LESS | TOKEN_GREATER_EQUAL | TOKEN_LESS_EQUAL => PREC_COMPARISON,
        TOKEN_VAR => PREC_ASSIGNMENT,
        _ => PREC_NONE,
    };
    parser.parse_precedence(Precedence::from_u8(precedence as u8 + 1));
    match op {
        TOKEN_PLUS => parser.emit_byte(OP_ADD as u8, op_line),
        TOKEN_MINUS => parser.emit_byte(OP_SUBTRACT as u8, op_line),
        TOKEN_STAR => parser.emit_byte(OP_MULTIPLY as u8, op_line),
        TOKEN_SLASH => parser.emit_byte(OP_DIVIDE as u8, op_line),
        TOKEN_EQUAL_EQUAL => parser.emit_byte(OP_EQUAL as u8, op_line),
        TOKEN_BANG_EQUAL => parser.emit_bytes(OP_EQUAL as u8, OP_NOT as u8, op_line),
        TOKEN_GREATER => parser.emit_byte(OP_GREATER as u8, op_line),
        TOKEN_LESS => parser.emit_byte(OP_LESS as u8, op_line),
        TOKEN_GREATER_EQUAL => parser.emit_bytes(OP_LESS as u8, OP_NOT as u8, op_line),
        TOKEN_LESS_EQUAL => parser.emit_bytes(OP_GREATER as u8, OP_NOT as u8, op_line),
        _ => parser.parse_error("Invalid binary operator"),
    }
}
fn number(parser: &mut Parser) {
    match parser.previous.lexeme.parse::<f64>() {
        Ok(constant_value) => {
            parser.emit_constant(Value::Number(constant_value));
        }
        Err(_) => {
            parser.parse_error("Error parsing number");
        }
    };
}

fn expression(parser: &mut Parser) {
    parser.parse_precedence(PREC_ASSIGNMENT);
}

fn declaration(parser: &mut Parser) {
    if parser.match_token(TOKEN_VAR) {
        var_declaration(parser);
    } else {
        statement(parser);
    }
    // If we hit an error parsing, synchronize to the next valid point. 
    // The execution will not happen but this enables compilation to continue
    // so we grab any additional errors. 
    if parser.panic_mode {
        parser.synchronize();
    }
}

fn var_declaration(parser: &mut Parser) {
    parser.consume(TOKEN_IDENTIFIER, "Expect variable name");
    parser.emit_constant(Value::Str(String::from(&parser.previous.lexeme)));
    if parser.match_token(TOKEN_EQUAL) {
        expression(parser);
    } else {
        parser.emit_byte(OP_NIL as u8, parser.previous.line_num);
    }
    parser.consume(TOKEN_SEMICOLON, "Expect ';' at end of statement");
    parser.emit_byte(OP_DEFINE_GLOBAL as u8, parser.previous.line_num);
}

fn statement(parser: &mut Parser) {
    if parser.match_token(TOKEN_PRINT) {
        print_stmt(parser);
    } else {
        expr_stmt(parser);
    };
}

fn expr_stmt(parser: &mut Parser) {
    let line_num = parser.current.line_num;
    expression(parser);
    parser.consume(TOKEN_SEMICOLON, "Expect ';' at end of statement");
    parser.emit_byte(OP_POP as u8, line_num);
}

fn print_stmt(parser: &mut Parser) {
    let line_num = parser.previous.line_num;
    expression(parser);
    parser.consume(TOKEN_SEMICOLON, "Expect ';' at end of statement");
    parser.emit_byte(OP_PRINT as u8, line_num);
}

fn unary(parser: &mut Parser) {
    let op = parser.previous.kind;
    let op_line = parser.previous.line_num;
    parser.parse_precedence(PREC_UNARY);
    match op {
        TOKEN_MINUS => parser.emit_byte(OP_NEGATE as u8, op_line),
        TOKEN_BANG => parser.emit_byte(OP_NOT as u8, op_line),
        _ => parser.parse_error("Invalid unary operator"),
    }
}

fn literal(parser: &mut Parser) {
    let op_line = parser.previous.line_num;
    match parser.previous.kind {
        TOKEN_FALSE => parser.emit_byte(OP_FALSE as u8, op_line),
        TOKEN_TRUE => parser.emit_byte(OP_TRUE as u8, op_line),
        TOKEN_NIL => parser.emit_byte(OP_NIL as u8, op_line),
        _ => {}
    }
}

fn identifier(parser: &mut Parser) {
    let identifier = String::from(&parser.previous.lexeme);
    let arg = parser.emit_constant(Value::Str(identifier));
    if parser.match_token(TOKEN_EQUAL) {
        expression(parser);
        parser.emit_bytes(OP_SET_GLOBAL as u8, arg as u8, parser.previous.line_num);
    } else {
        parser.emit_byte(OP_GET_GLOBAL as u8, parser.previous.line_num);
    }
}

fn string(parser: &mut Parser) {
    let lexeme = &parser.previous.lexeme;
    let string = String::from(&lexeme[1..lexeme.len() - 1]);
    parser.emit_constant(Value::Str(string));
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* A struct used to store the parse rules and precedence for each of the 
 * tokens in the Lox language. This wraps a map so that the Pratt parser
 * can call the appropriate function when it encounters a given token
 * as a prefix or infix. 
*/
struct ParseRule {
    precedence: Precedence,
    prefix: Option<&'static dyn Fn(&mut Parser)>,
    infix: Option<&'static dyn Fn(&mut Parser)>,
}

struct ParseRules(HashMap<TokenType, ParseRule>);
impl ParseRules {
    pub fn new() -> ParseRules {
        let mut rules = ParseRules(HashMap::new());
        rules.0.insert(TOKEN_EOF,           ParseRule {precedence: PREC_NONE,       prefix: None,            infix: None});
        rules.0.insert(TOKEN_NUMBER,        ParseRule {precedence: PREC_NONE,       prefix: Some(&number),   infix: None});
        rules.0.insert(TOKEN_LEFT_PAREN,    ParseRule {precedence: PREC_NONE,       prefix: Some(&grouping), infix: None});
        
        rules.0.insert(TOKEN_FALSE,         ParseRule {precedence: PREC_NONE,       prefix: Some(&literal), infix: None});
        rules.0.insert(TOKEN_TRUE,          ParseRule {precedence: PREC_NONE,       prefix: Some(&literal), infix: None});
        rules.0.insert(TOKEN_BANG,          ParseRule {precedence: PREC_NONE,       prefix: Some(&unary),   infix: None});
        rules.0.insert(TOKEN_NIL,           ParseRule {precedence: PREC_NONE,       prefix: Some(&literal), infix: None});
        rules.0.insert(TOKEN_RIGHT_PAREN,   ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_SEMICOLON,     ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});

        rules.0.insert(TOKEN_MINUS,         ParseRule {precedence: PREC_TERM,       prefix: Some(&unary),   infix: Some(&binary)});
        rules.0.insert(TOKEN_PLUS,          ParseRule {precedence: PREC_TERM,       prefix: Some(&unary),   infix: Some(&binary)});
        rules.0.insert(TOKEN_STAR,          ParseRule {precedence: PREC_FACTOR,     prefix: Some(&unary),   infix: Some(&binary)});
        rules.0.insert(TOKEN_SLASH,         ParseRule {precedence: PREC_FACTOR,     prefix: Some(&unary),   infix: Some(&binary)});

        rules.0.insert(TOKEN_EQUAL_EQUAL,   ParseRule {precedence: PREC_EQUALITY,   prefix: None,           infix: Some(&binary)});
        rules.0.insert(TOKEN_BANG_EQUAL,    ParseRule {precedence: PREC_EQUALITY,   prefix: None,           infix: Some(&binary)});

        rules.0.insert(TOKEN_GREATER,       ParseRule {precedence: PREC_COMPARISON, prefix: None,           infix: Some(&binary)});
        rules.0.insert(TOKEN_LESS,          ParseRule {precedence: PREC_COMPARISON, prefix: None,           infix: Some(&binary)});
        rules.0.insert(TOKEN_GREATER_EQUAL, ParseRule {precedence: PREC_COMPARISON, prefix: None,           infix: Some(&binary)});
        rules.0.insert(TOKEN_LESS_EQUAL,    ParseRule {precedence: PREC_COMPARISON, prefix: None,           infix: Some(&binary)});

        rules.0.insert(TOKEN_STRING,        ParseRule {precedence: PREC_NONE,       prefix: Some(&string),  infix: None});
        rules.0.insert(TOKEN_EQUAL,         ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_VAR,           ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_IDENTIFIER,    ParseRule {precedence: PREC_NONE,       prefix: Some(&identifier), infix: None});
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
    //println!("{:?}", parser.tokens);
    parser.advance();
    while parser.current.kind != TOKEN_EOF {
        declaration(&mut parser);
    }
    parser.emit_byte(OP_RETURN as u8, parser.current.line_num);
    println!("{:?}", parser.chunk);
    if parser.had_error {
        Err(Box::new(CompilerError()))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let source = "-(5+(15-3)/4*2);";
        let mut chunk = Chunk::new();
        compile(source, &mut chunk).unwrap();
        let res = Chunk {
            code: vec![1, 0, 1, 1, 1, 2, 4, 1, 3, 6, 1, 4, 5, 3, 2, 14, 0],
            lines: vec![1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            constant_pool: vec![
                Value::Number(5.0),
                Value::Number(15.0),
                Value::Number(3.0),
                Value::Number(4.0),
                Value::Number(2.0),
            ],
        };
        assert_eq!(chunk, res);
    }
}
