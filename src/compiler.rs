use crate::chunk::OpCode::*;
use crate::chunk::{Chunk, Value, };
use crate::scanner::TokenType::{self, *};
use crate::scanner::{Scanner, Token};
use crate::debug::disassemble_chunk;
use crate::Result;
use crate::stack::*;
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

#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: u8,
}

impl  std::cmp::PartialEq for Local {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

struct Compiler<'a> {
    tokens: Vec<Token>,
    chunk: &'a mut Chunk,
/*-----Parser-----------*/
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,
    parse_rules: ParseRules,
/*-----Variables---------*/
    locals: Stack<Local>,
    local_count: u8,
    scope_depth: u8,
}

impl Compiler<'_> {
    pub fn new(tokens: Vec<Token>, chunk: &mut Chunk) -> Compiler {
        Compiler {
            tokens,
            chunk,
            parse_rules: ParseRules::new(),
            previous: Token::empty(),
            current: Token::empty(),
            had_error: false,
            panic_mode: false,
            locals: Stack::new(),
            local_count: 0,
            scope_depth: 0,
        }
    }

    fn add_local(&mut self, name: String) -> usize {
        let local = Local { name, depth: self.scope_depth };
        self.local_count += 1;
        self.locals.push(local)
    }

    fn resolve_local(&self, token: &Token) -> Option<usize> {
        let variable_name = String::from(&token.lexeme);
        let local = Local { name: variable_name, depth: self.scope_depth };
        self.locals.find(local)
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }
     fn end_scope(&mut self) {
        self.scope_depth -= 1;
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

    fn peek(&mut self, expected: TokenType) -> bool {
        self.current.kind == expected
    }
    
    // Functions to handle "emitting" values which writes them to the chunk
    // being borrowed by the compiler.
    fn emit_byte(&mut self, byte: u8, line: i32) {
        self.chunk.write(byte, line);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8, line: i32) {
        self.chunk.write(b1, line);
        self.chunk.write(b2, line);
    }

    fn create_constant(&mut self, value: Value) -> usize {
        let const_idx = self.chunk.add_constant(value);
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
        let can_assign = precedence <= PREC_ASSIGNMENT;
        if let Some(prefix_fn) = self.parse_rules.get_prefix(self.previous.kind) {
            prefix_fn(self, can_assign);
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
        if can_assign && self.match_token(TOKEN_EQUAL) {
            self.parse_error("Invalid assignment target.");
        }
    }
}

/*----------------------------------------------------------------------
 Entry points to compile code
----------------------------------------------------------------------*/
fn declaration(compiler: &mut Compiler) {
    if compiler.match_token(TOKEN_VAR) {
        var_declaration(compiler);
    } else {
        statement(compiler);
    }
    // If we hit an error parsing, synchronize to the next valid point. 
    // Execution will not happen but this enables compilation to continue
    // so we grab any additional errors. 
    if compiler.panic_mode {
        compiler.synchronize();
    }
}

fn var_declaration(compiler: &mut Compiler) {
    // Parse the variable and add the name to constant table / local stack
    let var_idx = parse_variable(compiler);
    
    // Grab the value that the variable will be initialized as. Either there is
    // an = in which case parse the expression otherwise all variables start
    // out as Nil. Emit this onto the stack.
    if compiler.match_token(TOKEN_EQUAL) {
        expression(compiler);
    } else {
        compiler.emit_byte(OP_NIL as u8, compiler.previous.line_num);
    }

    // Define / Set the initial value of the variable 
    if compiler.scope_depth == 0 {
        compiler.emit_byte(OP_DEFINE_GLOBAL as u8, compiler.previous.line_num);
    } else {
        compiler.emit_bytes(OP_SET_LOCAL as u8, var_idx as u8, compiler.previous.line_num);
    }
    compiler.consume(TOKEN_SEMICOLON, "Expect ';' at end of statement");
}

fn parse_variable(compiler: &mut Compiler) -> usize {
    compiler.consume(TOKEN_IDENTIFIER, "Expect variable name");
    let variable_name = String::from(&compiler.previous.lexeme);
    if compiler.scope_depth == 0 {
        let const_idx = compiler.create_constant(Value::Str(variable_name));
        compiler.emit_bytes(OP_CONSTANT as u8, const_idx as u8, compiler.current.line_num);
        const_idx
    }
    else {
        compiler.add_local(variable_name)
    }
}

fn statement(compiler: &mut Compiler) {
    if compiler.match_token(TOKEN_PRINT) {
        print_stmt(compiler);
    } else if compiler.match_token(TOKEN_LEFT_BRACE) {
        compiler.begin_scope(); 
        block_stmt(compiler);
        compiler.end_scope(); 
    }
    else{
        expr_stmt(compiler);
    };
}

fn block_stmt(compiler: &mut Compiler) {
    // Keep looping so long as we don't hit the closing brace. If we hit EOF
    // that is an error that will be caught on the subsequent consume call. 
    while !compiler.peek(TOKEN_RIGHT_BRACE) && !compiler.peek(TOKEN_EOF) {
        declaration(compiler);
    }
    compiler.consume(TOKEN_RIGHT_BRACE, "Expect closing '}' at end of block");
}

fn expr_stmt(compiler: &mut Compiler) {
    let line_num = compiler.current.line_num;
    expression(compiler);
    compiler.consume(TOKEN_SEMICOLON, "Expect ';' at end of statement");
    compiler.emit_byte(OP_POP as u8, line_num);
}

fn print_stmt(compiler: &mut Compiler) {
    let line_num = compiler.previous.line_num;
    expression(compiler);
    compiler.consume(TOKEN_SEMICOLON, "Expect ';' at end of statement");
    compiler.emit_byte(OP_PRINT as u8, line_num);
}

fn expression(compiler: &mut Compiler) {
    compiler.parse_precedence(PREC_ASSIGNMENT);
}

/*----------------------------------------------------------------------
 Functions that are used in the Parse Precedence map and called to 
 actually compile the source code with a mutable refernce to the compiler
----------------------------------------------------------------------*/
fn grouping(compiler: &mut Compiler, _: bool) {
    expression(compiler);
    compiler.consume(TOKEN_RIGHT_PAREN, "Expect closing ')'");
}

fn binary(compiler: &mut Compiler) {
    let op = compiler.previous.kind;
    let op_line = compiler.previous.line_num;
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
    compiler.parse_precedence(Precedence::from_u8(precedence as u8 + 1));
    match op {
        TOKEN_PLUS => compiler.emit_byte(OP_ADD as u8, op_line),
        TOKEN_MINUS => compiler.emit_byte(OP_SUBTRACT as u8, op_line),
        TOKEN_STAR => compiler.emit_byte(OP_MULTIPLY as u8, op_line),
        TOKEN_SLASH => compiler.emit_byte(OP_DIVIDE as u8, op_line),
        TOKEN_EQUAL_EQUAL => compiler.emit_byte(OP_EQUAL as u8, op_line),
        TOKEN_BANG_EQUAL => compiler.emit_bytes(OP_EQUAL as u8, OP_NOT as u8, op_line),
        TOKEN_GREATER => compiler.emit_byte(OP_GREATER as u8, op_line),
        TOKEN_LESS => compiler.emit_byte(OP_LESS as u8, op_line),
        TOKEN_GREATER_EQUAL => compiler.emit_bytes(OP_LESS as u8, OP_NOT as u8, op_line),
        TOKEN_LESS_EQUAL => compiler.emit_bytes(OP_GREATER as u8, OP_NOT as u8, op_line),
        _ => compiler.parse_error("Invalid binary operator"),
    }
}
fn number(compiler: &mut Compiler, _: bool) {
    match compiler.previous.lexeme.parse::<f64>() {
        Ok(constant_value) => {
            let const_idx = compiler.create_constant(Value::Number(constant_value));
            compiler.emit_bytes(OP_CONSTANT as u8, const_idx as u8, compiler.current.line_num);
        }
        Err(_) => {
            compiler.parse_error("Error parsing number");
        }
    };
}

fn unary(compiler: &mut Compiler, _: bool) {
    let op = compiler.previous.kind;
    let op_line = compiler.previous.line_num;
    compiler.parse_precedence(PREC_UNARY);
    match op {
        TOKEN_MINUS => compiler.emit_byte(OP_NEGATE as u8, op_line),
        TOKEN_BANG => compiler.emit_byte(OP_NOT as u8, op_line),
        _ => compiler.parse_error("Invalid unary operator"),
    }
}

fn literal(compiler: &mut Compiler, _: bool) {
    let op_line = compiler.previous.line_num;
    match compiler.previous.kind {
        TOKEN_FALSE => compiler.emit_byte(OP_FALSE as u8, op_line),
        TOKEN_TRUE => compiler.emit_byte(OP_TRUE as u8, op_line),
        TOKEN_NIL => compiler.emit_byte(OP_NIL as u8, op_line),
        _ => {}
    }
}

fn identifier(compiler: &mut Compiler, can_assign: bool) {
    let op_set;
    let op_get;
    let arg;
    match compiler.resolve_local(&compiler.previous) {
        Some(v) => {
            arg = v;
            op_set = OP_SET_LOCAL;
            op_get = OP_GET_LOCAL;
        },
        _ => {
            let identifier = compiler.previous.lexeme.to_string();
            arg = compiler.create_constant(Value::Str(identifier));
            op_set = OP_SET_GLOBAL;
            op_get = OP_GET_GLOBAL;
        }
    };
    if can_assign && compiler.match_token(TOKEN_EQUAL) {
        expression(compiler);
        compiler.emit_bytes(op_set as u8, arg as u8, compiler.previous.line_num);
    } else {
        compiler.emit_bytes(op_get as u8, arg as u8, compiler.previous.line_num);
    }
}

fn string(compiler: &mut Compiler, _: bool) {
    let lexeme = &compiler.previous.lexeme;
    let string = String::from(&lexeme[1..lexeme.len() - 1]);
    let const_idx = compiler.create_constant(Value::Str(string));
    compiler.emit_bytes(OP_CONSTANT as u8, const_idx as u8, compiler.previous.line_num);
}

/*----------------------------------------------------------------------
 * A struct used to store the parse rules and precedence for each of the 
 * tokens in the Lox language. This wraps a map so that the Pratt compiler
 * can call the appropriate function when it encounters a given token
 * as a prefix or infix. 
----------------------------------------------------------------------*/
struct ParseRule {
    precedence: Precedence,
    prefix: Option<&'static dyn Fn(&mut Compiler, bool)>,
    infix: Option<&'static dyn Fn(&mut Compiler)>,
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

    pub fn get_prefix(&self, kind: TokenType) -> Option<&'static dyn Fn(&mut Compiler, bool)> {
        if let Some(parse_rule) = self.get(kind) {
            parse_rule.prefix
        } else {
            None
        }
    }
    pub fn get_infix(&self, kind: TokenType) -> Option<&'static dyn Fn(&mut Compiler)> {
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
    let mut compiler = Compiler::new(tokens, chunk);
    compiler.advance();
    while compiler.current.kind != TOKEN_EOF {
        declaration(&mut compiler);
    }
    compiler.emit_byte(OP_RETURN as u8, compiler.current.line_num);
    if crate::DEBUG {
        disassemble_chunk(compiler.chunk, "Compiling Complete");
    }

    if compiler.had_error {
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
