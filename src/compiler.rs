use crate::chunk::OpCode::{self, *};
use crate::value::{Value, LoxFn};
use crate::chunk::{Chunk};
use crate::scanner::TokenType::{self, *};
use crate::scanner::{Scanner, Token};
use crate::debug::disassemble_chunk;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::mem::take;
use std::rc::Rc;

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
pub struct CompilerError();
impl Error for CompilerError {}
impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error compiling input source")
    }
}

#[derive(Debug, Clone)]
struct Local {
    name: String,
    is_captured: bool,
    depth: u8,
}

struct Upvalue {
    idx: usize,
    is_local: bool
}

impl  std::cmp::PartialEq for Local {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

struct Compiler {
    // Tokens from the scanning phase
    tokens: Vec<Token>,
    // Parsing variables
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,
    parse_rules: ParseRules,
    // Compiler is always in a function. At the top level this is the script itself. 
    // These functions are represented as a stack, along with the locals, scope depth 
    // and upvalues for each function. Starting a new function pushes a new stack of locals 
    // and scope depths, popping the function off (completing compiling) pops the parallel 
    // locals and scope depths. 
    functions: Vec<LoxFn>, 
    locals: Vec<Vec<Local>>,
    upvalues: Vec<Vec<Upvalue>>,
    scope_depths: Vec<u8>,
}

fn create_jump_offset(jump_offset: usize) -> (u8,u8) {
        let jump_u16 = jump_offset as u16;
        let high_bits = ((jump_u16 >> 8) & 0xff) as u8;
        let low_bits = (jump_u16 & 0xff) as u8;
        (high_bits, low_bits)
}
 
impl Compiler {
    pub fn new(tokens: Vec<Token>) -> Compiler {
        let mut locals = Vec::new();
        locals.push(Local{ name: String::from(""), depth: 0, is_captured: false });
        Compiler {
            tokens,
            functions: vec![LoxFn::new()],
            locals: vec![locals],
            upvalues: vec![Vec::new()],
            scope_depths: vec![0],
            parse_rules: ParseRules::new(),
            previous: Token::empty(),
            current: Token::empty(),
            had_error: false,
            panic_mode: false,
       }
    }

    fn start_function(&mut self) {
        let mut function = LoxFn::new();
        let mut locals = Vec::new();

        let name = String::from(&self.previous.lexeme);
        locals.push(Local{ name: name.clone(), depth: 0, is_captured: false });

        function.name = Some(name);
        self.functions.push(function);
        self.locals.push(locals);
        self.scope_depths.push(0);
    }

    fn end_function(&mut self) -> LoxFn {
        self.scope_depths.pop();
        self.locals.pop();
        self.functions.pop().unwrap()
    }

    // A number of functions that help with the indirection around a vectors and stacks for
    // consistent handling of Option / Results and add granularity with mutable and immutable borrows
    // going into the functions as required. 
    //
    // TODO: replace unwrap with ok_or_else or something similar
    fn increment_arity(&mut self) {
        self.functions.last_mut().unwrap().arity += 1;
    }
    
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.functions.last_mut().unwrap().chunk
    }

    fn increment_depth(&mut self) {
        *self.scope_depths.last_mut().unwrap() += 1;
    }

    fn decrement_depth(&mut self) {
        *self.scope_depths.last_mut().unwrap() -= 1;
    }

    fn depth(&self) -> u8 {
        *self.scope_depths.last().unwrap()
    }

    fn push_local(&mut self, local: Local) -> usize {
        self.locals.last_mut().unwrap().push(local);
        self.locals.last_mut().unwrap().len() - 1
    }

    fn find_local(&self, needle: &str) -> Option<usize> {
        // Locals is a Vec<Vec<Value>>, the last element ties to the current compiler function.
        let locals = self.locals.last().unwrap();
        for (idx,var) in locals.iter().rev().enumerate() {
            if var.name == needle {
                return Some(locals.len() - idx - 1);
            }
        }
        None
    }

    fn peek_local(&self) -> Option<&Local> {
        self.locals.last().unwrap().last()
    }

    fn pop_local(&mut self) -> Local {
        self.locals.last_mut().unwrap().pop().unwrap()
    }

    //TODO: This does not appropriately handle shadowed variables like below
    // var a = "outer" {
    //  var a = a;
    // }
    fn add_local(&mut self, name: String) -> usize {
        let local = Local { name, depth: self.depth(), is_captured: false };
        self.push_local(local)
    }

    // Looks for and returns the index of a local variable by name starting in current
    // depth / scope and working backwards. None if not found. 
    fn resolve_local(&self, token: &Token) -> Option<usize> {
        self.find_local(&token.lexeme)
    }

    fn begin_scope(&mut self) {
        self.increment_depth();
    }

    // At the end of a scope we need to pop the locals off the stack
    fn end_scope(&mut self) {
        let mut current = self.peek_local();
        while let Some(local) = current {
            if local.depth == self.depth() {
                self.pop_local();
                self.emit_byte(OP_POP as u8, self.previous.line_num);
                current = self.peek_local();
            }
            else {
                current = None
            }
        }
        self.decrement_depth();
    }
     
    // Emit errors and set the flags to ensure a program with compiler 
    // errors does not run. Also sets the start of synchonrization to next 
    // valid spot in source code. 
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
                "Error reading token".to_string(),
            ));
            if self.current.is_error() {
                self.parse_error("Error encountered while compiling");
            }
        } else {
            self.parse_error("Unexpected EOF while compiling");
        }
    }

    // Consumes and advances if there is a specific type of
    // token encountered e.g. to consume the closing ")" in a grouping
    // this will ensure that token is found and then advance beyond it.
    //
    // This requires the next token be expected type or else it is 
    // an error. See match_advance for a less restrictive consumption.
    fn consume(&mut self, expected: TokenType, msg: &'static str) {
        if self.current.kind == expected {
            self.advance();
        } else {
            self.parse_error(msg);
        }
    }

    // Similar to consume, but is not an error if the token does not 
    // match.
    fn match_advance(&mut self, expected: TokenType) -> bool {
        if self.current.kind == expected {
            self.advance();
            return true;
        }
        return false;
    }

    fn peek(&mut self, expected: TokenType) -> bool {
        self.current.kind == expected
    }
    
    // Functions to handle "emitting" values which writes them to the 
    // chunk associated with the current function being compiled. 
    fn emit_byte(&mut self, byte: u8, line: i32) {
        self.chunk().write(byte, line);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8, line: i32) {
        self.chunk().write(b1, line);
        self.chunk().write(b2, line);
    }

    fn emit_return(&mut self) {
        let line = self.previous.line_num;
        self.emit_byte(OP_NIL as u8, line);
        self.emit_byte(OP_RETURN as u8, line);
    }
   
    // Emits bytecode to read a constant at const_idx onto the VM stack
    fn emit_read_constant(&mut self, const_idx: usize) {
       self.emit_bytes(OP_CONSTANT as u8, const_idx as u8, self.previous.line_num);
    }

    // A jump emits as a jump operation followed by two bytes of UNKNOWN
    // memory that will later be patched. This memory represents the jump 
    // *offset* that will be executed, not the specific address in memory to jump
    // to. 
    //
    // Returns the address to patch the high bits for jump instruction (B) below.
    //
    // [JUMP | HIGH_BITS | LOW_BITS | ...]
    //   A         B          C
    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit_byte(op as u8, self.current.line_num);
        self.emit_byte(OP_UNKNOWN as u8, self.current.line_num);
        self.emit_byte(OP_UNKNOWN as u8, self.current.line_num);
        self.chunk().count() - 2
    }

    // Per emit_jump this is the patching logic that replaces the UNKNOWN
    // code with the offset to jump. 
    //
    // Input is the address the patch (see emit_jump) and this is patched to jump
    // to the current memory location. That location is determined by the
    // chunk().count() [Total Instructions] - [Patch Addr] - 2. -2 is due to the 
    // actual VM moving the IP forward by 2 when reading the jump offset. 
    fn patch_jump(&mut self, addr: usize) {
        let (high_bits, low_bits) = create_jump_offset(self.chunk().count() - addr - 2);
        self.chunk().update(addr, high_bits);
        self.chunk().update(addr + 1, low_bits);
    }

    // Loops don't need to be patched, so single step takes in an address that represents the start
    // of a loop and emits OP_LOOP | HIGH BITS | LOW BITS.
    //
    // The offset has 3 added to it to make up for the IP moving forward in the VM by 3 when
    // actually reading the jump instruction + offset bytes. 
    fn emit_loop(&mut self, addr: usize) {
        let (high_bits, low_bits) = create_jump_offset(self.chunk().count() - addr + 3);
        self.emit_byte(OP_LOOP as u8, self.current.line_num);
        self.emit_byte(high_bits as u8, self.current.line_num);
        self.emit_byte(low_bits as u8, self.current.line_num);
    }


    // Creates a constant by putting it into the constant_pool on the code Chunk. If that constant
    // already exists in there just return the index into that. 
    fn create_constant(&mut self, value: Value) -> usize {
        match self.chunk().find_constant(&value) {
            Some(const_idx) => const_idx,
            _ => self.chunk().add_constant(Rc::new(value))
        }
    }

    // Play forward until a statement so we can try to being compilation again there.
    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.kind != TOKEN_EOF && self.previous.kind != TOKEN_SEMICOLON{
            self.advance();
        }
    }
       
    // The magic function that handles everything. Parses the specific prefix function
    // for the previous token and then parses the following as infix expressions
    // if they are greater than or equal precedence. 
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        // Check that the current precedene is lower than ASSIGNMENT precedence. This gets
        // plumbed through the parsing functions to determine if we can assign to a variable.
        //
        // a * b = c + d;
        // ----^
        //      a * b infix is too high precedence to assign, in other words 'b' should not be 
        //      treated as an assignment here. 
        let can_assign = precedence <= PREC_ASSIGNMENT;
        // The lookup for prefix function--always done first since when we hit a new token it is
        // by construction a prefix from its own perspective. 
        //
        // a = 1 + 2;
        //-----^
        //   prefix
        //-------^
        //     infix
        //---------^
        //       prefix
        if let Some(prefix_fn) = self.parse_rules.get_prefix(self.previous.kind) {
            prefix_fn(self, can_assign);
        } else {
            self.parse_error("Expect expression for prefix");
        }
        
//        println!("Precedence: {:?}. Previous: {:?}. Current: {:?}",precedence, self.previous, self.current);
        
        // So long as the precedence being parsed is lower than the next token we can continue
        // parsing that as an infix operator. After parsing the 4 (as a prefix and emitting an
        // OP_CONSTANT) we get to the '-' as an infix but this has a lower precedence than '*' so
        // we end parsing and compute 3 * 4 first (as expected).
        // a = 3 * 4 - 2 == 10 != 6
        //-----^
        //   prefix
        //-------^
        //     infix [MULT]
        //---------^
        //       prefix
        //-----------^
        //          infix [SUB]
        //-------------^
        //            prefix
        while precedence <= self.parse_rules.get_precedence(self.current.kind).unwrap() {
            self.advance();
            if let Some(infix_fn) = self.parse_rules.get_infix(self.previous.kind) {
                infix_fn(self);
            } else {
                println!("{:?}", self.current);
                self.parse_error("Expect expression for infix");
            }
        }
        // Final check to make sure didn't have some odd input that was written as an assignment
        // precedence not followed by '='.
        if can_assign && self.match_advance(TOKEN_EQUAL) {
            self.parse_error("Invalid assignment target.");
        }
    }
}

/*----------------------------------------------------------------------
 Entry points to compile code
----------------------------------------------------------------------*/
fn declaration(compiler: &mut Compiler) {
    if compiler.match_advance(TOKEN_VAR) {
        var_declaration(compiler);
    } else if compiler.match_advance(TOKEN_FUN) {
        fun_declaration(compiler);
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

fn fun_declaration(compiler: &mut Compiler) {
    let var_idx = parse_variable(compiler);
    function(compiler);
    if compiler.depth() == 0 {
        compiler.emit_byte(OP_DEFINE_GLOBAL as u8, compiler.previous.line_num);
    } else {
        compiler.emit_bytes(OP_SET_LOCAL as u8, var_idx as u8, compiler.previous.line_num);
    }
}

fn var_declaration(compiler: &mut Compiler) {
    // Parse the variable and add the name to constant table / local stack.
    let var_idx = parse_variable(compiler);
    
    // Grab the value that the variable will be initialized as. Either there is
    // an = in which case parse the expression otherwise all variables start
    // out as Nil. Emit this onto the stack.
    if compiler.match_advance(TOKEN_EQUAL) {
        expression(compiler);
    } else {
        compiler.emit_byte(OP_NIL as u8, compiler.previous.line_num);
    }

    // Define / Set the initial value of the variable 
    if compiler.depth() == 0 {
        compiler.emit_byte(OP_DEFINE_GLOBAL as u8, compiler.previous.line_num);
    } else {
        compiler.emit_bytes(OP_SET_LOCAL as u8, var_idx as u8, compiler.previous.line_num);
    }
    compiler.consume(TOKEN_SEMICOLON, "Expect ';' at end of variable declaration");
}

// If we are in a variable declaration, handles adding that variable name to either the constant
// pool (global) or into the local variable stack. Determines this simply based on the current
// scope_depth of the compiler.
fn parse_variable(compiler: &mut Compiler) -> usize {
    compiler.consume(TOKEN_IDENTIFIER, "Expect variable name");
    let variable_name = String::from(&compiler.previous.lexeme);
    if compiler.depth() == 0 {
        let const_idx = compiler.create_constant(Value::Str(variable_name));
        compiler.emit_read_constant(const_idx);
        const_idx
    }
    else {
        compiler.add_local(variable_name)
    }
}

// Basic switch for statement types.
fn statement(compiler: &mut Compiler) {
    if compiler.match_advance(TOKEN_RETURN) {
        return_statement(compiler);
    } else if compiler.match_advance(TOKEN_IF) {
        if_else_statement(compiler);
    } else if compiler.match_advance(TOKEN_WHILE) {
        while_(compiler);
    } else if compiler.match_advance(TOKEN_FOR) {
        for_(compiler);
    } else if compiler.match_advance(TOKEN_LEFT_BRACE) {
        compiler.begin_scope(); 
        block_stmt(compiler);
        compiler.end_scope(); 
    }
    else{
        expr_stmt(compiler);
    };
}

/*---------------------------------------------------------------------------------------
* Control flow functions
*---------------------------------------------------------------------------------------*/

fn while_(compiler: &mut Compiler) {
    // Save the current address to use later when emitting the loop instruction at the
    // end of the while loop.
    let loop_start_addr = compiler.chunk().count();
    compiler.consume(TOKEN_LEFT_PAREN, "Expect opening '(' at start of while loop");
    // Compile the loop condition (which will leave it at the top of the stack)
    expression(compiler);
    compiler.consume(TOKEN_RIGHT_PAREN, "Expect closing ')' at end of while loop");
    // Conditional jump if the loop expression is false--to be patched with the end of the loop
    // once body is compiled
    let loop_end = compiler.emit_jump(OP_JUMP_IF_FALSE);
    // If we don't jump, pop the loop condition result off the stack
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
    statement(compiler);
    // Emit the loop instruction back to the start
    compiler.emit_loop(loop_start_addr);
    // Patch the end of loop onto the conditional jump
    compiler.patch_jump(loop_end);
    // Pop the loop condition if we jumped to the end
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
}

fn for_(compiler: &mut Compiler) {
    compiler.begin_scope();
    compiler.consume(TOKEN_LEFT_PAREN, "Expect opening '(' at start of for loop");

    // Handle the initializer for(var i = 0;) for(i=0) for(;)
    if !compiler.match_advance(TOKEN_SEMICOLON) {
        if compiler.match_advance(TOKEN_VAR) {
            var_declaration(compiler);
        } else {
            expr_stmt(compiler);
        }
    }
    
    // Condition check
    let mut loop_start_addr = compiler.chunk().count();
    let mut exit_jump = None; // OP_JUMP_NOT_EQUAL at the start of the loop
    let mut condition_start = None; // OP_LOOP to the condition if required (e.g. if there is an increment)
    if !compiler.match_advance(TOKEN_SEMICOLON) {
        condition_start = Some(compiler.chunk().count());
        expression(compiler);
        exit_jump = Some(compiler.emit_jump(OP_JUMP_IF_FALSE));
        compiler.emit_byte(OP_POP as u8,compiler.current.line_num);
        compiler.consume(TOKEN_SEMICOLON, "Expect opening ';' after loop condition");
    } 
    
    if !compiler.match_advance(TOKEN_RIGHT_PAREN) {
        let condition_end_jump = compiler.emit_jump(OP_JUMP);
        loop_start_addr = compiler.chunk().count();
        expression(compiler);
        compiler.consume(TOKEN_RIGHT_PAREN, "Expect closing ')' at end of for loop");
        if let Some(addr) = condition_start {
            compiler.emit_loop(addr);
        }
        compiler.patch_jump(condition_end_jump);
    }
    
    statement(compiler);
    compiler.emit_loop(loop_start_addr); 
    
    // If we had a condition, hence JUMP_IF_FALSE, we need to patch that to actually jump 
    // to the end of the body now.
    if let Some(addr) = exit_jump {
        compiler.patch_jump(addr);
    }
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
    compiler.end_scope();
}

// Every if statement is compiled with an implicit (potentially empty) else block. 
// This makes the control flow simpler since every if / else is the same. 
fn if_else_statement(compiler: &mut Compiler) {
    compiler.consume(TOKEN_LEFT_PAREN, "Expect opening '(' at start of if statement");
    expression(compiler);
    compiler.consume(TOKEN_RIGHT_PAREN, "Expect opening '(' at start of if statement");
    // Jump to the else clause if the if statement is false
    let if_jump = compiler.emit_jump(OP_JUMP_IF_FALSE);
    // Pop the conditional result that was just checked if we do not jump
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
    // Compile the body to the if statement followed by jump over the else body
    statement(compiler);
    let else_jump = compiler.emit_jump(OP_JUMP);
    // Patch the conditional if jump into the else statement and pop that condtion off the stack
    compiler.patch_jump(if_jump);
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
    // Actually compile an else body if there is an else
    if compiler.match_advance(TOKEN_ELSE) {
        statement(compiler);
    }
    // Patch the jump over the else statement 
    compiler.patch_jump(else_jump);
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

fn return_statement(compiler: &mut Compiler) {
    if compiler.match_advance(TOKEN_SEMICOLON) {
        compiler.emit_return();
    }
    else {
        expression(compiler);
        compiler.consume(TOKEN_SEMICOLON, "Expect ';' following return statement");
        compiler.emit_byte(OP_RETURN as u8, compiler.previous.line_num);
    }
}

fn function(compiler: &mut Compiler) {
    compiler.start_function();
    compiler.begin_scope(); 
    
    compiler.consume(TOKEN_LEFT_PAREN, "Expect '(' at start of function");
    if !compiler.match_advance(TOKEN_RIGHT_PAREN) {
        compiler.increment_arity();
        parse_variable(compiler);
        while compiler.match_advance(TOKEN_COMMA) {
            compiler.increment_arity();
            parse_variable(compiler);
        }
        compiler.consume(TOKEN_RIGHT_PAREN, "Expect ')' at end of function");
    }

    compiler.consume(TOKEN_LEFT_BRACE, "Expect '{' before function body");
    block_stmt(compiler);
    compiler.emit_return();
    let function = compiler.end_function();
    if crate::debug() {
        disassemble_chunk(&function.chunk, &format!("Compiling {} complete", function));
    }
    let function_idx = compiler.create_constant(Value::Function(Rc::new(function)));
    compiler.emit_bytes(OP_CLOSURE as u8, function_idx as u8, compiler.previous.line_num);
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
    
    // Handle parsing up to the appropriate point. Pass in 1 higher precedence so that
    // this handles left-associative trait of binary operators in rlox since parse_precedence
    // will stop parsing when it hits the next * since we are now one above * in precedence.
    // a = 3 * 4 * 2 == (3*4)*2
    //-----^
    //   prefix
    //-------^
    //     infix [MULT]
    //---------^
    //       prefix
    //-----------^
    //          infix [SUB]
    //-------------^
    //            prefix
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

// Works by checking the first condition -- if false short circuit (leaving false on the stack) 
// and jump over the next condition. Otherwise, pop the true off the stack and compile the
// next condition which will be true / false on its own and as the result of the &&
fn and(compiler: &mut Compiler) {
    let short_circuit = compiler.emit_jump(OP_JUMP_IF_FALSE);
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
    compiler.parse_precedence(PREC_AND);
    compiler.patch_jump(short_circuit);
}

// Similar to and but short circuit logic is a bit flipped. Jump over if the first condition is
// true, otherwise evaluate the second condition. 
fn or(compiler: &mut Compiler) {
    // If the first condition is false, we need to actually check the second condition. Jump over
    // the short_circuit command and continue along. 
    let cont = compiler.emit_jump(OP_JUMP_IF_FALSE);
    // In the or case, we short circuit when the first condition is true. So if we do not
    // JUMP_IF_FALSE, then the first condition was true so we need to jump over the second. 
    let short_circuit = compiler.emit_jump(OP_JUMP);
    // Jumping over the short circuit, pop the previous result. 
    compiler.patch_jump(cont);
    compiler.emit_byte(OP_POP as u8, compiler.current.line_num);
    compiler.parse_precedence(PREC_OR);
    // Jump over the second condition
    compiler.patch_jump(short_circuit);
}

fn number(compiler: &mut Compiler, _: bool) {
    // If we can parse the lexeme as an f64 without error then we're good to "cast" it into a
    // Value::Number()
    match compiler.previous.lexeme.parse::<f64>() {
        Ok(constant_value) => {
            let const_idx = compiler.create_constant(Value::Number(constant_value));
            compiler.emit_read_constant(const_idx);
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
    // Need to handle local and global separately. If we find a local variable defined with the
    // previous lexeme name, use that. Otherwise default to global. The only difference is really
    // how we get the argument to the GET / SET code (from local stack versus constant pool).
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
    if can_assign && compiler.match_advance(TOKEN_EQUAL) {
            expression(compiler);
            compiler.emit_bytes(op_set as u8, arg as u8, compiler.previous.line_num);
    } else {
            compiler.emit_bytes(op_get as u8, arg as u8, compiler.previous.line_num);
    }
}

// Entry into a function call
fn call(compiler: &mut Compiler) {
    let arg_count = parse_args(compiler);
    compiler.emit_bytes(OP_CALL as u8, arg_count, compiler.previous.line_num);
}

// Puts the arguments for a function call at the top of the stack by calling through to expression
// for the number of args. 
fn parse_args(compiler: &mut Compiler) -> u8 {
    let mut arg_count = 0;
    if !compiler.match_advance(TOKEN_RIGHT_PAREN) {
        expression(compiler);
        arg_count += 1;
        while compiler.match_advance(TOKEN_COMMA) {
            expression(compiler);
            arg_count += 1;
        }
        compiler.consume(TOKEN_RIGHT_PAREN, "Expect ')' at end of function call");
    };
    arg_count
}

fn string(compiler: &mut Compiler, _: bool) {
    let lexeme = &compiler.previous.lexeme;
    // Trim the leading and trailing " from the lexeme
    let string = String::from(&lexeme[1..lexeme.len() - 1]);
    let const_idx = compiler.create_constant(Value::Str(string));
    compiler.emit_read_constant(const_idx);
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
        rules.0.insert(TOKEN_LEFT_PAREN,    ParseRule {precedence: PREC_CALL,       prefix: Some(&grouping), infix: Some(&call)});
        rules.0.insert(TOKEN_STRING,        ParseRule {precedence: PREC_NONE,       prefix: Some(&string),  infix: None});
        rules.0.insert(TOKEN_EQUAL,         ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_VAR,           ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_IDENTIFIER,    ParseRule {precedence: PREC_NONE,       prefix: Some(&identifier), infix: None});
        rules.0.insert(TOKEN_FALSE,         ParseRule {precedence: PREC_NONE,       prefix: Some(&literal), infix: None});
        rules.0.insert(TOKEN_TRUE,          ParseRule {precedence: PREC_NONE,       prefix: Some(&literal), infix: None});
        rules.0.insert(TOKEN_BANG,          ParseRule {precedence: PREC_NONE,       prefix: Some(&unary),   infix: None});
        rules.0.insert(TOKEN_NIL,           ParseRule {precedence: PREC_NONE,       prefix: Some(&literal), infix: None});
        rules.0.insert(TOKEN_RIGHT_PAREN,   ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_SEMICOLON,     ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
        rules.0.insert(TOKEN_COMMA,         ParseRule {precedence: PREC_NONE,       prefix: None,           infix: None});
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
        rules.0.insert(TOKEN_OR,            ParseRule {precedence: PREC_OR,         prefix: None,           infix: Some(&or)});
        rules.0.insert(TOKEN_AND,           ParseRule {precedence: PREC_AND,        prefix: None,           infix: Some(&and)});

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
            Some(PREC_NONE)
        }
    }
}

// 1. Scan the code and create a vector of Tokens
// 2. Compile the tokens and emit bytecode onto the Chunk
pub fn compile(source: &str) -> Result<LoxFn, CompilerError> {
    // Generate a vector of tokens in reverse order so that compiler can use 
    // standard pop commands to move through the Vec.
    let tokens = Scanner::new(source.trim()).scan();
    let mut compiler = Compiler::new(tokens);
    // Start the compiler by advancing on the first token. 
    compiler.advance();
    // Loop until we hit EOF, creating declarations. 
    while compiler.current.kind != TOKEN_EOF {
        declaration(&mut compiler);
    }
    compiler.emit_byte(OP_RETURN as u8, compiler.current.line_num);
    if crate::debug() {
        disassemble_chunk(compiler.chunk(), "Compiling Script Complete");
    }

    if compiler.had_error {
        Err(CompilerError())
    } else {
        compiler.functions.pop().ok_or_else(|| CompilerError())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let source = "-(5+(15-3)/4*2);";
        let chunk = compile(source).unwrap();
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
