use crate::chunk::OpCode::{self, *};
use crate::debug::disassemble_chunk;
use crate::precedence::get_precedence;
use crate::precedence::Precedence;
use crate::precedence::Precedence::*;
use crate::scanner::TokenType::{self, *};
use crate::scanner::{Scanner, Token, TokenStream};
use crate::value::{LoxFn, Value};
use std::cell::RefCell;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

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
    is_local: bool,
}

impl std::cmp::PartialEq for Local {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

struct Compiler {
    // Compiler is always in a function. At the top level this is the script itself.
    // Enclosing is the compiler / function that surrounds this (or none if top level).
    tokens: Rc<RefCell<TokenStream>>,
    enclosing: *mut Compiler,
    function: LoxFn,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    depth: u8,
    had_error: bool,
    panic_mode: bool,
}

fn create_jump_offset(jump_offset: usize) -> (u8, u8) {
    let jump_u16 = jump_offset as u16;
    let high_bits = ((jump_u16 >> 8) & 0xff) as u8;
    let low_bits = (jump_u16 & 0xff) as u8;
    (high_bits, low_bits)
}

impl Compiler {
    pub fn new(tokens: Rc<RefCell<TokenStream>>, enclosing: *mut Compiler, fun_name: Option<String>) -> Self {
        let mut function = LoxFn::new();
        function.name = fun_name;
        Compiler {
            tokens,
            enclosing,
            function,
            locals: vec![Local {
                name: String::from(""),
                depth: 0,
                is_captured: false,
            }],
            upvalues: Vec::new(),
            depth: 0,
            had_error: false,
            panic_mode: false,
        }
    }

    fn start_function(&mut self, fun_name: &str) -> Compiler {
        Compiler::new(self.tokens.clone(), &mut *self, Some(fun_name.to_string()))
    }

    fn end_function(self) -> LoxFn {
        self.function
    }

    fn push_local(&mut self, local: Local) -> usize {
        self.locals.push(local);
        self.locals.len() - 1
    }

    fn peek_local(&self) -> Option<&Local> {
        self.locals.last()
    }

    fn pop_local(&mut self) -> Result<Local, CompilerError> {
        self.locals.pop().ok_or(CompilerError())
    }

    fn find_local(&self, needle: &str) -> Option<usize> {
        for (idx, var) in self.locals.iter().rev().enumerate() {
            if var.name == needle {
                return Some(self.locals.len() - idx - 1);
            }
        }
        None
    }

    //TODO: This does not appropriately handle shadowed variables like below
    // var a = "outer" {
    //  var a = a;
    // }
    fn add_local(&mut self, name: String) -> usize {
        let local = Local {
            name,
            depth: self.depth,
            is_captured: false,
        };
        self.push_local(local)
    }

    // Looks for and returns the index of a local variable by name starting in current
    // depth / scope and working backwards. None if not found.
    fn resolve_local(&self, token: &Token) -> Option<usize> {
        self.find_local(&token.lexeme)
    }

    fn begin_scope(&mut self) {
        self.depth += 1;
    }

    // At the end of a scope we need to pop the locals off the stack
    fn end_scope(&mut self) {
        let mut current = self.peek_local();
        while let Some(local) = current {
            if local.depth == self.depth {
                self.pop_local().unwrap();
                self.emit_byte(OP_POP as u8);
                current = self.peek_local();
            } else {
                current = None
            }
        }
        self.depth -= 1;
    }

    // Small wrappers to shorten code elsewhere by hiding the indirection
    // onto the tokens vec
    fn line(&self) -> i32 {
        self.tokens.borrow_mut().peek().line_num
    }

    fn peek(&self) -> Token {
        self.tokens.borrow_mut().peek()
    }

    fn next(&self) -> Token {
        self.tokens.borrow_mut().next()
    }

    fn expect(&mut self, kind: TokenType, msg: &'static str) {
        let ret = self.tokens.borrow_mut().expect(kind, msg); 
        match ret {
            Ok(_) => (),
            Err(_) => self.parse_error(msg),
        };
    }

    fn match_(&self, kind: TokenType) -> bool {
        self.tokens.borrow_mut().match_(kind)
    }

    // Functions to handle "emitting" values which writes them to the
    // chunk associated with the current function being compiled.
    fn emit_byte(&mut self, byte: u8) {
        self.function.chunk.write(byte, self.line());
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.function.chunk.write(b1, self.line());
        self.function.chunk.write(b2, self.line());
    }

    fn emit_return(&mut self) {
        self.emit_byte(OP_NIL as u8);
        self.emit_byte(OP_RETURN as u8);
    }

    // Emits bytecode to read a constant at const_idx onto the VM stack
    fn emit_read_constant(&mut self, const_idx: usize) {
        self.emit_bytes(OP_CONSTANT as u8, const_idx as u8);
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
        self.emit_byte(op as u8);
        self.emit_byte(OP_UNKNOWN as u8);
        self.emit_byte(OP_UNKNOWN as u8);
        self.function.chunk.count() - 2
    }

    // Loops don't need to be patched, so single step takes in an address that represents the start
    // of a loop and emits OP_LOOP | HIGH BITS | LOW BITS.
    //
    // The offset has 3 added to it to make up for the IP moving forward in the VM by 3 when
    // actually reading the jump instruction + offset bytes.
    fn emit_loop(&mut self, addr: usize) {
        let (high_bits, low_bits) = create_jump_offset(self.function.chunk.count() - addr + 3);
        self.emit_byte(OP_LOOP as u8);
        self.emit_byte(high_bits as u8);
        self.emit_byte(low_bits as u8);
    }

    // Per emit_jump this is the patching logic that replaces the UNKNOWN
    // code with the offset to jump.
    //
    // Input is the address the patch (see emit_jump) and this is patched to jump
    // to the current memory location. That location is determined by the
    //.function.chunk.count() [Total Instructions] - [Patch Addr] - 2. -2 is due to the
    // actual VM moving the IP forward by 2 when reading the jump offset.
    fn patch_jump(&mut self, addr: usize) {
        let (high_bits, low_bits) = create_jump_offset(self.function.chunk.count() - addr - 2);
        self.function.chunk.update(addr, high_bits);
        self.function.chunk.update(addr + 1, low_bits);
    }

    // Creates a constant by putting it into the constant_pool on the code Chunk. If that constant
    // already exists in there just return the index into that.
    fn create_constant(&mut self, value: Value) -> usize {
        match self.function.chunk.find_constant(&value) {
            Some(const_idx) => const_idx,
            _ => self.function.chunk.add_constant(Rc::new(value)),
        }
    }

    // Emit errors and set the flags to ensure a program with compiler
    // errors does not run. Also sets the start of synchonrization to next
    // valid spot in source code.
    fn parse_error(&mut self, msg: &'static str) {
        self.had_error = true;
        if !self.panic_mode {
            let token = &self.peek();
            let location = if token.is_eof() {
                " at end".to_string()
            } else {
                format!("at {}", token.lexeme)
            };
            println!("[line {}] Error {}: {}", self.line(), location, msg);
            self.panic_mode = true;
        }
    }

    // Play forward until a statement so we can try to being compilation again there.
    fn synchronize(&mut self) {
        self.panic_mode = false;
        while !self.match_(TOKEN_EOF) && !self.match_(TOKEN_SEMICOLON) {
            self.next();
        }
    }

    // The magic function that handles everything. Parses the specific prefix function
    // for the previous token and then parses the following as infix expressions
    // if they are greater than or equal precedence.
    fn parse_precedence(&mut self, precedence: Precedence) {
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
        let token = self.peek();
        match token.kind {
            TOKEN_LEFT_PAREN => self.grouping(),
            TOKEN_STRING => self.string(),
            TOKEN_NUMBER => self.number(),
            TOKEN_IDENTIFIER => self.identifier(can_assign),
            TOKEN_FALSE | TOKEN_TRUE | TOKEN_NIL => self.literal(),
            TOKEN_BANG | TOKEN_MINUS | TOKEN_PLUS | TOKEN_STAR | TOKEN_SLASH => self.unary(),
            _ => self.parse_error("Expect expression for prefix"),
        }

        //println!("Precedence after prefix: {:?}. Previous: {:?}. Current: {:?}", precedence, token, self.peek());

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
        while precedence <= get_precedence(self.peek().kind) {
            let token = self.peek();
            //println!("Precedence before infix: {:?}. Previous: {:?}. Current: {:?}", precedence, token, self.peek());
            match token.kind {
                TOKEN_LEFT_PAREN => self.call(),
                TOKEN_OR => self.or(),
                TOKEN_AND => self.and(),
                TOKEN_MINUS | TOKEN_PLUS | TOKEN_STAR | TOKEN_SLASH | TOKEN_EQUAL_EQUAL | TOKEN_BANG_EQUAL | TOKEN_GREATER | TOKEN_LESS
                | TOKEN_GREATER_EQUAL | TOKEN_LESS_EQUAL => self.binary(),
                _ => self.parse_error("Expect expression for infix"),
            }
        }

        // Final check to make sure didn't have some odd input that was written as an assignment
        // precedence not followed by '='.
        if can_assign && self.match_(TOKEN_EQUAL) {
            self.parse_error("Invalid assignment target.");
        }
    }

    /*------------------------------------------------------------------
     * Functions that map to the Lox grammar and represent the recursive
     * descent algorithm.
     *
     * Declarations
    --------------------------------------------------------------------*/
    fn declaration(&mut self) {
        match self.peek().kind {
            TOKEN_FUN => self.fun_declaration(),
            TOKEN_VAR => self.var_declaration(),
            _ => self.statement(),
        }
        // If we hit an error parsing, synchronize to the next valid point.
        // Execution will not happen but this enables compilation to continue
        // so we grab any additional errors.
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        self.expect(TOKEN_FUN, "Expect 'fun' at start of a function declaration");
        let fun_name = self.peek().lexeme;
        let var_idx = self.parse_variable();
        self.function(&fun_name);
        if self.depth == 0 {
            self.emit_byte(OP_DEFINE_GLOBAL as u8);
        } else {
            self.emit_bytes(OP_SET_LOCAL as u8, var_idx as u8);
        }
    }

    fn var_declaration(&mut self) {
        // Parse the variable and add the name to constant table / local stack.
        let var_idx = self.define_variable();

        // Grab the value that the variable will be initialized as. Either there is
        // an = in which case parse the expression otherwise all variables start
        // out as Nil. Emit this onto the stack.
        match self.peek().kind {
            TOKEN_EQUAL => {
                self.next();
                self.expression();
            }
            _ => {
                self.emit_byte(OP_NIL as u8);
            }
        }

        // Define / Set the initial value of the variable
        if self.depth == 0 {
            self.emit_byte(OP_DEFINE_GLOBAL as u8);
        } else {
            self.emit_bytes(OP_SET_LOCAL as u8, var_idx as u8);
        }
        self.expect(TOKEN_SEMICOLON, "Expect ';' at end of variable declaration");
    }

    /*------------------------------------------------------------------
     * Statements
    --------------------------------------------------------------------*/
    fn statement(&mut self) {
        match self.peek().kind {
            TOKEN_RETURN => self.return_stmt(),
            TOKEN_IF => self.if_else_stmt(),
            TOKEN_WHILE => self.while_stmt(),
            TOKEN_FOR => self.for_stmt(),
            TOKEN_LEFT_BRACE => self.block_stmt(),
            _ => self.expr_stmt(),
        }
    }

    fn while_stmt(&mut self) {
        // Save the current address to use later when emitting the loop instruction at the
        // end of the while loop.
        self.expect(TOKEN_WHILE, "Expect while at start of while loop");
        let loop_start_addr = self.function.chunk.count();
        self.expect(TOKEN_LEFT_PAREN, "Expect opening '(' at start of while loop");
        // Compile the loop condition (which will leave it at the top of the stack)
        self.expression();
        self.expect(TOKEN_RIGHT_PAREN, "Expect closing ')' at end of while loop");
        // Conditional jump if the loop expression is false--to be patched with the end of the loop
        // once body is compiled
        let loop_end = self.emit_jump(OP_JUMP_IF_FALSE);
        // If we don't jump, pop the loop condition result off the stack
        self.emit_byte(OP_POP as u8);
        self.statement();
        // Emit the loop instruction back to the start
        self.emit_loop(loop_start_addr);
        // Patch the end of loop onto the conditional jump
        self.patch_jump(loop_end);
        // Pop the loop condition if we jumped to the end
        self.emit_byte(OP_POP as u8);
    }

    fn for_stmt(&mut self) {
        self.begin_scope();
        self.expect(TOKEN_FOR, "Expect for at start of for loop");
        self.expect(TOKEN_LEFT_PAREN, "Expect opening '(' at start of for loop");

        // Handle the initializer for(var i = 0;) for(i=0) for(;)
        match self.peek() {
            token if token.kind == TOKEN_VAR => {
                self.var_declaration();
            }
            token if token.kind == TOKEN_SEMICOLON => {
                self.next();
            }
            _ => {
                self.expr_stmt();
            }
        }

        // Condition check
        let mut loop_start_addr = self.function.chunk.count();
        let mut exit_jump = None; // OP_JUMP_NOT_EQUAL at the start of the loop
        let mut condition_start = None; // OP_LOOP to the condition if required (e.g. if there is an increment)
        if !self.match_(TOKEN_SEMICOLON) {
            condition_start = Some(self.function.chunk.count());
            self.expression();
            exit_jump = Some(self.emit_jump(OP_JUMP_IF_FALSE));
            self.emit_byte(OP_POP as u8);
            self.expect(TOKEN_SEMICOLON, "Expect opening ';' after loop condition");
        }

        // Increment
        if !self.match_(TOKEN_RIGHT_PAREN) {
            let condition_end_jump = self.emit_jump(OP_JUMP);
            loop_start_addr = self.function.chunk.count();
            self.expression();
            self.expect(TOKEN_RIGHT_PAREN, "Expect closing ')' at end of for loop");
            if let Some(addr) = condition_start {
                self.emit_loop(addr);
            }
            self.patch_jump(condition_end_jump);
        }

        self.statement();
        self.emit_loop(loop_start_addr);

        // If we had a condition, hence JUMP_IF_FALSE, we need to patch that to actually jump
        // to the end of the body now.
        if let Some(addr) = exit_jump {
            self.patch_jump(addr);
        }
        self.emit_byte(OP_POP as u8);
        self.end_scope();
    }

    // Every if statement is compiled with an implicit (potentially empty) else block.
    // This makes the control flow simpler since every if / else is the same.
    fn if_else_stmt(&mut self) {
        self.expect(TOKEN_IF, "Expect if at start of branch");
        self.expect(TOKEN_LEFT_PAREN, "Expect opening '(' at start of if statement");
        self.expression();
        self.expect(TOKEN_RIGHT_PAREN, "Expect closing ')' at end of if statement");
        // Jump to the else clause if the if statement is false
        let if_jump = self.emit_jump(OP_JUMP_IF_FALSE);
        // Pop the conditional result that was just checked if we do not jump
        self.emit_byte(OP_POP as u8);
        // Compile the body to the if statement followed by jump over the else body
        self.statement();
        let else_jump = self.emit_jump(OP_JUMP);
        // Patch the conditional if jump into the else statement and pop that condtion off the stack
        self.patch_jump(if_jump);
        self.emit_byte(OP_POP as u8);
        // Actually compile an else body if there is an else
        if self.match_(TOKEN_ELSE) {
            self.statement();
        }
        // Patch the jump over the else statement
        self.patch_jump(else_jump);
    }

    fn block_stmt(&mut self) {
        self.expect(TOKEN_LEFT_BRACE, "Expect opening '{' at start of block");
        self.begin_scope();
        // Keep looping until we hit the closing brace. If we hit EOF
        // that is an error that will be caught on the subsequent consume call.
        while !self.match_(TOKEN_RIGHT_BRACE) && !self.match_(TOKEN_EOF) {
            self.declaration();
        }
        self.end_scope();
    }

    fn return_stmt(&mut self) {
        self.expect(TOKEN_RETURN, "Expect return at start of return");
        if self.match_(TOKEN_SEMICOLON) {
            self.emit_return();
        } else {
            self.expression();
            self.expect(TOKEN_SEMICOLON, "Expect ';' following return statement");
            self.emit_byte(OP_RETURN as u8);
        }
    }

    fn expr_stmt(&mut self) {
        self.expression();
        self.expect(TOKEN_SEMICOLON, "Expect ';' at end of statement");
        self.emit_byte(OP_POP as u8);
    }

    /*------------------------------------------------------------------
     * Expressions
    --------------------------------------------------------------------*/
    fn expression(&mut self) {
        self.parse_precedence(PREC_ASSIGNMENT);
    }

    fn grouping(&mut self) {
        self.expression();
        self.expect(TOKEN_RIGHT_PAREN, "Expect closing ')'");
    }

    fn binary(&mut self) {
        let op = self.next().kind;
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
        self.parse_precedence(Precedence::from_u8(precedence as u8 + 1));
        match op {
            TOKEN_PLUS => self.emit_byte(OP_ADD as u8),
            TOKEN_MINUS => self.emit_byte(OP_SUBTRACT as u8),
            TOKEN_STAR => self.emit_byte(OP_MULTIPLY as u8),
            TOKEN_SLASH => self.emit_byte(OP_DIVIDE as u8),
            TOKEN_EQUAL_EQUAL => self.emit_byte(OP_EQUAL as u8),
            TOKEN_BANG_EQUAL => self.emit_bytes(OP_EQUAL as u8, OP_NOT as u8),
            TOKEN_GREATER => self.emit_byte(OP_GREATER as u8),
            TOKEN_LESS => self.emit_byte(OP_LESS as u8),
            TOKEN_GREATER_EQUAL => self.emit_bytes(OP_LESS as u8, OP_NOT as u8),
            TOKEN_LESS_EQUAL => self.emit_bytes(OP_GREATER as u8, OP_NOT as u8),
            _ => self.parse_error("Invalid binary operator"),
        }
    }

    // Works by checking the first condition -- if false short circuit (leaving false on the stack)
    // and jump over the next condition. Otherwise, pop the true off the stack and compile the
    // next condition which will be true / false on its own and as the result of the &&
    fn and(&mut self) {
        let short_circuit = self.emit_jump(OP_JUMP_IF_FALSE);
        self.emit_byte(OP_POP as u8);
        self.parse_precedence(PREC_AND);
        self.patch_jump(short_circuit);
    }

    // Similar to and but short circuit logic is a bit flipped. Jump over if the first condition is
    // true, otherwise evaluate the second condition.
    fn or(&mut self) {
        // If the first condition is false, we need to actually check the second condition. Jump over
        // the short_circuit command and continue along.
        let cont = self.emit_jump(OP_JUMP_IF_FALSE);
        // In the or case, we short circuit when the first condition is true. So if we do not
        // JUMP_IF_FALSE, then the first condition was true so we need to jump over the second.
        let short_circuit = self.emit_jump(OP_JUMP);
        // Jumping over the short circuit, pop the previous result.
        self.patch_jump(cont);
        self.emit_byte(OP_POP as u8);
        self.parse_precedence(PREC_OR);
        // Jump over the second condition
        self.patch_jump(short_circuit);
    }

    fn number(&mut self) {
        // If we can parse the lexeme as an f64 without error then we're good to "cast" it into a
        // Value::Number()
        match self.next().lexeme.parse::<f64>() {
            Ok(constant_value) => {
                let const_idx = self.create_constant(Value::Number(constant_value));
                self.emit_read_constant(const_idx);
            }
            Err(_) => {
                self.parse_error("Error parsing number");
            }
        };
    }

    fn unary(&mut self) {
        let op = self.peek().kind;
        self.parse_precedence(PREC_UNARY);
        match op {
            TOKEN_MINUS => self.emit_byte(OP_NEGATE as u8),
            TOKEN_BANG => self.emit_byte(OP_NOT as u8),
            _ => self.parse_error("Invalid unary operator"),
        }
    }

    fn literal(&mut self) {
        match self.next().kind {
            TOKEN_FALSE => self.emit_byte(OP_FALSE as u8),
            TOKEN_TRUE => self.emit_byte(OP_TRUE as u8),
            TOKEN_NIL => self.emit_byte(OP_NIL as u8),
            _ => {}
        }
    }

    fn identifier(&mut self, can_assign: bool) {
        let op_set;
        let op_get;
        let arg;
        // Need to handle local, upvalue and global separately. If we find a local variable defined with the
        // previous lexeme name, use that. Otherwise check upvalues. Finally default to global.
        let identifier = self.next();
        match self.resolve_local(&identifier) {
            Some(local_idx) => {
                arg = local_idx;
                op_set = OP_SET_LOCAL;
                op_get = OP_GET_LOCAL;
            }
            _ => {
                let identifier = identifier.lexeme.to_string();
                arg = self.create_constant(Value::Str(identifier));
                op_set = OP_SET_GLOBAL;
                op_get = OP_GET_GLOBAL;
            }
        };
        if can_assign && self.match_(TOKEN_EQUAL) {
            self.expression();
            self.emit_bytes(op_set as u8, arg as u8);
        } else {
            self.emit_bytes(op_get as u8, arg as u8);
        }
    }

    // Entry into a function call
    fn call(&mut self) {
        self.expect(TOKEN_LEFT_PAREN, "Expect '(' before argument(s)");
        let arg_count = self.parse_args();
        self.emit_bytes(OP_CALL as u8, arg_count);
    }

    fn string(&mut self) {
        let lexeme = &self.next().lexeme;
        // Trim the leading and trailing " from the lexeme
        let string = String::from(&lexeme[1..lexeme.len() - 1]);
        let const_idx = self.create_constant(Value::Str(string));
        self.emit_read_constant(const_idx);
    }

    // If we are in a variable declaration, handles adding that variable name to either the constant
    // pool (global) or into the local variable stack. Determines this simply based on the current
    // scope_depth of the compiler.
    //
    // Broken into define + parse for the cases that actually don't have "var" in front e.g. in a
    // function definition.
    fn define_variable(&mut self) -> usize {
        self.expect(TOKEN_VAR, "Expect 'var' before variable definition");
        self.parse_variable()
    }

    fn parse_variable(&mut self) -> usize {
        let variable = self.peek();
        self.expect(TOKEN_IDENTIFIER, "Expect variable name");
        let variable_name = variable.lexeme.to_string();
        if self.depth == 0 {
            let const_idx = self.create_constant(Value::Str(variable_name));
            self.emit_read_constant(const_idx);
            const_idx
        } else {
            self.add_local(variable_name)
        }
    }

    // Puts the arguments for a function call at the top of the stack by calling through to expression
    // for the number of args.
    fn parse_args(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.match_(TOKEN_RIGHT_PAREN) {
            self.expression();
            arg_count += 1;
            while self.match_(TOKEN_COMMA) {
                self.expression();
                arg_count += 1;
            }
            self.expect(TOKEN_RIGHT_PAREN, "Expect ')' at end of function call");
        };
        arg_count
    }

    fn function(&mut self, fun_name: &str) {
        let mut compiler = self.start_function(fun_name);
        // Start a new scope to avoid anything being defined as a global inside of a function
        // since we use depth == 0 to decide on global definition.
        compiler.begin_scope();
        compiler.expect(TOKEN_LEFT_PAREN, "Expect '(' at start of function");
        match compiler.peek().kind {
            TOKEN_RIGHT_PAREN => {
                compiler.next();
            }
            _ => {
                compiler.function.arity += 1;
                compiler.parse_variable();
                while compiler.peek().kind == TOKEN_COMMA {
                    compiler.function.arity += 1;
                    compiler.parse_variable();
                }
                compiler.expect(TOKEN_RIGHT_PAREN, "Expect ')' at end of function");
            }
        }

        compiler.block_stmt();
        compiler.emit_return();
        let function = compiler.end_function();

        if crate::debug() {
            disassemble_chunk(&function.chunk, &format!("Compiling {} complete", function));
        }

        let function_idx = self.create_constant(Value::Function(Rc::new(function)));
        self.emit_bytes(OP_CLOSURE as u8, function_idx as u8);
    }
}

// 1. Scan the code and create a vector of Tokens
// 2. Compile the tokens and emit bytecode onto the Chunk
pub fn compile(source: &str) -> Result<LoxFn, CompilerError> {
    // Generate a vector of tokens in reverse order so that compiler can use
    // standard pop commands to move through the Vec.
    let tokens = Rc::new(RefCell::new(Scanner::new(source.trim()).scan()));
    let mut compiler = Compiler::new(tokens, std::ptr::null_mut(), None);
    // Start the compiler by advancing on the first token.
    // Loop until we hit EOF, creating declarations.
    while compiler.peek().kind != TOKEN_EOF {
        compiler.declaration();
    }
    compiler.emit_byte(OP_RETURN as u8);
    if crate::debug() {
        disassemble_chunk(&compiler.function.chunk, "Compiling Script Complete");
    }

    if compiler.had_error {
        Err(CompilerError())
    } else {
        Ok(compiler.function)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let source = "-(5+(15-3)/4*2);";
        let fun = compile(source).unwrap();
        let res = Chunk {
            code: vec![1, 0, 1, 1, 1, 2, 4, 1, 3, 6, 1, 4, 5, 3, 2, 14, 0],
            lines: vec![1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            constant_pool: vec![
                Rc::new(Value::Number(5.0)),
                Rc::new(Value::Number(15.0)),
                Rc::new(Value::Number(3.0)),
                Rc::new(Value::Number(4.0)),
                Rc::new(Value::Number(2.0)),
            ],
        };
        assert_eq!(fun.chunk, res);
    }
}
