use std::{error::Error, fmt::Display};
use crate::{chunk::OpCode, scanner::{Scanner, Token, TokenType}, value::{Function, Object, Value}};

/// Write a number of bytes to the chunk's code.
macro_rules! emit_bytes {
    ($($arg:expr),+; $line:expr, $chunk:expr) => {
        {
            $(
                $chunk.write_instruction($arg as u8, $line as i32);
            )+
        }
    };
}

/// Print debug information.
macro_rules! trace {
    ($($arg:tt)+) => {
        #[cfg(debug_assertions)]
        {
            print!("TRACE: ");
            println!($($arg)*);
        }
    };
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// The precedence order of different operations.
enum Precedence {
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}

impl Display for Precedence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "PREC_NONE"),
            Self::Assignment => write!(f, "PREC_ASSIGNMENT"),
            Self::Or => write!(f, "PREC_OR"),
            Self::And => write!(f, "PREC_AND"),
            Self::Equality => write!(f, "PREC_EQUALITY"),
            Self::Comparison => write!(f, "PREC_COMPARISON"),
            Self::Term => write!(f, "PREC_TERM"),
            Self::Factor => write!(f, "PREC_FACTOR"),
            Self::Unary => write!(f, "PREC_UNARY"),
            Self::Call => write!(f, "PREC_CALL"),
            Self::Primary => write!(f, "PREC_PRIMARY"),
        }   
    }
}

impl From<&u8> for Precedence {
    fn from(value: &u8) -> Self {
        match value {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Equality,
            5 => Precedence::Comparison,
            6 => Precedence::Term,
            7 => Precedence::Factor,
            8 => Precedence::Unary,
            9 => Precedence::Call,
            10 => Precedence::Primary,
            _ => panic!("Invalid Precedence value")
        }
    }
}

type CompileFn<'b> = fn(&mut Compiler<'b>, bool);

#[derive(Debug, Clone, Copy)]
/// A struct representing a parse rule. The rule contains two optional function pointers to the prefix and infix rules, and the precedence.
struct ParseRule<'b> {
    prefix: Option<CompileFn<'b>>,
    infix: Option<CompileFn<'b>>,
    precedence: Precedence,
}

/// TODO: Improve error handling here
#[derive(Debug, Clone, Copy)]
pub enum CompileError {
    ParseError
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError => write!(f, "Parse Error")
        }
    }
}

impl Error for CompileError {}

/// Get the [`ParseRule`] for the current [`TokenType`].
fn get_rule<'a>(token_type: TokenType) -> ParseRule<'a> {
    let rules: [ParseRule; 40] = [
        ParseRule { prefix: Some(Compiler::grouping), infix: Some(Compiler::call), precedence: Precedence::Call }, // TOKEN_LEFT_PAREN
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_RIGHT_PAREN
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_LEFT_BRACE
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_RIGHT_BRACE
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_COMMA
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_DOT
        ParseRule { prefix: Some(Compiler::unary), infix: Some(Compiler::binary), precedence: Precedence::Term }, // TOKEN_MINUS
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term }, // TOKEN_PLUS
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_SEMICOLON
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor }, // TOKEN_SLASH
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor }, // TOKEN_STAR
        ParseRule { prefix: Some(Compiler::unary), infix: None, precedence: Precedence::None }, // TOKEN_BANG
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Equality }, // TOKEN_BANG_EQUAL
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_EQUAL
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Equality }, // TOKEN_EQUAL_EQUAL
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // TOKEN_GREATER
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // TOKEN_GREATER_EQUAL
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // TOKEN_LESS
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // TOKEN_LESS_EQUAL
        ParseRule { prefix: Some(Compiler::variable), infix: None, precedence: Precedence::None }, // TOKEN_IDENTIFIER
        ParseRule { prefix: Some(Compiler::string), infix: None, precedence: Precedence::None }, // TOKEN_STRING
        ParseRule { prefix: Some(Compiler::number), infix: None, precedence: Precedence::None }, // TOKEN_NUMBER
        ParseRule { prefix: None, infix: Some(Compiler::and), precedence: Precedence::And }, // TOKEN_AND
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_CLASS
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_ELSE
        ParseRule { prefix: Some(Compiler::literal), infix: None, precedence: Precedence::None }, // TOKEN_FALSE
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_FOR
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_FUN
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_IF
        ParseRule { prefix: Some(Compiler::literal), infix: None, precedence: Precedence::None }, // TOKEN_NIL
        ParseRule { prefix: None, infix: Some(Compiler::or), precedence: Precedence::Or }, // TOKEN_OR
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_PRINT
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_RETURN
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_SUPER
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_THIS
        ParseRule { prefix: Some(Compiler::literal), infix: None, precedence: Precedence::None }, // TOKEN_TRUE
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_VAR
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_WHILE
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_ERROR
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_EOF
    ];

    rules[token_type as usize]
}

#[derive(Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Script
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Local<'a> {
    name: Token<'a>,
    depth: i16,
}

/// A compiler object that costructs a chunk by reading tokens from the input.
pub struct Compiler<'a> {
    function: Function,
    function_type: FunctionType,
    current: Token<'a>,
    previous: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    locals: Vec<Local<'a>>,
    scope_depth: i16,
}

/// Publicly accessible functions and helper functions.
impl<'a> Compiler<'a> {
    /// Create a new compiler for the source code that will output into the chunk.
    pub fn new(function_type: FunctionType, source: &'a str) -> Self {
        let scanner = Scanner::new(source);
        let locals = vec![Local { name: Token { token_type: TokenType::NULL, data: "", line: 0 }, depth: 0 }];
        Compiler {
            function: Function::new(),
            function_type,
            current: Token::new(source),
            previous: Token::new(source),
            scanner,
            had_error: false,
            panic_mode: false,
            locals,
            scope_depth: 0
        }
    }

    /// Compile the source code into the chunk.
    pub fn compile(mut self) -> Result<Function, CompileError> {
        self.advance();
        while !self.match_token(TokenType::EOF) {
            self.declaration();
        }

        #[cfg(debug_assertions)]
        if !self.had_error {
            println!("{}", "-".repeat(50));
            println!("DEBUG: Function: {}", self.function);
            println!("DEBUG: Chunk contents:");
            print!("{}", self.function.chunk);
            println!("{}", "-".repeat(50));
        }

        self.emit_return();
        
        if self.had_error {
            Err(CompileError::ParseError)
        } else {
            Ok(self.function)
        }
    }

    /// Advance forward a token, retrieving the new one from the scanner.
    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            let res = self.scanner.scan_token();

            if res.is_ok() {
                self.current = res.unwrap();
                #[cfg(debug_assertions)]
                println!("DEBUG: {}", self.current);
                break;
            } else {
                print!("{}", res.err().unwrap());
                self.had_error = true;
                break;
            }
        }
    }

    /// Consume the current token if it is of the given type, otherwise error with the given message.
    /// 
    /// **Arguments:**
    /// - `token_type` - The [`TokenType`] to check against.
    /// - `message` - The message to pass as an error if the check fails.
    fn consume(&mut self, token_type: TokenType, message: &str) {
        if !self.match_token(token_type) { self.error(true, message) }
    }

    /// Consume the current token if it is of the given type and return true, otherwise return false.
    /// 
    /// **Arguments**:
    /// - `token_type` - The [`TokenType`] to check against.
    /// 
    /// **Returns:**
    /// Whether the current token matches the `token_type` type.
    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.current.token_type != token_type { return false }
        self.advance();
        true
    }

    /// Prints an error message and sets the `had_error` and `panic_mode` flags in the compiler.
    /// 
    /// **Arguments:**
    /// - `current` - Flag to indicate the current token should be used, if set to false the previous token will be used.
    /// - `message` - The message to emit for the error.
    fn error(&mut self, current: bool, message: &str) {
        // If an error has already occured, don't spit out further errors to them until the compiler has resynchronised
        if self.panic_mode { return }

        let token = if current { &self.current } else { &self.previous };

        print!("[line {}] Error ", token.line);

        if token.token_type == TokenType::EOF {
            print!("at end");
        } else {
            print!( "on line {}, at '{}'", token.line, token.data);
        }

        println!(" {}", message);
        self.had_error = true;
        self.panic_mode = true;
    }

    /// Move the compiler to an appropriate safe starting point after an error has been encountered.
    fn synchronise(&mut self) {
        self.panic_mode = false;

        while self.current.token_type != TokenType::EOF {
            if self.previous.token_type == TokenType::Semicolon { return }
            match self.current.token_type {
                TokenType::Class => return,
                TokenType::Fun => return,
                TokenType::Var => return,
                TokenType::For => return,
                TokenType::If => return,
                TokenType::While => return,
                TokenType::Print => return,
                TokenType::Return => return,
                _ => {},
            }

            self.advance()
        }
    }

    /// Write a constant to the current chunk, handling the case where a [`OpCode::ConstantLong`] is required.
    /// 
    /// **Arguments:**
    /// - `constant` - The [`Value`] to write.
    fn emit_constant(&mut self, constant: Value) {
        let location = self.function.chunk.write_constant(constant);
        if location < 255 {
            emit_bytes!(OpCode::Constant, location; self.previous.line, self.function.chunk);
        } else {
            emit_bytes!(OpCode::ConstantLong; self.previous.line, self.function.chunk);
            let bytes = (location as u16).to_le_bytes();
            emit_bytes!(bytes[0], bytes[1]; self.previous.line, self.function.chunk);
        }
    }

    /// Emit a placeholder jump instruction, the jump OpCode's operand is two [`u8::MAX`] bytes.
    /// 
    /// **Returns:**
    /// The offset the jump operand starts in the code.
    fn emit_jump(&mut self, code: OpCode) -> usize {
        emit_bytes!(code, 0xff, 0xff; self.previous.line, self.function.chunk);
        self.function.chunk.get_code_count() - 2
    }

    /// Patch a previously emitted jump instruction's operand to jump to the current location.
    /// 
    /// **Arguments:**
    /// - `offset` - The offset into the chunk's code where the jump instruction is located.
    fn patch_jump(&mut self, offset: usize) {
        let jump = self.function.chunk.get_code_count() - offset - 2;

        if jump > u16::MAX as usize {
            self.error(true, "Too much code to jump over");
        }

        self.function.chunk.write_jump_dest(offset, jump as u16)
    }

    /// Emit a loop instruction, calculating the jump size from the current position in code back to the starting position.
    /// 
    /// **Arguments:**
    /// `start` - The starting position of the loop in code.
    fn emit_loop(&mut self, start: usize) {
        emit_bytes!(OpCode::Loop; self.previous.line, self.function.chunk);

        let jump_offset = self.function.chunk.get_code_count() - start + 2;
        if jump_offset > u16::MAX as usize { self.error(true, "Loop body too large") }

        let bytes = jump_offset.to_le_bytes();
        emit_bytes!(bytes[0], bytes[1]; self.previous.line, self.function.chunk);
    }

    /// Emit the OpCodes for returns.
    fn emit_return(&mut self) {
        emit_bytes!(OpCode::Nil, OpCode::Return; self.previous.line, self.function.chunk)
    }

    /// Begin a new scope. This increases the scope depth by one and changes how variables are defined.
    fn begin_scope(&mut self) {
        trace!("Defining new scope");
        self.scope_depth += 1;
    }

    /// End the current scope. This decreases the local scope by one and emits instructions to pop all local variables.
    fn end_scope(&mut self) {
        trace!("Ending local scope");
        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
            self.locals.pop();
        }
    }
}

impl<'a> Compiler<'a> {
    fn declaration(&mut self) {
        trace!("Called declaration rule");
        if self.match_token(TokenType::Var) {
            self.var_declaration()
        } else if self.match_token(TokenType::Fun) {
            self.fun_declaration()
        } else {
            self.statement()
        }

        // The end of a declaration is an appropriate place to try and synchronise
        if self.panic_mode { self.synchronise() }
    }

    /// 
    fn parse_precendence(&mut self, precedence: Precedence) {
        trace!{"Parsing precedence: {}", precedence};
        self.advance();
        let prefix_rule = get_rule(self.previous.token_type).prefix;
        let can_assign = precedence <= Precedence::Assignment;
        if let Some(prefix_fn) = prefix_rule {
            prefix_fn(self, can_assign);
        } else {
            self.error(false, "Expect expression");
            return;
        }

        // Continue parsing until the infix operator is no longer valid 
        while precedence <= get_rule(self.current.token_type).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.token_type).infix.unwrap();
            infix_rule(self, can_assign)
        }

        if can_assign && self.match_token(TokenType::Equal) { self.error(true, "Invalid assignment target") }
    }

    fn block(&mut self) {
        trace!("Called block rule");
        while self.current.token_type != TokenType::RightBrace && self.current.token_type != TokenType::EOF {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block")
    }

    fn fun_declaration(&mut self) {
        self.parse_variable("Expect function name");
        self.mark_initialised();
        self.function();
        self.define_variable();
    }

    fn function(&mut self) {
        let enclosing = self.function.clone();
        self.function = Function::new();
        self.function.name = self.previous.data.to_string();
        self.function_type = FunctionType::Function;
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name");
        if self.current.token_type != TokenType::RightParen {
            loop {
                // TODO: This actually errors at 254 params so fix
                self.function.arity += 1;
                if self.function.arity == 255 {
                    self.error(true, "Can't have more than 255 parameters");
                }

                self.parse_variable("Expect parameter name");
                self.define_variable();
                if !self.match_token(TokenType::Comma) { break }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' affter parameters");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body");
        self.block();

        self.end_scope();
        let val = Value::Obj(Object::Function(self.function.clone()));
        self.function = enclosing;
        self.emit_constant(val);
    }

    fn call(&mut self, _: bool) {
        let arg_count = self.arguments_list();
        emit_bytes!(OpCode::Call, arg_count; self.previous.line, self.function.chunk);
    }

    fn arguments_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if self.current.token_type != TokenType::RightParen {
            loop {
                self.expression();

                if arg_count == 255 {
                    self.error(false, "Can't have more than 255 arguments");
                }

                arg_count += 1;
                if !self.match_token(TokenType::Comma) { break }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after arguments");
        arg_count
    }
}

/// Variable declaration rules
// These need to be re-written at some point as they are a mess and hacky
impl<'a> Compiler<'a> {
    /// Parse a variable declaration, the var keyword has already been consumed.
    fn var_declaration(&mut self) {
        trace!("Called var declaration rule");

        // Parse the identifier
        self.parse_variable("Expect variable identifier");

        // Assign a value if there is one
        if self.match_token(TokenType::Equal) {
            self.expression()
        } else {
            emit_bytes!(OpCode::Nil; self.previous.line, self.function.chunk)
        }
        self.consume(TokenType::Semicolon, "Expect ';' after variable");

        // Set the local depth or emit a global instruction
        self.define_variable();
    }

    // Parse the variable identifier and place it in the correct space
    fn parse_variable(&mut self, message: &str) {
        trace!("Called parse variable rule");
        self.consume(TokenType::Identifier, message);

        self.declare_variable();

        // If this variable is not a global it is in our list of locals and we don't need to put it in the code
        if self.scope_depth > 0 { return }
        self.emit_constant(Value::Obj(Object::String(self.previous.data.to_string().clone())))
    }

    /// Declare the variable in the local scope.
    fn declare_variable(&mut self) {
        if self.scope_depth == 0 { return }
        trace!("Called declare variable rule");

        // Ensure it is unique in the local scope, it is allowed to shadow a variable of higher scope
        let mut duplicate = false;
        for local in self.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.scope_depth { break }
            if local.name == self.previous { duplicate = true }
        }

        if duplicate { self.error(false, "Already a variable with this name in this scope") }
        self.add_local(self.previous);
    }

    /// Add the variable to the locals list, leave local level to be initialised later.
    fn add_local(&mut self, name: Token<'a>) {
        if self.locals.len() as u8 == u8::MAX { self.error(false, "Too many variables in local scope") }

        // The variable scope is not added until the entire declaration has been parsed, this ensures that if an error occurs
        // the compiler doesn't accidentally think this variable exists.
        trace!("Adding variable: \"{}\"", name.data);
        self.locals.push(Local { name, depth: -1 })
    }

    // TODO: If this is not used later it should be removed in favour of just calling named_variable 
    fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign)
    }

    /// Access an already declared variable
    fn named_variable(&mut self, can_assign: bool) {
        trace!("Called named variable rule");
        let arg = self.resolve_local();

        // If the variable was not found in local scope it is a global and should be accessed as such
        let (get_op, set_op) = if arg.is_some() {
            (OpCode::GetLocal, OpCode::SetLocal)
        } else {
            self.emit_constant(Value::Obj(Object::String(self.previous.data.to_string().clone())));
            (OpCode::GetGlobal, OpCode::SetGlobal)
        };

        // Check whether it is a get or set op
        let op = if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            set_op
        } else {
            get_op
        };

        if let Some(val) = arg {
            emit_bytes!(op, val; self.previous.line, self.function.chunk)
        } else {
            emit_bytes!(op; self.previous.line, self.function.chunk)
        }
    }

    /// Attempt to find the variable in local scope
    fn resolve_local(&mut self) -> Option<u8> {
        trace!("Caled resolve local");
        for (index, local) in self.locals.iter().enumerate().rev() {
            if local.name.data == self.previous.data {
                if local.depth == -1 { self.error(false, "Can't read local variable in its own initializer") }
                return Some(index as u8)
            }
        }

        None
    }

    /// Handle definition of variable
    fn define_variable(&mut self) {
        if self.scope_depth > 0 {
            self.mark_initialised();
            return
        }

        trace!("Variable in global scope");
        emit_bytes!(OpCode::DefineGlobal; self.previous.line, self.function.chunk)
    }

    /// Mark a local variable as initialised
    fn mark_initialised(&mut self) {
        trace!("Marking local variable initialised");
        if self.scope_depth == 0 { return }
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }
}

/// Statement rules
impl<'a> Compiler<'a> {
    fn statement(&mut self) {
        trace!("Called statement rule");
        match self.current.token_type {
            TokenType::Print => { self.advance(); self.print_statement() },
            TokenType::Return => { self.advance(); self.return_statement() },
            TokenType::For => { self.advance(); self.for_statement() },
            TokenType::While => { self.advance(); self.while_statement() },
            TokenType::If => { self.advance(); self.if_statement() },
            TokenType::LeftBrace => {
                self.advance();
                self.begin_scope();
                self.block();
                self.end_scope();
            },
            _ => self.expression_statement()
        }
    }

    fn print_statement(&mut self) {
        trace!("Called print statement");
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        emit_bytes!(OpCode::Print; self.previous.line, self.function.chunk)
    }

    fn return_statement(&mut self) {
        if self.function_type == FunctionType::Script {
            self.error(true, "Cannot return from top-level code");
        }

        trace!("Called return statement");
        if self.match_token(TokenType::Semicolon) {
            self.emit_return()
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value");
            emit_bytes!(OpCode::Return; self.previous.line, self.function.chunk)
        }
    }

    fn if_statement(&mut self) {
        trace!("Called if statement");

        // Consume the conditional
        self.consume(TokenType::LeftParen, "Expect '(' after if statement");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition");

        // Emit a jump for if the conditiomal is false that takes the program to the then condition and parse the body
        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
        self.statement();

        // Create an implicit else location regardless of whether the user has specified one and plug in the then jump location
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);

        // If we have an else block then parse it, then set the else jump location regardless for our implicit else
        if self.match_token(TokenType::Else) { self.statement() }
        self.patch_jump(else_jump)
    }

    fn while_statement(&mut self) {
        trace!("Called while statement");

        // Create a marker for the start of the loop so it can be repeated
        let loop_start = self.function.chunk.get_code_count();

        // Parse the conditional
        self.consume(TokenType::LeftParen, "Expect '(' after if statement");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition");

        // Emit a jump for if the conditional is false and parse the loop body
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
        self.statement();

        // Plug the location into the exit jump and create the loop jump
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
    }

    fn for_statement(&mut self) {
        trace!("Called for statement");

        // A new local scope is created for variables declared in the for loop
        self.begin_scope();

        // An initialisation is not required but if it exists, parse it
        self.consume(TokenType::LeftParen, "Expect '(' after for");
        match self.current.token_type {
            TokenType::Semicolon => self.advance(),
            TokenType::Var => { self.advance(); self.var_declaration() },
            _ => self.expression_statement(),
        }

        // Create a marker that starts at the conditional and a jump to the exit of the loop
        let mut loop_start = self.function.chunk.get_code_count();
        let mut exit_jump = -1;

        // A conditional is not required but if it exists, parse it
        if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition");

            exit_jump = self.emit_jump(OpCode::JumpIfFalse) as isize;
            emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
        }
        
        // An advancement is not required but if it exists, parse it
        if !self.match_token(TokenType::RightParen) {
            // After completing the body should jump to the advancement and execute it
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.function.chunk.get_code_count();

            // Parse the increment
            self.expression();
            emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses");

            // Ensure the loop jumps back to the conditional after incrementing the advancement
            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        // Parse the body and emit a loop that moves back to the start
        self.statement();
        self.emit_loop(loop_start);

        // No exit jump is needed if there is no conditional, if there is plug the value into the previously emitted jump
        if exit_jump != -1 {
            self.patch_jump(exit_jump as usize);
            emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
        }

        self.end_scope();
    }

    /// An expression expected to be in a statement
    fn expression_statement(&mut self) {
        trace!("Called expression statement rule");
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk)
    }

    /// An expression of any kind, it will be parsed starting at the lowest precedence
    fn expression(&mut self) {
        trace!("Called expression rule");
        self.parse_precendence(Precedence::Assignment)
    }
}

/// The most basic building blocks of the language
impl<'a> Compiler<'a> {
    fn and(&mut self, _: bool) {
        // Because and logical operators short circuit we can skip the second condition if the first is false
        trace!("Called and rule");
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);

        // Ensure we pop the value calculated for the first condition if it evaluated to false
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);
        self.parse_precendence(Precedence::And);

        self.patch_jump(end_jump)
    }

    fn or(&mut self, _: bool) {
        // Because or logical operators short circuit, they are achieved by using jumps
        // We create a JumpIfFalse to the second condition to evaluate that if needed
        // Otherwrise we simply jump to the end of the or statement
        trace!("Called or rule");
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        // Ensure to pop the value caculated for the first condition if it evaluated to false
        self.patch_jump(else_jump);
        emit_bytes!(OpCode::Pop; self.previous.line, self.function.chunk);

        self.parse_precendence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn number(&mut self, _: bool) {
        let value = Value::Double(self.previous.data.parse().unwrap());
        trace!("Called number rule with: {}", value);
        self.emit_constant(value);
    }

    fn grouping(&mut self, _: bool) {
        trace!("Called grouping rule");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _: bool) {
        let operator_type = self.previous.token_type;
        trace!("Called unary rule: {}", operator_type);

        self.parse_precendence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => emit_bytes!(OpCode::Negate; self.previous.line, self.function.chunk),
            TokenType::Bang => emit_bytes!(OpCode::Not; self.previous.line, self.function.chunk),
            _ => {}
        }
    }

    fn binary(&mut self, _: bool) {
        let operator_type = self.previous.token_type;
        let rule = get_rule(operator_type);
        let prec = (&(rule.precedence as u8 + 1)).into();

        trace!("Called binary rule: {}, with precedence: {}", operator_type, prec);

        self.parse_precendence(prec);

        match operator_type {
            TokenType::BangEqual => emit_bytes!(OpCode::Equal, OpCode::Not; self.previous.line, self.function.chunk),
            TokenType::EqualEqual => emit_bytes!(OpCode::Equal; self.previous.line, self.function.chunk),
            TokenType::Greater => emit_bytes!(OpCode::Greater; self.previous.line, self.function.chunk),
            TokenType::GreaterEqual => emit_bytes!(OpCode::Less, OpCode::Not; self.previous.line, self.function.chunk),
            TokenType::Less => emit_bytes!(OpCode::Less; self.previous.line, self.function.chunk),
            TokenType::LessEqual => emit_bytes!(OpCode::Greater, OpCode::Not; self.previous.line, self.function.chunk),
            TokenType::Plus => emit_bytes!(OpCode::Add; self.previous.line, self.function.chunk),
            TokenType::Minus => emit_bytes!(OpCode::Subtract; self.previous.line, self.function.chunk),
            TokenType::Star => emit_bytes!(OpCode::Multipliy; self.previous.line, self.function.chunk),
            TokenType::Slash => emit_bytes!(OpCode::Divide; self.previous.line, self.function.chunk),
            _ => {},
        }
    }

    fn literal(&mut self, _: bool) {
        trace!("Called literal rule with: {}", self.previous.token_type);
        match self.previous.token_type {
            TokenType::False => emit_bytes!(OpCode::False; self.previous.line, self.function.chunk),
            TokenType::True => emit_bytes!(OpCode::True; self.previous.line, self.function.chunk),
            TokenType::Nil => emit_bytes!(OpCode::Nil; self.previous.line, self.function.chunk),
            _ => {},
        }
    }

    fn string(&mut self, _: bool) {
        trace!("Called string literal rule with: {}", self.previous.data.to_string().clone());
        self.emit_constant(Value::Obj(Object::String(self.previous.data.to_string().clone())));
    }
}