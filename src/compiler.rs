use std::{error::Error, fmt::Display};
use crate::{chunk::{Chunk, OpCode}, scanner::{Scanner, Token, TokenType}, value::{Object, Value}};

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

// TODO: To reduce complexiy this is a From instead of TryFrom, maybe change this at some point
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
        ParseRule { prefix: Some(Compiler::grouping), infix: None, precedence: Precedence::None }, // TOKEN_LEFT_PAREN
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
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_AND
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_CLASS
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_ELSE
        ParseRule { prefix: Some(Compiler::literal), infix: None, precedence: Precedence::None }, // TOKEN_FALSE
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_FOR
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_FUN
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_IF
        ParseRule { prefix: Some(Compiler::literal), infix: None, precedence: Precedence::None }, // TOKEN_NIL
        ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // TOKEN_OR
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

/// A compiler object that costructs a chunk by reading tokens from the input.
pub struct Compiler<'a> {
    current: Token,
    previous: Token,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    chunk: &'a mut Chunk,
}

// TODO: String copies can likely be changed for mem::take() as we don't care about keeping the data in the tokens
// Just be careful cause sometimes we access a previous token, we just need to ensure we don't access its data
impl<'a> Compiler<'a> {
    /// Create a new compiler for the source code.
    pub fn new(chunk: &'a mut Chunk, source: &'a str) -> Self {
        let scanner = Scanner::new(source.chars().peekable());
        Compiler { current: Token::new(), previous: Token::new(), scanner, had_error: false, panic_mode: false, chunk }
    }

    // TOOD: Maybe change back to the book version and have the chunk be passed as a mutable reference
    pub fn compile(&mut self) -> Result<(), CompileError> {
        self.advance();
        while !self.match_token(TokenType::EOF) {
            self.declaration();
        }

        if cfg!(debug_assertions) && !self.had_error {
            println!("DEBUG: Chunk contents:");
            println!("{}", self.chunk);
        }
        
        if self.had_error {
            Err(CompileError::ParseError)
        } else {
            Ok(())
        }
    }

    /// Advance forward a token, retrieving the new one from the scanner.
    fn advance(&mut self) {
        // TODO: Maybe use RC to avoid lots of copies
        self.previous = self.current.clone();

        loop {
            let res = self.scanner.scan_token();

            if res.is_ok() {
                self.current = res.unwrap();
                break;
            } else {
                print!("{}", res.err().unwrap());
                self.had_error = true;
            }
        }
    }

    /// Consume the current token if it is the given type, otherwise error with the given message.
    /// 
    /// # Arguments:
    /// - `token_type` - The [`TokenType`] to check against.
    /// - `message` - The message to pass as an error if the check fails.
    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.current.token_type == token_type {
            self.advance();
            return;
        }

        self.error(true, message)
    }

    fn match_token(&mut self, token_type:TokenType) -> bool {
        if self.current.token_type != token_type { return false }
        self.advance();
        true
    }

    /// Prints an error and sets the flags in the compiler.
    /// 
    /// # Arguments:
    /// - `current` - Flag to indicate the current token should be used, if set to false the previous token will be used.
    /// - `message` - The message to emit for the error.
    fn error(&mut self, current: bool, message: &str) {
        if self.panic_mode { return }

        let token = if current { &self.current } else { &self.previous };

        print!("[line {}] Error ", token.line);

        if token.token_type == TokenType::EOF {
            print!("at end");
        } else {
            print!( "at '{}'", token.data);
        }

        println!(" {}", message);
        self.had_error = true;
        self.panic_mode = true;
    }

    /// Write a constant to the current chunk, handling the case where a [`OpCode::ConstantLong`] is required.
    /// 
    /// # Arguments:
    /// - `constant` - The [`Value`] to write.
    fn emit_constant(&mut self, constant: Value) {
        let location = self.chunk.write_constant(constant);
        if location < 255 {
            emit_bytes!(OpCode::Constant, location; self.previous.line, self.chunk);
        } else {
            emit_bytes!(OpCode::ConstantLong; self.previous.line, self.chunk);
            self.chunk.write_long_constant_location(location as u16, self.previous.line as i32);
        }
    }

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

    fn parse_variable(&mut self) {
        self.consume(TokenType::Identifier, "Expect variable name");
        self.emit_constant(Value::Obj(Object::String(self.previous.data.clone())))
    }

    fn declaration(&mut self) {
        trace!("Called declaration rule");
        if self.match_token(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }

        if self.panic_mode { self.synchronise() }
    }

    fn var_declaration(&mut self) {
        trace!("Called var declaration rule");
        self.parse_variable();

        if self.match_token(TokenType::Equal) {
            self.expression()
        } else {
            emit_bytes!(OpCode::Nil; self.previous.line, self.chunk)
        }

        self.consume(TokenType::Semicolon, "Expect ';' after bariable");
        self.define_variable();

    }

    fn define_variable(&mut self) {
        emit_bytes!(OpCode::DefineGlobal; self.previous.line, self.chunk)
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign)
    }

    fn named_variable(&mut self, can_assign: bool) {
        self.emit_constant(Value::Obj(Object::String(self.previous.data.clone())));

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            emit_bytes!(OpCode::SetGlobal; self.previous.line, self.chunk)
        } else {
            emit_bytes!(OpCode::GetGlobal; self.previous.line, self.chunk)
        }
    }

    fn statement(&mut self) {
        trace!("Called statement rule");
        if self.match_token(TokenType::Print) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn expression(&mut self) {
        trace!("Called expression rule");
        self.parse_precendence(Precedence::Assignment)
    }

    fn expression_statement(&mut self) {
        trace!("Called expression statement rule");
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        emit_bytes!(OpCode::Pop; self.previous.line, self.chunk)
    }

    fn print_statement(&mut self) {
        trace!("Called print statement");
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        emit_bytes!(OpCode::Print; self.previous.line, self.chunk)
    }

    fn number(&mut self, _: bool) {
        let value = Value::Double(self.previous.data.parse().unwrap());
        trace!("Called number rule: {}", value);
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
            TokenType::Minus => emit_bytes!(OpCode::Negate; self.previous.line, self.chunk),
            TokenType::Bang => emit_bytes!(OpCode::Not; self.previous.line, self.chunk),
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
            TokenType::BangEqual => emit_bytes!(OpCode::Equal, OpCode::Not; self.previous.line, self.chunk),
            TokenType::EqualEqual => emit_bytes!(OpCode::Equal; self.previous.line, self.chunk),
            TokenType::Greater => emit_bytes!(OpCode::Greater; self.previous.line, self.chunk),
            TokenType::GreaterEqual => emit_bytes!(OpCode::Less, OpCode::Not; self.previous.line, self.chunk),
            TokenType::Less => emit_bytes!(OpCode::Less; self.previous.line, self.chunk),
            TokenType::LessEqual => emit_bytes!(OpCode::Greater, OpCode::Not; self.previous.line, self.chunk),
            TokenType::Plus => emit_bytes!(OpCode::Add; self.previous.line, self.chunk),
            TokenType::Minus => emit_bytes!(OpCode::Subtract; self.previous.line, self.chunk),
            TokenType::Star => emit_bytes!(OpCode::Multipliy; self.previous.line, self.chunk),
            TokenType::Slash => emit_bytes!(OpCode::Divide; self.previous.line, self.chunk),
            _ => {},
        }
    }

    fn literal(&mut self, _: bool) {
        trace!("Called literal rule: {}", self.previous.token_type);
        match self.previous.token_type {
            TokenType::False => emit_bytes!(OpCode::False; self.previous.line, self.chunk),
            TokenType::True => emit_bytes!(OpCode::True; self.previous.line, self.chunk),
            TokenType::Nil => emit_bytes!(OpCode::Nil; self.previous.line, self.chunk),
            _ => {},
        }
    }

    fn string(&mut self, _: bool) {
        self.emit_constant(Value::Obj(Object::String(self.previous.data.clone())));
    }
}