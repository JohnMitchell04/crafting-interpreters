use std::{error::Error, fmt::Display};
use crate::{chunk::{Chunk, OpCode}, scanner::{Scanner, Token, TokenType}, value::Value};

/// Write a number of bytes to the chunk's code.
macro_rules! emit_bytes {
    ($($arg:expr),+; $line:expr, $chunk:expr) => {
        {
            $(
                $chunk.write_instruction($arg, $line as i32);
            )+
        }
    };
}

/// Print debug information.
macro_rules! trace {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*)
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

type CompileFn<'b> = fn(&mut Compiler<'b>);

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
        ParseRule { prefix: Some(Compiler::grouping), infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: Some(Compiler::unary), infix: Some(Compiler::binary), precedence: Precedence::Term },
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor },
        ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: Some(Compiler::number), infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
        ParseRule { prefix: None, infix: None, precedence: Precedence::None },
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
    chunk: Chunk,
}

impl<'a> Compiler<'a> {
    /// Create a new compiler for the source code.
    pub fn new(source: &'a str) -> Self {
        let scanner = Scanner::new(source.chars().peekable());
        Compiler { current: Token::new(), previous: Token::new(), scanner, had_error: false, panic_mode: false, chunk: Chunk::new() }
    }

    // TOOD: Maybe change back to the book version and have the chunk be passed as a mutable reference
    pub fn compile(&mut self) -> Result<Chunk, CompileError> {
        self.advance();
        self.expression();
        self.consume(TokenType::EOF, "Expect end of expression");

        emit_bytes!(OpCode::Return as u8; self.previous.line, self.chunk);

        if cfg!(debug_assertions) {
            if !self.had_error {
                println!("{}", self.chunk);
            }
        }
        
        if self.had_error {
            Err(CompileError::ParseError)
        } else {
            Ok(self.chunk.clone())
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

        print!(" {}", message);
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
            emit_bytes!(OpCode::Constant as u8, location as u8; self.previous.line, self.chunk);
        } else {
            emit_bytes!(OpCode::ConstantLong as u8; self.previous.line, self.chunk);
            self.chunk.write_long_constant_location(location as u16, self.previous.line as i32);

        }
    }

    fn parse_precendence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = get_rule(self.previous.token_type).prefix;
        if let Some(prefix_fn) = prefix_rule {
            prefix_fn(self);
        } else {
            self.error(false, "Expect expression");
            return;
        }

        // Continue parsing until the infix operator is no longer valid 
        while precedence <= get_rule(self.current.token_type).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.token_type).infix.unwrap();
            infix_rule(self)
        }
    }

    fn expression(&mut self) {
        trace!("Calling expression rule");
        self.parse_precendence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value = Value::Double(self.previous.data.parse().unwrap());
        trace!("Called number rule: {}", value);
        self.emit_constant(value);
    }

    fn grouping(&mut self) {
        trace!("Called grouping rule");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type = self.previous.token_type;
        trace!("Called unary rule: {}", operator_type);

        self.parse_precendence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => emit_bytes!(OpCode::Negate as u8; self.previous.line, self.chunk),
            _ => { return }
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous.token_type;
        let rule = get_rule(operator_type);
        let prec = (&(rule.precedence as u8 + 1)).into();

        trace!("Called binary rule: {}, with precedence: {}", operator_type, prec);

        self.parse_precendence(prec);

        match operator_type {
            TokenType::Plus => emit_bytes!(OpCode::Add as u8; self.previous.line, self.chunk),
            TokenType::Minus => emit_bytes!(OpCode::Subtract as u8; self.previous.line, self.chunk),
            TokenType::Star => emit_bytes!(OpCode::Multipliy as u8; self.previous.line, self.chunk),
            TokenType::Slash => emit_bytes!(OpCode::Divide as u8; self.previous.line, self.chunk),
            _ => {},
        }
    }
}