use std::{fmt::Display, iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, Copy, PartialEq)]
/// All token types.
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,
    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    // Literals.
    Identifier, String, Number,
    // Keywords.
    And, Class, Else, False,
    For, Fun, If, Nil, Or,
    Print, Return, Super, This,
    True, Var, While,

    EOF, NULL
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftParen => write!(f, "TOKEN_LEFT_PAREN"),
            Self::RightParen => write!(f, "TOKEN_RIGHT_PAREN"),
            Self::LeftBrace => write!(f, "TOKEN_LEFT_BRACE"),
            Self::RightBrace => write!(f, "TOKEN_RIGHT_BRACE"),
            Self::Comma => write!(f, "TOKEN_COMMA"),
            Self::Dot => write!(f, "TOKEN_DOT"),
            Self::Minus => write!(f, "TOKEN_MINUS"),
            Self::Plus => write!(f, "TOKEN_PLUS"),
            Self::Semicolon => write!(f, "TOKEN_SEMICOLON"),
            Self::Slash => write!(f, "TOKEN_SLASH"),
            Self::Star => write!(f, "TOKEN_STAR"),
            Self::Bang => write!(f, "TOKEN_BANG"),
            Self::BangEqual => write!(f, "TOKEN_BANG_EQUAL"),
            Self::Equal => write!(f, "TOKEN_EQUAL"),
            Self::EqualEqual => write!(f, "TOKEN_EQUAL_EQUAL"),
            Self::Greater => write!(f, "TOKEN_GREATER"),
            Self::GreaterEqual => write!(f, "TOKEN_GREATER_EQUAL"),
            Self::Less => write!(f, "TOKEN_LESS"),
            Self::LessEqual => write!(f, "TOKEN_LESS_EQUAL"),
            Self::Identifier => write!(f, "TOKEN_IDENTIFIER"),
            Self::String => write!(f, "TOKEN_STRING"),
            Self::Number => write!(f, "TOKEN_NUMBER"),
            Self::And => write!(f, "TOKEN_AND"),
            Self::Class => write!(f, "TOKEN_CLASS"),
            Self::Else => write!(f, "TOKEN_ELSE"),
            Self::False => write!(f, "TOKEN_FALSE"),
            Self::For => write!(f, "TOKEN_FOR"),
            Self::Fun => write!(f, "TOKEN_FUN"),
            Self::If => write!(f, "TOKEN_IF"),
            Self::Nil => write!(f, "TOKEN_NIL"),
            Self::Or => write!(f, "TOKEN_OR"),
            Self::Print => write!(f, "TOKEN_PRINT"),
            Self::Return => write!(f, "TOKEN_RETURN"),
            Self::Super => write!(f, "TOKEN_SUPER"),
            Self::This => write!(f, "TOKEN_THIS"),
            Self::True => write!(f, "TOKEN_TRUE"),
            Self::Var => write!(f, "TOKEN_VAR"),
            Self::While => write!(f, "TOKEN_WHILE"),
            Self::EOF => write!(f, "TOKEN_EOF"),
            Self::NULL => write!(f, "TOKEN_NULL"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// A struct representing a token. It contains the type, the data referenced by the token and the line the token was generated on.
pub struct Token<'a> {
    pub token_type: TokenType,
    pub data: &'a str,
    pub line: usize,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04} {:<19} \"{}\"", self.line, format!("{}", self.token_type), self.data)
    }
}

impl<'a> Token<'a> {
    pub fn new(source: &'a str) -> Self {
        Token { token_type: TokenType::EOF, data: source, line: 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParseErrorType {
    UnexpectedToken,
    UnterminatedString
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken => write!(f, "Unexpected character"),
            Self::UnterminatedString => write!(f, "Unterminated string"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParseError {
    line: usize,
    col: usize,
    error_type: ParseErrorType,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {} on line {} at column {}", self.error_type, self.line, self.col)
    }
}

#[derive(Debug)]
/// A scanner object that iterates through the source code and outputs tokens.
pub struct Scanner<'a> {
    line: usize,
    col: usize,
    current: usize,
    source: &'a str,
    iter: Peekable<CharIndices<'a>>
}

impl<'a> Scanner<'a> {
    /// Create a new scanner with the given source.
    pub fn new(source: &'a str) -> Self {
        Scanner { line: 1, col: 0, current: 0, source, iter: source.char_indices().peekable() }
    }

    /// Scan and produce the next token.
    /// 
    /// **Returns:**
    /// A [`Token`] if scanning was successful and a [`ParseError`] if there was an issue.
    pub fn scan_token(&mut self) -> Result<Token<'a>, ParseError> {
        let res = self.skip_whitespace();
        self.col += 1;

        if res.is_none() {
            return Ok(self.make_token(TokenType::EOF, ""))
        }

        let res = res.unwrap();

        let token_type = match res {
            (_, "(") => self.make_token(TokenType::LeftParen, res.1),
            (_, ")") => self.make_token(TokenType::RightParen, res.1),
            (_, "{") => self.make_token(TokenType::LeftBrace, res.1),
            (_, "}") => self.make_token(TokenType::RightBrace, res.1),
            (_, ";") => self.make_token(TokenType::Semicolon, res.1),
            (_, ",") => self.make_token(TokenType::Comma, res.1),
            (_, ".") => self.make_token(TokenType::Dot, res.1),
            (_, "-") => self.make_token(TokenType::Minus, res.1),
            (_, "+") => self.make_token(TokenType::Plus, res.1),
            (_, "/") => self.make_token(TokenType::Slash, res.1),
            (_, "*") => self.make_token(TokenType::Star, res.1),
            (index, "!") => {
                if self.match_next('=') {
                    self.make_token(TokenType::BangEqual, &self.source[index..self.current])
                } else {
                    self.make_token(TokenType::Bang, res.1)
                } 
            },
            (index, "=") => {
                if self.match_next('=') {
                    self.make_token(TokenType::EqualEqual, &self.source[index..self.current])
                } else {
                    self.make_token(TokenType::Equal, res.1)
                } 
            },
            (index, "<") => {
                if self.match_next('=') {
                    self.make_token(TokenType::LessEqual, &self.source[index..self.current])
                } else {
                    self.make_token(TokenType::Less, res.1)
                } 
            },
            (index, ">") => {
                if self.match_next('=') {
                    self.make_token(TokenType::GreaterEqual, &self.source[index..self.current])
                } else {
                    self.make_token(TokenType::Greater, res.1)
                } 
            },
            (_, "\"") => return self.string(),
            (index, c) if c.chars().all(|c| c.is_alphabetic()) => self.identifier(index),
            (index, c) if c.chars().all(|c| c.is_ascii_digit()) => self.number(index),
            _ => return Err(self.make_error(ParseErrorType::UnexpectedToken)),
        };

        Ok(token_type)
    }

    /// Create a string token.
    fn string(&mut self) -> Result<Token<'a>, ParseError> {
        let start = self.current;
        while let Some((index, char)) = self.iter.next() {
            match char {
                '\n' => { 
                    self.line += 1;
                    self.col = 1
                },
                '"' => { 
                    self.current = self.next_index();
                    self.col += 1;
                    return Ok(self.make_token(TokenType::String, &self.source[start..index]))
                },
                _ => { self.col += 1 },
            }
        }

        Err(self.make_error(ParseErrorType::UnterminatedString))
    }

    /// Create a new number token, taking care of decimal points correctly.
    fn number(&mut self, start: usize) -> Token<'a> {
        while self.iter.next_if(|(_, c)| c.is_ascii_digit()).is_some() { self.col += 1 }

        if let Some((_, '.')) = self.iter.peek() {
            // Create a new iter to allow two characters of lookahead
            let mut two_look = self.iter.clone();
            _ = two_look.next();

            match two_look.peek() {
                Some((_, c)) if c.is_ascii_digit() => {
                    _ = self.iter.next();
                    self.col += 1;
                    while self.iter.next_if(|(_, c)| c.is_ascii_digit()).is_some() { self.col += 1 }
                },
                _ => {}
            }
        }
        
        let end = self.next_index();
        self.make_token(TokenType::Number, &self.source[start..end])
    }

    /// Creates a new identifier token, checking whether the identifier is a key word first.
    fn identifier(&mut self, start: usize) -> Token<'a> {
        while let Some((_, c)) = self.iter.peek() {
            if !c.is_alphanumeric() { break }
            self.iter.next();
            self.col += 1;
        }

        let end = self.next_index();
        let ident = &self.source[start..end];

        match &self.source[start..end] {
            "and" =>    self.make_token(TokenType::And,         ident),
            "class" =>  self.make_token(TokenType::Class,       ident),
            "else" =>   self.make_token(TokenType::Else,        ident),
            "if" =>     self.make_token(TokenType::If,          ident),
            "nil" =>    self.make_token(TokenType::Nil,         ident),
            "or" =>     self.make_token(TokenType::Or,          ident),
            "print" =>  self.make_token(TokenType::Print,       ident),
            "return" => self.make_token(TokenType::Return,      ident),
            "super" =>  self.make_token(TokenType::Super,       ident),
            "var" =>    self.make_token(TokenType::Var,         ident),
            "while" =>  self.make_token(TokenType::While,       ident),
            "false" =>  self.make_token(TokenType::False,       ident),
            "for" =>    self.make_token(TokenType::For,         ident),
            "fun" =>    self.make_token(TokenType::Fun,         ident),
            "this" =>   self.make_token(TokenType::This,        ident),
            "true" =>   self.make_token(TokenType::True,        ident),
            _ =>        self.make_token(TokenType::Identifier,  ident),
        }
    }

    /// Find the end of the curent character in the source slice.
    fn next_index(&mut self) -> usize {
        self.iter.peek().map(|(index, _)| *index).unwrap_or(self.source.len())
    }

    /// Create a string reference to the next character.
    fn next_str(&mut self, index: usize) -> &'a str {
        &self.source[index..self.next_index()]
    }

    /// Get the next character, if it is whitespace or a comment then skip it, otherwise return it.
    fn skip_whitespace(&mut self) -> Option<(usize, &'a str)> {
        while let Some((index, c)) = self.iter.next() {
            match c {
                '\n' => {
                    self.line += 1;
                    self.col = 0
                },
                '/' => if let Some((index, str)) = self.skip_comment(index) { return Some((index, str)) },
                c if c.is_whitespace() => {},
                _ => {
                    self.current = self.next_index();
                    return Some((index, self.next_str(index)))
                }
            }
        }

        None
    }

    /// Checks if this is genuinely a comment and if so skips it, otherwise it is just a forward slash.
    fn skip_comment(&mut self, index: usize) -> Option<(usize, &'a str)> {
        if let Some((_, '/')) = self.iter.peek() {
            while let Some((_, c)) = self.iter.peek() {
                if *c != '\n' { break }
                self.iter.next();
            }
        } else { 
            self.current = self.next_index();
            return Some((index, self.next_str(index)))
        }

        None
    }

    /// If the next character macthes then advance and return it, otherwise return none.
    fn match_next(&mut self, check: char) -> bool {
        if let Some((_, c)) = self.iter.peek() {
            if *c == check {
                self.iter.next();
                self.col += 1;
                self.current = self.next_index();
                return true
            }
        }

        false
    }

    /// Create a new token with the given information.
    fn make_token(&self, token_type: TokenType, data: &'a str) -> Token<'a> {
        Token { token_type, data, line: self.line }
    }

    /// Create a new parse error based on the state of the scanner.
    fn make_error(&self, error_type: ParseErrorType) -> ParseError {
        ParseError { line: self.line, col: self.col, error_type }
    }
}