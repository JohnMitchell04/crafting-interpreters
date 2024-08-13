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
        Token { token_type: TokenType::NULL, data: source, line: 0 }
    }

    pub fn new_specified(token_type: TokenType, data: &'a str, line: usize) -> Self {
        Token { token_type, data, line }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParseError {
    line: usize,
    col: usize,
    message: &'static str,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {} on line {} at column {}", self.message, self.line, self.col)
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
        Scanner { line: 1, col: 1, current: 0, source, iter: source.char_indices().peekable() }
    }

    /// Scan and produce the next token.
    /// 
    /// **Returns:**
    /// A [`Token`] if scanning was successful and a [`ParseError`] if there was an issue.
    pub fn scan_token(&mut self) -> Result<Token<'a>, ParseError> {
        let res = self.skip_whitespace();

        if res.is_none() { return Ok(Token::new_specified(TokenType::EOF, "", self.line)) }
        let res = res.unwrap();

        let token_type = match res {
            (_, "(") => Token::new_specified(TokenType::LeftParen, res.1, self.line),
            (_, ")") => Token::new_specified(TokenType::RightParen, res.1, self.line),
            (_, "{") => Token::new_specified(TokenType::LeftBrace, res.1, self.line),
            (_, "}") => Token::new_specified(TokenType::RightBrace, res.1, self.line),
            (_, ";") => Token::new_specified(TokenType::Semicolon, res.1, self.line),
            (_, ",") => Token::new_specified(TokenType::Comma, res.1, self.line),
            (_, ".") => Token::new_specified(TokenType::Dot, res.1, self.line),
            (_, "-") => Token::new_specified(TokenType::Minus, res.1, self.line),
            (_, "+") => Token::new_specified(TokenType::Plus, res.1, self.line),
            (_, "/") => Token::new_specified(TokenType::Slash, res.1, self.line),
            (_, "*") => Token::new_specified(TokenType::Star, res.1, self.line),
            (index, "!") => {
                if self.match_next('=') {
                    Token::new_specified(TokenType::BangEqual, &self.source[index..self.current], self.line)
                } else {
                    Token::new_specified(TokenType::Bang, res.1, self.line)
                } 
            },
            (index, "=") => {
                if self.match_next('=') {
                    Token::new_specified(TokenType::EqualEqual, &self.source[index..self.current], self.line)
                } else {
                    Token::new_specified(TokenType::Equal, res.1, self.line)
                } 
            },
            (index, "<") => {
                if self.match_next('=') {
                    Token::new_specified(TokenType::LessEqual, &self.source[index..self.current], self.line)
                } else {
                    Token::new_specified(TokenType::Less, res.1, self.line)
                } 
            },
            (index, ">") => {
                if self.match_next('=') {
                    Token::new_specified(TokenType::GreaterEqual, &self.source[index..self.current], self.line)
                } else {
                    Token::new_specified(TokenType::Greater, res.1, self.line)
                } 
            },
            (_, "\"") => return self.string(),
            (index, c) if c.chars().all(|c| c.is_alphabetic()) => self.identifier(index),
            (index, c) if c.chars().all(|c| c.is_ascii_digit()) => self.number(index),
            _ => return Err(self.make_error("Unexpected Token")),
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
                    return Ok(Token::new_specified(TokenType::String, &self.source[start..index], self.line))
                },
                _ => { self.col += 1 },
            }
        }

        Err(self.make_error("Unterminated String"))
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
        Token::new_specified(TokenType::Number, &self.source[start..end], self.line)
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

        match ident {
            "and" =>    Token::new_specified(TokenType::And,         ident, self.line),
            "class" =>  Token::new_specified(TokenType::Class,       ident, self.line),
            "else" =>   Token::new_specified(TokenType::Else,        ident, self.line),
            "if" =>     Token::new_specified(TokenType::If,          ident, self.line),
            "nil" =>    Token::new_specified(TokenType::Nil,         ident, self.line),
            "or" =>     Token::new_specified(TokenType::Or,          ident, self.line),
            "print" =>  Token::new_specified(TokenType::Print,       ident, self.line),
            "return" => Token::new_specified(TokenType::Return,      ident, self.line),
            "super" =>  Token::new_specified(TokenType::Super,       ident, self.line),
            "var" =>    Token::new_specified(TokenType::Var,         ident, self.line),
            "while" =>  Token::new_specified(TokenType::While,       ident, self.line),
            "false" =>  Token::new_specified(TokenType::False,       ident, self.line),
            "for" =>    Token::new_specified(TokenType::For,         ident, self.line),
            "fun" =>    Token::new_specified(TokenType::Fun,         ident, self.line),
            "this" =>   Token::new_specified(TokenType::This,        ident, self.line),
            "true" =>   Token::new_specified(TokenType::True,        ident, self.line),
            _ =>        Token::new_specified(TokenType::Identifier,  ident, self.line),
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
            self.col += 1;
            match c {
                '\n' => {
                    self.line += 1;
                    self.col = 1
                },
                '/' => if let Some((index, str)) = self.skip_comment(index) { return Some((index, str)) },
                c if c.is_whitespace() => (),
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
                if *c == '\n' { break }
                self.iter.next();
            }
        } else {
            self.current = self.next_index();
            return Some((index, self.next_str(index)))
        }

        None
    }

    /// If the next character matches then advance and return true, otherwise return false.
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

    /// Create a new parse error based on the state of the scanner.
    fn make_error(&self, message: &'static str) -> ParseError {
        ParseError { line: self.line, col: self.col, message }
    }
}