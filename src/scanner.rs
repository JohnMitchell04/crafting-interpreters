use std::{fmt::Display, iter::Peekable, str::Chars};

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

    EOF
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// A struct representing a token. It contains the type, the data held by the token and the line the token was generated on.
pub struct Token {
    pub token_type: TokenType,
    pub data: String,
    pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04} {: <19} {}", self.line, self.token_type, self.data)
    }
}

impl Token {
    pub fn new() -> Self {
        Token { token_type: TokenType::EOF, data: String::new(), line: 0 }
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
// TODO: Add more information to the parse error.
pub struct ParseError {
    line: usize,
    error_type: ParseErrorType,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {} at {}", self.error_type, self.line)
    }
}

#[derive(Debug)]
/// A scanner object that iterates through the source code and outputs tokens.
pub struct Scanner<'a> {
    line: usize,
    source: Peekable<Chars<'a>>,
}

// TODO: The scanner and tokens should be changed to string slices to improve performance
impl<'a> Scanner<'a> {
    /// Create a new scanner with the given source.
    pub fn new(source: Peekable<Chars<'a>>) -> Self {
        Scanner { line: 0, source }
    }

    /// Scan and produce the next token.
    /// 
    /// # Returns:
    /// A [`Token`] if scanning was successful and a [`ParseError`] if there was an issue.
    pub fn scan_token(&mut self) -> Result<Token, ParseError> {
        let cur_char = self.skip_whitespace();

        let token_type = match cur_char {
            Some('(') => self.make_token(TokenType::LeftParen, "("),
            Some(')') => self.make_token(TokenType::RightParen, ")"),
            Some('{') => self.make_token(TokenType::LeftBrace, "{"),
            Some('}') => self.make_token(TokenType::RightBrace, "}"),
            Some(';') => self.make_token(TokenType::Semicolon, ";"),
            Some(',') => self.make_token(TokenType::Comma, ","),
            Some('.') => self.make_token(TokenType::Dot, "."),
            Some('-') => self.make_token(TokenType::Minus, "-"),
            Some('+') => self.make_token(TokenType::Plus, "+"),
            Some('/') => self.make_token(TokenType::Slash, "/"),
            Some('*') => self.make_token(TokenType::Star, "*"),
            Some('!') => if self.match_next('=') { self.make_token(TokenType::BangEqual, "!=") } else { self.make_token(TokenType::Bang, "!") },
            Some('=') => if self.match_next('=') { self.make_token(TokenType::EqualEqual, "==") } else { self.make_token(TokenType::Equal, "=") },
            Some('<') => if self.match_next('=') { self.make_token(TokenType::LessEqual, "<=") } else { self.make_token(TokenType::Less, "<") },
            Some('>') => if self.match_next('=') { self.make_token(TokenType::GreaterEqual, ">=") } else { self.make_token(TokenType::Greater, ">") },
            Some('"') => return self.string(),
            Some(c) if c.is_alphabetic() => self.identifier(c),
            Some(c) if c.is_ascii_digit() => self.number(c),
            Some(_) => return Err(self.make_error(ParseErrorType::UnexpectedToken)),
            None => return Ok(self.make_token(TokenType::EOF, "")),
        };

        Ok(token_type)
    }

    fn string(&mut self) -> Result<Token, ParseError> {
        let mut output = String::new();

        while let Some(char) = self.source.next() {
            match char {
                '\n' => self.line += 1,
                '"' => return Ok(self.make_token(TokenType::String, &output)),
                _ => output.push(char),
            }
        }

        Err(self.make_error(ParseErrorType::UnterminatedString))
    }

    fn number(&mut self, char: char) -> Token {
        let mut output = String::from(char);
        while let Some(c) = self.source.next_if(|c| c.is_ascii_digit()) {
            output.push(c);
        }

        if self.source.peek() == Some(&'.') {
            let mut two_look = self.source.clone();
            _ = two_look.next();
            match two_look.peek() {
                Some(c) if c.is_ascii_digit() => {
                    output.push(self.source.next().unwrap());
                    while let Some(c) = self.source.next_if(|c| c.is_ascii_digit()) {
                        output.push(c);
                    }

                    return self.make_token(TokenType::Number, &output)
                },
                _ => {}
            }
        }
        
        self.make_token(TokenType::Number, &output)
    }

    fn identifier(&mut self, char: char) -> Token {
        let mut output = String::from(char);
        while let Some(c) = self.source.peek() {
            if !c.is_alphanumeric() { break }

            output.push(*c);
            self.source.next();
        }

        match output.as_str() {
            "and" => self.make_token(TokenType::And, &output),
            "class" => self.make_token(TokenType::Class, &output),
            "else" => self.make_token(TokenType::Else, &output),
            "if" => self.make_token(TokenType::If, &output),
            "nil" => self.make_token(TokenType::Nil, &output),
            "or" => self.make_token(TokenType::Or, &output),
            "print" => self.make_token(TokenType::Print, &output),
            "return" => self.make_token(TokenType::Return, &output),
            "super" => self.make_token(TokenType::Super, &output),
            "var" => self.make_token(TokenType::Var, &output),
            "while" => self.make_token(TokenType::While, &output),
            "false" => self.make_token(TokenType::False, &output),
            "for" => self.make_token(TokenType::For, &output),
            "fun" => self.make_token(TokenType::Fun, &output),
            "this" => self.make_token(TokenType::This, &output),
            "true" => self.make_token(TokenType::True, &output),
            _ => self.make_token(TokenType::Identifier, &output)
        }
    }

    fn skip_whitespace(&mut self) -> Option<char> {
        // Get the next character, if it is whitespace or a comment then skip it, otherwise return it
        loop {
            match self.source.next() {
                Some('\n') => { self.line += 1 },
                Some('/') => {
                    if let Some('/') = self.source.peek() {
                        while let Some(&c) = self.source.peek() {
                            if c == '\n' { break }
                            
                            self.source.next();
                        }
                    } else {
                        return Some('/');
                    }
                },
                Some(c) if c.is_whitespace() => {},
                c => return c,
            }
        }
    }

    fn match_next(&mut self, check: char) -> bool {
        match self.source.peek() {
            Some(&c) if c == check => {
                self.source.next();
                true
            }
            _ => false
        }
    }

    fn make_token(&mut self, token_type: TokenType, data: &str) -> Token {
        Token { token_type, data: data.to_string(), line: self.line }
    }

    fn make_error(&self, error_type: ParseErrorType) -> ParseError {
        ParseError { line: self.line, error_type }
    }
}