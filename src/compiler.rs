use crate::{chunk::Chunk, scanner::{Scanner, TokenType}};

pub struct Compiler {

}

impl Compiler {
    pub fn new() -> Self {
        Compiler {  }
    }

    pub fn compile(&mut self, source: String, chunk: &mut Chunk) -> bool {
        let mut scanner = Scanner::new(source.chars().peekable());
    
        let mut line = 0;
        loop {
            let token = scanner.scan_token().unwrap();
            println!("{}", token);
            line = token.line;
    
            if token.token_type == TokenType::EOF { break; }
        }

        true
    }
}