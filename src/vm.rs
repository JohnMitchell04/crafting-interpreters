use std::{error::Error, fmt::Display};
use crate::{chunk::{write_instruction, Chunk, OpCode}, value::Value};

// TODO: This can be rewritten to avoid the last pop and the push
macro_rules! binary_op {
    ($stack:expr, $op:tt) => {
        let b = $stack.pop().unwrap();
        let a = $stack.pop().unwrap();
        $stack.push(a $op b);
    };
}

#[derive(Debug, Clone, Copy)]
pub enum InterpretError {
    InterpretCompileError,
    InterpretRuntimeError,
}

// TODO: Improve this error reporting
impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InterpretCompileError => write!(f, "There was an error when compiling the program"),
            Self::InterpretRuntimeError => write!(f, "There was an error when running the program")
        }
    }
}

impl Error for InterpretError {}

pub struct VMOptions {
    pub debug_trace: bool,
}

pub struct VM {
    chunk: Chunk,
    options: VMOptions,
    stack: Vec<Value>
}

impl VM {
    pub fn new(options: VMOptions) -> Self {
        VM { chunk: Chunk::new(), options, stack: Vec::new() }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), InterpretError> {
        self.chunk = chunk;
        self.run()
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        let mut ip = self.chunk.get_code_iter();
        let mut counter = 0;
        let constants = self.chunk.get_constants();
        
        loop {
            // TODO: Improve error handling here
            let op_code: OpCode = ip.next().unwrap().try_into().unwrap();
            counter += 1;

            if self.options.debug_trace {
                for value in self.stack.iter() {
                    println!("[ {} ]", value);
                }

                let mut output = String::new();
                write_instruction(&mut output, &mut ip.clone(), op_code, self.chunk.get_lines()[counter], constants).unwrap();
                print!("{}", output);
            }

            match op_code {
                OpCode::Constant => {
                    let value = constants[*ip.next().unwrap() as usize];
                    self.stack.push(value)
                },
                OpCode::ConstantLong => {
                    let value = constants[u16::from_le_bytes([*ip.next().unwrap(), *ip.next().unwrap()]) as usize];
                    self.stack.push(value)
                },
                OpCode::Negate => {
                    if let Value::Double(ref mut d) = self.stack.last_mut().unwrap() {
                        *d = -*d;
                    }
                },
                OpCode::Add => { binary_op!(self.stack, +); },
                OpCode::Subtract => { binary_op!(self.stack, -); },
                OpCode::Multipliy => { binary_op!(self.stack, *); },
                OpCode::Divide => { binary_op!(self.stack, /); },
                OpCode::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    return Ok(())
                },
            }
        }
    }
}