use std::{error::Error, fmt::Display, mem};
use crate::{chunk::{write_instruction, Chunk, OpCode}, compiler::Compiler, value::Value};

macro_rules! binary_op {
    ($line:expr, $stack:expr, $op:tt) => {
        {
            let b = $stack.pop().unwrap();
            let a = $stack.last_mut().unwrap();
            let temp = mem::take(a);
            *a = match temp $op b {
                Ok(val) => val,
                Err(err) => runtime_error!($line, $stack; "{}", err),
            };
        }
    };
}

macro_rules! binary_comp {
    ($line:expr, $stack:expr, $op:tt) => {
        {
            let b = $stack.pop().unwrap();
            let a = $stack.last_mut().unwrap();
            *a = Value::Bool(*a $op b);
        }
    }
}

macro_rules! runtime_error {
    ($line:expr, $stack:expr; $($arg:tt)+) => {
        {
            println!($($arg)*);
            println!("[line {}] in script", $line);
            $stack.clear();
            return Err(InterpretError::InterpretRuntimeError);
        }
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

pub struct VM {
    chunk: Chunk,
    stack: Vec<Value>
}

impl VM {
    pub fn new() -> Self {
        VM { chunk: Chunk::new(), stack: Vec::new() }
    }

    pub fn interpret(&mut self, source: String) -> Result<(), InterpretError> {
        let mut compiler = Compiler::new(&source);

        let chunk = compiler.compile().map_err(|_| InterpretError::InterpretCompileError)?;
        
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

            if cfg!(debug_assertions) {
                println!("DEBUG: Stack contents and current instruction:");
                for value in self.stack.iter() {
                    println!("[ {} ]", value);
                }

                let mut output = String::new();
                write_instruction(&mut output, &mut ip.clone(), op_code, self.chunk.get_lines()[counter], constants).unwrap();
                print!("{}", output);
            }

            match op_code {
                OpCode::Constant => {
                    let value = constants[*ip.next().unwrap() as usize].clone();
                    self.stack.push(value)
                },
                OpCode::ConstantLong => {
                    let value = constants[u16::from_le_bytes([*ip.next().unwrap(), *ip.next().unwrap()]) as usize].clone();
                    self.stack.push(value)
                },
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Equal => binary_comp!(self.chunk.get_lines()[counter], self.stack, ==),
                OpCode::Greater => binary_comp!(self.chunk.get_lines()[counter], self.stack, >),
                OpCode::Less => binary_comp!(self.chunk.get_lines()[counter], self.stack, <),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Add => binary_op!(self.chunk.get_lines()[counter], self.stack, +),
                OpCode::Subtract => binary_op!(self.chunk.get_lines()[counter], self.stack, -),
                OpCode::Multipliy => binary_op!(self.chunk.get_lines()[counter], self.stack, *),
                OpCode::Divide => binary_op!(self.chunk.get_lines()[counter], self.stack, /),
                OpCode::Not => {
                    let val = match self.stack.pop().unwrap() {
                        Value::Bool(b) => !b,
                        Value::Nil => true,
                        _ => false,
                    };

                    self.stack.push(Value::Bool(val));
                }
                OpCode::Negate => {
                    let value = self.stack.last_mut().unwrap();
                    let temp = mem::take(value);

                    *value = match -temp {
                        Ok(val) => val,
                        Err(err) => runtime_error!(self.chunk.get_lines()[counter], self.stack; "{}", err)
                    }
                },
                OpCode::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    return Ok(())
                },
            }

            counter += 1;
        }
    }
}