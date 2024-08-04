use std::{collections::HashMap, error::Error, fmt::Display, mem};
use crate::{chunk::{output_instruction, Chunk, OpCode}, compiler::Compiler, value::Value};

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
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    instruction_counter: usize,
}

impl VM {
    pub fn new() -> Self {
        VM { chunk: Chunk::new(), stack: Vec::new(), globals: HashMap::new(), instruction_counter: 0 }
    }

    pub fn interpret(&mut self, source: String) -> Result<(), InterpretError> {
        let mut compiler = Compiler::new(&mut self.chunk, &source);

        match compiler.compile() {
            Ok(_) => {},
            Err(_) => {
                self.chunk.clear_code();
                return Err(InterpretError::InterpretCompileError)
            }
        }
        self.run()?;
        self.chunk.clear_code();
        Ok(())
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        let mut ip = self.chunk.get_code_iter();
        let constants = self.chunk.get_constants();
        let lines = self.chunk.get_lines();
        
        loop {
            let op_code = match ip.next() {
                Some(code) => code.try_into().unwrap(),
                None => return Ok(()),
            };

            if cfg!(debug_assertions) {
                let mut some = false;
                println!("DEBUG: Globals:");
                for key in self.globals.keys() {
                    println!("Name: \"{}\" Value: {}", key, self.globals[key]);
                    some = true;
                }

                if !some { println!("NONE"); }

                some = false;
                println!("DEBUG: Stack contents:");
                for value in self.stack.iter() {
                    println!("[ {:?} ]", value);
                    some = true;
                }

                if !some { println!("NONE"); }

                println!("DEBUG: Current instruction: ");
                let mut output = String::new();
                output_instruction(&mut output, &mut ip.clone(), op_code, lines[self.instruction_counter], constants).unwrap();
                print!("{}", output);
                println!("{}", "-".repeat(50));
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
                OpCode::Pop => _ = self.stack.pop(),
                OpCode::GetGlobal => {
                    let global = self.stack.pop().unwrap().get_string().unwrap();
                    let value = self.globals.get(&global);
                    match value {
                        Some(value) => self.stack.push(value.clone()),
                        None => runtime_error!(lines[self.instruction_counter], self.stack; "Undefined variable: \"{}\"", global)
                    }
                },
                OpCode::DefineGlobal => {
                    let value = self.stack.pop().unwrap();
                    self.globals.insert(self.stack.last().unwrap().get_string().unwrap().clone(), value);
                    self.stack.pop();
                },
                OpCode::SetGlobal => {
                    let value = self.stack.pop().unwrap();
                    let global = self.stack.pop().unwrap().get_string().unwrap();
                    match self.globals.get(&global) {
                        Some(_) => {
                            self.globals.insert(global, value.clone());
                            self.stack.push(value);
                        },
                        None => runtime_error!(lines[self.instruction_counter], self.stack; "Undefined variable: \"{}\"", global),
                    }
                },
                OpCode::Equal => binary_comp!(lines[self.counter], self.stack, ==),
                OpCode::Greater => binary_comp!(lines[self.counter], self.stack, >),
                OpCode::Less => binary_comp!(lines[self.counter], self.stack, <),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Add => binary_op!(lines[self.instruction_counter], self.stack, +),
                OpCode::Subtract => binary_op!(lines[self.instruction_counter], self.stack, -),
                OpCode::Multipliy => binary_op!(lines[self.instruction_counter], self.stack, *),
                OpCode::Divide => binary_op!(lines[self.instruction_counter], self.stack, /),
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
                        Err(err) => runtime_error!(lines[self.instruction_counter], self.stack; "{}", err)
                    }
                },
                OpCode::Print => println!("{}", self.stack.pop().unwrap()),
                OpCode::Return => return Ok(()),
            }

            self.instruction_counter += 1;
        }
    }
}