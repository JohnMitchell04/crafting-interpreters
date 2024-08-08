use std::{collections::HashMap, error::Error, fmt::Display, mem};
use crate::{chunk::{output_instruction, OpCode}, compiler::{Compiler, FunctionType}, value::{Function, NativeFn, Object, Value}};

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

// TODO: Add stack trace
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

macro_rules! is_falsey {
    ($value:expr) => {
        {
            match $value {
                Value::Bool(b) => !b,
                Value::Nil => true,
                _ => false,
            }
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

#[derive(Debug, Clone)]
struct CallFrame {
    pub function: Function,
    pub ip: usize,
    pub slot_start: usize,
}

impl CallFrame {
    pub fn get_ip(&self) -> usize {
        self.ip
    }

    pub fn get_ip_mut(&mut self) -> &mut usize {
        &mut self.ip
    }

    pub fn get_constants(&self) -> &[Value] {
        self.function.chunk.get_constants()
    }

    pub fn get_lines(&self) -> &[i32] {
        self.function.chunk.get_lines()
    }

    pub fn next(&mut self) -> Option<u8> {
        if self.ip >= self.function.chunk.get_code_count() {
            None
        } else {
            self.ip += 1;
            Some(self.function.chunk.get_code()[self.ip - 1])
        }
    }

    // TODO: Clean this function up
    #[cfg(debug_assertions)]
    pub fn debug_info(&self, op_code: OpCode, line_counter: usize) {
        println!("DEBUG: Current instruction: ");
        let mut output = String::new();
        output_instruction(
            &mut output,
            &mut self.function.chunk.get_code_iter().skip(self.ip),
            op_code, 
            self.get_lines()[line_counter], 
            self.get_constants()
        ).unwrap();
        print!("{}", output);
        println!("{}", "-".repeat(50));
    }
}

const FRAMES_MAX: usize = 64;

fn test_native(arg_count: u8, args: &[Value]) -> Value {
    Value::Obj(Object::String(String::from("Test function")))
}

pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    operation_counter: usize,
    frames: Vec<CallFrame>,
}

// TODO: There are a lot of clones creating duplicate values, ideally these should be cleaned up
impl VM {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("test".to_string(), Value::Obj(Object::NativeFunction(test_native))); 

        VM { 
            stack: Vec::with_capacity(FRAMES_MAX * u16::MAX as usize),
            globals,
            operation_counter: 0,
            frames: Vec::with_capacity(FRAMES_MAX)
        }
    }

    pub fn interpret(&mut self, source: String) -> Result<(), InterpretError> {
        let compiler = Compiler::new(FunctionType::Script, &source);
        let function = compiler.compile().map_err(|_| InterpretError::InterpretCompileError)?;
        
        // TODO: this probably shouldn't be a clone
        let frame = CallFrame { function: function.clone(), ip: 0, slot_start: self.stack.len() };
        self.frames.push(frame.clone());
        self.stack.push(Value::Obj(Object::Function(function)));

        self.run()?;
        Ok(())
    }

    // TODO: Re-write this function as it is very hacky right now, ideally split into smaller function to avoid lifetimes intersecting
    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            let op_code = {
                let frame = self.frames.last_mut().unwrap();
                match &frame.next() {
                    Some(val) => val,
                    None => return Ok(()),
                }.into()
            };

            #[cfg(debug_assertions)]
            {
                self.debug_info();
                let frame = self.frames.last_mut().unwrap();
                frame.debug_info(op_code, self.operation_counter);
            }

            let frame = self.frames.last_mut().unwrap();
            match op_code {
                OpCode::Constant => {
                    let loc = frame.next().unwrap() as usize;
                    let value = frame.get_constants()[loc].clone();
                    self.stack.push(value)
                },
                OpCode::ConstantLong => {
                    let loc = u16::from_le_bytes([frame.next().unwrap(), frame.next().unwrap()]) as usize;
                    let value = frame.get_constants()[loc].clone();
                    self.stack.push(value)
                },
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Pop => _ = self.stack.pop(),
                OpCode::GetGlobal => {
                    let global = self.stack.pop().unwrap().get_string().unwrap();
                    let value = self.globals.get(&global);
                    match value {
                        Some(value) => self.stack.push(value.clone()),
                        None => runtime_error!(frame.get_lines()[self.operation_counter], self.stack; "Undefined variable: \"{}\"", global)
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
                        None => runtime_error!(frame.get_lines()[self.operation_counter], self.stack; "Undefined variable: \"{}\"", global),
                    }
                },
                OpCode::GetLocal => {
                    let value = self.stack[frame.slot_start + frame.next().unwrap() as usize].clone();
                    self.stack.push(value);
                },
                OpCode::SetLocal => {
                    let slot = frame.next().unwrap() as usize;
                    self.stack[frame.slot_start + slot] = self.stack.last().unwrap().clone()
                },
                OpCode::Equal => binary_comp!(lines[self.counter], self.stack, ==),
                OpCode::Greater => binary_comp!(lines[self.counter], self.stack, >),
                OpCode::Less => binary_comp!(lines[self.counter], self.stack, <),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Add => binary_op!(frame.get_lines()[self.operation_counter], self.stack, +),
                OpCode::Subtract => binary_op!(frame.get_lines()[self.operation_counter], self.stack, -),
                OpCode::Multipliy => binary_op!(frame.get_lines()[self.operation_counter], self.stack, *),
                OpCode::Divide => binary_op!(frame.get_lines()[self.operation_counter], self.stack, /),
                OpCode::Not => {
                    let val = is_falsey!(self.stack.pop().unwrap());
                    self.stack.push(Value::Bool(val))
                },
                OpCode::Negate => {
                    let value = self.stack.last_mut().unwrap();
                    let temp = mem::take(value);

                    *value = match -temp {
                        Ok(val) => val,
                        Err(err) => runtime_error!(frame.get_lines()[self.operation_counter], self.stack; "{}", err)
                    }
                },
                OpCode::Print => println!("{}", self.stack.pop().unwrap()),
                OpCode::Jump => {
                    let offset = u16::from_le_bytes([frame.next().unwrap(), frame.next().unwrap()]);
                    *frame.get_ip_mut() += offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = u16::from_le_bytes([frame.next().unwrap(), frame.next().unwrap()]);
                    if is_falsey!(self.stack.last().unwrap()) { *frame.get_ip_mut() += offset as usize}
                },
                OpCode::Loop => {
                    let offset = u16::from_le_bytes([frame.next().unwrap(), frame.next().unwrap()]);
                    *frame.get_ip_mut() -= offset as usize;
                },
                OpCode::Call => {
                    self.call_value();
                },
                OpCode::Return => {
                    let res = self.stack.pop().unwrap();
                    let index = self.frames.pop().unwrap().slot_start;
                    if self.frames.len() == 0 {
                        self.frames.pop();
                        return Ok(())
                    }

                    while self.stack.len() != index {
                        self.stack.pop();
                    }

                    self.stack.push(res);
                },
            }

            // TODO: Find some way to add line counts back
            // self.operation_counter += 1;
        }
    }

    fn call_value(&mut self) -> Result<(), InterpretError> {
        let frame = self.frames.last_mut().unwrap();
        let arg_count = frame.next().unwrap();
        let lines = frame.get_lines();

        match self.stack[self.stack.len() - 1 - arg_count as usize].clone() {
            Value::Obj(Object::Function(func)) => self.call(func, arg_count),
            Value::Obj(Object::NativeFunction(fun)) => {
                let res = fun(arg_count, &self.stack[(self.stack.len() - arg_count as usize)..self.stack.len()]);

                for _ in 0..arg_count + 1 {
                    self.stack.pop();
                }

                self.stack.push(res);
                Ok(())
            },
            _ => runtime_error!(lines[self.operation_counter], self.stack; "Can only call functions and classes"),
        }
    }

    fn call(&mut self, function: Function, arg_count: u8) -> Result<(), InterpretError> {
        if function.arity != arg_count {
            runtime_error!(self.frames.last().unwrap().get_lines()[self.operation_counter], self.stack; "Expected {} args but got {}", function.arity, arg_count)
        }

        if self.frames.len() == FRAMES_MAX {
            runtime_error!(self.frames.last().unwrap().get_lines()[self.operation_counter], self.stack; "Stack overflow");
        }

        self.frames.push(CallFrame { function, ip: 0, slot_start: self.stack.len() - 1 - arg_count as usize });
        Ok(())
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        self.stack.push(Value::Obj(Object::String(name.to_string())));
        self.stack.push(Value::Obj(Object::NativeFunction(function)));
        self.globals.insert(name.to_string(), self.stack.last().unwrap().clone());
        self.stack.pop();
        self.stack.pop();
    }

    #[cfg(debug_assertions)]
    fn debug_info(&self) {
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
            println!("[ {} ]", value);
            some = true;
        }

        if !some { println!("NONE"); }
    }
}