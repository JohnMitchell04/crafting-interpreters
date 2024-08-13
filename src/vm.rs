use std::{collections::HashMap, error::Error, fmt::Display, mem, rc::Rc};
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

/// Maximum number of call frames the VM can have.
const FRAMES_MAX: usize = 64;

#[derive(Debug, Clone)]
/// A call frame that the VM uses to execute code.
struct CallFrame {
    function: Rc<Function>,
    ip: usize,
    slot_start: usize,
}

impl CallFrame {
    /// Get a slice containing the function's constants.
    pub fn get_constants(&self) -> &[Value] {
        self.function.chunk.get_constants()
    }

    /// Get a slice containing the function's constants.
    pub fn get_lines(&self) -> &[i32] {
        self.function.chunk.get_lines()
    }

    /// Retrieve the next op code.
    pub fn next(&mut self) -> Option<u8> {
        if self.ip >= self.function.chunk.get_code_count() {
            None
        } else {
            self.ip += 1;
            Some(self.function.chunk.get_code()[self.ip - 1])
        }
    }

    #[cfg(debug_assertions)]
    /// Print debug information about the frame's state
    pub fn debug_info(&self, line_counter: usize) {
        println!("DEBUG: Current instruction: ");

        let op_code = (&self.function.chunk.get_code()[self.ip - 1]).into();
        let mut output = String::new();
        let mut iter = self.function.chunk.get_code_iter().skip(self.ip);
        output_instruction(&mut output, &mut iter, op_code, self.get_lines()[line_counter], self.get_constants()).unwrap();
        
        print!("{}", output);
        println!("{}", "-".repeat(50));
    }
}

/// Temporary native test function that just puts "Test Function" on the stack for now.
fn test_native(arg_count: u8, args: &[Value]) -> Value {
    Value::Obj(Object::String(String::from("Test function")))
}

/// A VM object that compiles and executes code.
pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    operation_counter: usize,
    frames: Vec<CallFrame>,
}

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

    /// Interpret the given source code, this function will compile and execute the code.
    /// 
    /// **Arguments:**
    /// - `source` - The provided source code.
    /// 
    /// **Returns:**
    pub fn interpret(&mut self, source: String) -> Result<(), InterpretError> {
        // Compile provided source code
        let compiler = Compiler::new(FunctionType::Script, &source);
        let function = compiler.compile().map_err(|_| InterpretError::InterpretCompileError)?;

        // Place the anonymous function that encloses the source code onto the VM        
        let fn_ref = Rc::new(function);
        self.frames.push(CallFrame { function: fn_ref.clone(), ip: 0, slot_start: self.stack.len() });
        self.stack.push(Value::Obj(Object::Function(fn_ref)));

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
            self.debug_info();

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
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
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
                    frame.ip += offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = u16::from_le_bytes([frame.next().unwrap(), frame.next().unwrap()]);
                    if is_falsey!(self.stack.last().unwrap()) { frame.ip += offset as usize }
                },
                OpCode::Loop => {
                    let offset = u16::from_le_bytes([frame.next().unwrap(), frame.next().unwrap()]);
                    frame.ip -= offset as usize;
                },
                OpCode::Call => self.call_value()?,
                OpCode::Return => {
                    let res = self.stack.pop().unwrap();
                    let index = self.frames.pop().unwrap().slot_start;
                    if self.frames.is_empty() {
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

    /// Call a function on the stack.
    /// 
    /// **Returns:**
    /// An [`InterpretError`] if the call is invalid, nothing otherwise.
    fn call_value(&mut self) -> Result<(), InterpretError> {
        let frame = self.frames.last_mut().unwrap();
        let arg_count = frame.next().unwrap();

        match &self.stack[self.stack.len() - 1 - arg_count as usize] {
            Value::Obj(Object::Function(func)) => self.call(func.clone(), arg_count),
            Value::Obj(Object::NativeFunction(func)) => Ok(self.call_native(*func, arg_count)),
            _ => runtime_error!(frame.get_lines()[self.operation_counter], self.stack; "Can only call functions and classes"),
        }
    }

    /// Call a function by placing a new [`CallFrame`] containing the provided function on the VM's frames.
    /// This function ensures the user has provided the correct number of arguments and a stack overflow will not occur.
    /// 
    /// **Arguments:**
    /// - `function` - The [`Function`] to call.
    /// - `arg_count` - The number of arguments povided by the user.
    /// 
    /// **Returns:**
    /// An [`InterpretError`] if the call is invalid, nothing otherwise.
    fn call(&mut self, function: Rc<Function>, arg_count: u8) -> Result<(), InterpretError> {
        if function.arity != arg_count {
            runtime_error!(self.frames.last().unwrap().get_lines()[self.operation_counter], self.stack; "Expected {} args but got {}", function.arity, arg_count)
        }

        if self.frames.len() == FRAMES_MAX {
            runtime_error!(self.frames.last().unwrap().get_lines()[self.operation_counter], self.stack; "Stack overflow");
        }

        self.frames.push(CallFrame { function, ip: 0, slot_start: self.stack.len() - 1 - arg_count as usize });
        Ok(())
    }

    /// Call a native function with the given arguments, the result is computed natively in Rust and returned to the user on the stack.
    /// 
    /// **Arguments:**
    /// - `function` - The [`NativeFn`] to call.
    /// - `arg_count` - The number of arguments povided by the user.
    fn call_native(&mut self, function: NativeFn, arg_count: u8) {
        let res = function(arg_count, &self.stack[(self.stack.len() - arg_count as usize - 1)..self.stack.len()]);

        for _ in 0..arg_count + 1 {
            self.stack.pop();
        }

        self.stack.push(res);
    }

    #[cfg(debug_assertions)]
    /// Print debug information about the current VM state.
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

        // Debug the current instruction
        let frame = self.frames.last().unwrap();
        frame.debug_info(self.operation_counter);
    }
}