use std::{fmt::{Debug, Display}, ops::{Add, Div, Mul, Neg, Sub}};

use crate::chunk::Chunk;

pub type NativeFn = fn(arg_count: u8, args: &[Value]) -> Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String,
}

impl Function {
    pub fn new() -> Self {
        Function { arity: 0, chunk: Chunk::new(), name: String::new() }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn({}) {}>", self.arity, self.name)
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    String(String),
    Function(Function),
    NativeFunction(NativeFn),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Function(fun) => write!(f, "{}", fun),
            Self::NativeFunction(_) => write!(f, "<native fn>")
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Obj(Object),
    #[default]
    Nil,
}

impl Value {
    pub fn get_string(&self) -> Result<String, &'static str> {
        match self {
            Self::Obj(Object::String(s)) => Ok(s.clone()),
            Self::Obj(Object::Function(f)) => Ok(f.name.clone()),
            _ => Err("Value must be a string"),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, &'static str>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Double(d) => Ok(Value::Double(-d)),
            _ => Err("Operand must be a number"),
        }
    }
}

impl Add for Value {
    type Output = Result<Self, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Double(dl), Self::Double(dr)) => Ok(Value::Double(dl + dr)),
            (Self::Obj(Object::String(sl)), Self::Obj(Object::String(sr))) => Ok(Value::Obj(Object::String(sl.clone() + &sr))),
            _ => Err("Operands must both be numbers or strings"),
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Double(dl), Self::Double(dr)) => Ok(Value::Double(dl - dr)),
            _ => Err("Operands must be numbers"),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Double(dl), Self::Double(dr)) => Ok(Value::Double(dl * dr)),
            _ => Err("Operands must be numbers"),
        }    
    }
}

impl Div for Value {
    type Output = Result<Self, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Double(dl), Self::Double(dr)) => Ok(Value::Double(dl / dr)),
            _ => Err("Operands must be numbers"),
        }
    }
}



impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Double(d) => write!(f, "{}", d),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Obj(o) => write!(f, "{}", o),
            Self::Nil => write!(f, "NIL"),
        }
    }
}