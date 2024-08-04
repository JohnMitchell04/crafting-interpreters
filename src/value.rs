use std::{fmt::{Debug, Display}, ops::{Add, Div, Mul, Neg, Sub}};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    String(String)
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Obj(Object),
    Nil,
}

impl Default for Value {
    fn default() -> Self {
        Value::Nil
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
            Self::Double(d) => write!(f, "Type: DOUBLE, Value: {}", d),
            Self::Bool(b) => write!(f, "Type: BOOL, Value: {}", b),
            Self::Obj(o) => write!(f, "Type: OBJECT, Value: {}", o),
            Self::Nil => write!(f, "Type: NIL"),
        }
    }
}