use std::{fmt::Display, ops::{Add, Div, Mul, Neg, Sub}};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Nil,
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
            _ => Err("Operands must be numbers"),
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
            Self::Nil => write!(f, "Type: NIL"),
        }
    }
}