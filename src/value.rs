use std::{fmt::Display, ops::{Add, Div, Mul, Neg, Sub}};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Double(f64),
}


// TODO: Clean these up and hadle errors better, these can likely be implemented with macros 
impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Double(d) => Value::Double(-d),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Double(dl) => {
                match rhs {
                    Self::Double(dr) => Value::Double(dl + dr)
                }
            }
        }    
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::Double(dl) => {
                match rhs {
                    Self::Double(dr) => Value::Double(dl - dr)
                }
            }
        }    
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Self::Double(dl) => {
                match rhs {
                    Self::Double(dr) => Value::Double(dl * dr)
                }
            }
        }    
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Self::Double(dl) => {
                match rhs {
                    Self::Double(dr) => Value::Double(dl / dr)
                }
            }
        }    
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Double(d) => write!(f, "{}", d)
        }
    }
}