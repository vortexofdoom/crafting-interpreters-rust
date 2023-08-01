use std::convert::Infallible;

use anyhow::{anyhow, Result};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Result<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
            _ => Err(anyhow!("cannot add {self} and {rhs}")),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Result<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
            _ => Err(anyhow!("cannot subtract {self} and {rhs}")),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Result<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
            _ => Err(anyhow!("cannot subtract {self} and {rhs}")),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
            _ => Err(anyhow!("cannot subtract {self} and {rhs}")),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Result<Self>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Number(n) => Ok(Self::Number(-n)),
            _ => Err(anyhow!("cannot negate {self}")),
        }
    }
}

impl std::ops::Not for Value {
    type Output = Result<Self>;

    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Ok(Self::Bool(!b)),
            _ => Err(anyhow!("cannot negate {self}")),
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(x), Self::Number(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}
