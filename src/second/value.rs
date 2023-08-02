use std::ptr::NonNull;

use anyhow::{anyhow, Result};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Obj(NonNull<Obj>),
    Nil,
}

impl Value {
    pub fn obj_type(&self) -> Option<&Obj> {
        match self {
            // Safety: ObjType will always be non-null, since it is derived ONLY from *const Obj items
            Self::Obj(o) => unsafe { Some(o.as_ref()) },
            _ => None,
        }
    }

    pub fn new_string(string: String) -> Self {
        let obj = ObjString::new(string);
        let ptr = NonNull::new(Box::into_raw(Box::new(obj)))
            .expect("just created a string")
            .cast();
        Self::Obj(ptr)
    }
}

pub trait IsObj {
    fn as_type_ptr(&self) -> *const Obj {
        self as *const Self as *const Obj
    }

    fn ptr_from_type(ptr: NonNull<Obj>) -> NonNull<Self>
    where
        Self: Sized,
    {
        ptr.cast()
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ObjType {
    String,
    Function,
}

#[repr(C)]
pub struct Obj {
    kind: ObjType,
    next: Option<NonNull<Obj>>,
}

impl Obj {
    fn string() -> Self {
        Self {
            kind: ObjType::String,
            next: None,
        }
    }

    pub fn set_next(&mut self, next: Option<NonNull<Self>>) {
        self.next = next;
    }
}

#[repr(C)]
pub struct ObjString {
    obj: Obj,
    // This is an extra word in heap memory vs the book's representation,
    // but it comes with ergonomics (and potential optimization later)
    string: String,
}

impl IsObj for ObjString {}

impl ObjString {
    pub fn new(string: String) -> Self {
        Self {
            obj: Obj::string(),
            string,
        }
    }
}

impl std::fmt::Display for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
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
            Value::Obj(o) => unsafe {
                // SAFETY: The ObjType enum's entire usage is to validate these pointer casts.
                // a *const ObjType::String will only ever be generated from a *const ObjString
                match o.as_ref().kind {
                    ObjType::String => write!(f, "{}", ObjString::ptr_from_type(*o).as_ref()),
                    ObjType::Function => todo!(),
                    _ => unreachable!(),
                }
            },
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Result<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        unsafe {
            match (self, rhs) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                (Value::Obj(o), _) | (_, Value::Obj(o)) if o.as_ref().kind == ObjType::String => {
                    let new_string = ObjString::new(format!("{self}{rhs}"));
                    let ptr = NonNull::new(Box::into_raw(Box::new(new_string)))
                        .unwrap()
                        .cast();
                    Ok(Value::Obj(ptr))
                }
                _ => Err(anyhow!("cannot add {self} and {rhs}")),
            }
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
            _ => Err(anyhow!("cannot divide {self} and {rhs}")),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
            _ => Err(anyhow!("cannot multiply {self} and {rhs}")),
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
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Self::Bool(!b),
            _ => Self::Bool(!self.is_truthy()),
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
