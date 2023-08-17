use std::{
    cell::Cell,
    hash::Hash,
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
    time::{SystemTime, UNIX_EPOCH},
};

use anyhow::{anyhow, Result};
use datasize::DataSize;

use super::{
    chunk::Chunk,
    memory::Heap,
    object::{
        Obj, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative, ObjString, ObjType,
        ObjUpvalue,
    },
};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Obj(NonNull<Obj>),
    Nil,
}

impl DataSize for Value {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        match self {
            Value::Obj(o) => unsafe {
                let o = o.as_ptr();
                match (*o).kind {
                    ObjType::Class => (*o.cast::<ObjClass>()).estimate_heap_size(),
                    ObjType::Closure => (*o.cast::<ObjClosure>()).estimate_heap_size(),
                    ObjType::Function => (*o.cast::<ObjFunction>()).estimate_heap_size(),
                    ObjType::Function => (*o.cast::<ObjFunction>()).estimate_heap_size(),
                    ObjType::String => (*o.cast::<ObjString>()).estimate_heap_size(),
                    ObjType::Native => (*o.cast::<ObjNative>()).estimate_heap_size(),
                    ObjType::Upvalue => (*o.cast::<ObjUpvalue>()).estimate_heap_size(),
                    ObjType::Class => (*o.cast::<ObjClass>()).estimate_heap_size(),
                    ObjType::Instance => (*o.cast::<ObjInstance>()).estimate_heap_size(),
                }
            },
            _ => 0,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Bool(b) => b.hash(state),
            Value::Number(n) => n.to_bits().hash(state),
            Value::Obj(o) => unsafe {
                (*o.cast::<ObjString>().as_ptr()).hash(state);
            },
            Value::Nil => (),
        }
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

    pub fn new_string(string: String) -> Self {
        let obj = ObjString::new(string);
        let ptr = NonNull::new(Box::into_raw(Box::new(obj)))
            .expect("just created a string")
            .cast();
        Self::Obj(ptr)
    }

    pub fn clock(args: Option<&[Cell<Self>]>) -> Self {
        Self::Number(SystemTime::elapsed(&UNIX_EPOCH).unwrap().as_millis() as f64 / 1000.0)
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
                    ObjType::String => write!(f, "{}", o.cast::<ObjString>().as_ref()),
                    ObjType::Function => write!(f, "{}", o.cast::<ObjFunction>().as_ref()),
                    ObjType::Native => write!(f, "<native fn>"),
                    // This will never actually print in user generated code,
                    // but for debugging purposes it can help to know we're looking at an upvalue.
                    ObjType::Upvalue => write!(f, "upvalue"),
                    ObjType::Closure => write!(
                        f,
                        "{}",
                        (*o.cast::<ObjClosure>().as_ptr()).function.as_ref()
                    ),
                    ObjType::Class => {
                        write!(f, "{}", (*o.cast::<ObjClass>().as_ptr()).name.as_ref())
                    }
                    ObjType::Instance => write!(
                        f,
                        "{} instance",
                        Value::Obj((*o.cast::<ObjInstance>().as_ptr()).class.cast())
                    ),
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
                (Value::Obj(o), _) | (_, Value::Obj(o))
                    if (*o.as_ptr()).kind == ObjType::String =>
                {
                    // this memory is leaked atm, either need a custom allocator or a hack.
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
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::Obj(l), Self::Obj(r)) => unsafe {
                (*l.as_ptr()).kind == (*r.as_ptr()).kind
                    && match (*l.as_ptr()).kind {
                        ObjType::String => {
                            l.cast::<ObjString>().as_ref() == r.cast::<ObjString>().as_ref()
                        }
                        ObjType::Function => {
                            l.cast::<ObjFunction>().as_ref() == r.cast::<ObjFunction>().as_ref()
                        }
                        // Could make this more robust
                        _ => false,
                    }
            },
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

impl PartialEq<str> for Value {
    fn eq(&self, other: &str) -> bool {
        match self {
            Self::Obj(o) => unsafe { o.cast::<ObjString>().as_ref().deref() == other },
            _ => false,
        }
    }
}
