use std::ptr::NonNull;

use super::{chunk::Chunk, value::Value};

pub trait IsObj {
    fn as_obj_ptr(&self) -> NonNull<Obj> {
        NonNull::from(self).cast()
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ObjType {
    String,
    Function,
    Native,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Obj {
    pub kind: ObjType,
    pub next: Option<NonNull<Obj>>,
}

impl Obj {
    fn string() -> Self {
        Self {
            kind: ObjType::String,
            next: None,
        }
    }

    fn function() -> Self {
        Self {
            kind: ObjType::Function,
            next: None,
        }
    }

    pub fn set_next(&mut self, next: Option<NonNull<Self>>) {
        self.next = next;
    }
}

/// The base type for Lox Strings
/// Inlining length and turning it into a dynamically sized type could increase performance
/// TODO: Figure out how to format raw strs/byte arrays
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjString {
    obj: Obj,
    // This is an extra word in heap memory vs the book's representation,
    // but it comes with ergonomics (and potential optimization later)
    string: String,
}

impl Eq for ObjString {}

impl std::hash::Hash for ObjString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
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

impl std::ops::Deref for ObjString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl std::fmt::Display for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Script,
    Function,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjFunction {
    pub obj: Obj,
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Option<String>,
}

impl std::fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<fn>"),
        }
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.name == other.name
    }
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            obj: Obj {
                kind: ObjType::Function,
                next: None,
            },
            arity: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }
}

#[repr(C)]
pub struct ObjNative {
    obj: Obj,
    arity: usize,
    function: fn(Option<&[Value]>) -> Value,
}

impl ObjNative {
    pub fn new(arity: usize, function: fn(Option<&[Value]>) -> Value) -> Self {
        Self {
            obj: Obj {
                kind: ObjType::Native,
                next: None,
            },
            arity,
            function,
        }
    }

    pub fn function(&self) -> fn(Option<&[Value]>) -> Value {
        self.function
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}
