use std::{cell::Cell, ptr::NonNull};

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
    Closure,
    Upvalue,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Obj {
    pub kind: ObjType,
    pub next: Option<NonNull<Obj>>,
}

impl Obj {
    #[inline]
    fn string() -> Self {
        Self {
            kind: ObjType::String,
            next: None,
        }
    }

    #[inline]
    fn function() -> Self {
        Self {
            kind: ObjType::Function,
            next: None,
        }
    }

    #[inline]
    fn closure() -> Self {
        Self {
            kind: ObjType::Closure,
            next: None,
        }
    }

    fn upvalue() -> Self {
        Self {
            kind: ObjType::Upvalue,
            next: None,
        }
    }

    #[inline]
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
    #[inline]
    pub fn new(string: String) -> Self {
        Self {
            obj: Obj::string(),
            string,
        }
    }

    #[inline]
    pub fn into_ptr(self) -> NonNull<Self> {
        NonNull::new(Box::into_raw(Box::new(self))).unwrap()
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
    pub upvalue_count: u8,
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
    #[inline]
    pub fn new() -> Self {
        Self {
            obj: Obj {
                kind: ObjType::Function,
                next: None,
            },
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    #[inline]
    pub fn into_ptr(self) -> NonNull<Self> {
        NonNull::new(Box::into_raw(Box::new(self))).unwrap()
    }
}

#[repr(C)]
pub struct ObjNative {
    obj: Obj,
    arity: usize,
    function: fn(Option<&[Cell<Value>]>) -> Value,
}

impl ObjNative {
    #[inline]
    pub fn new(arity: usize, function: fn(Option<&[Cell<Value>]>) -> Value) -> Self {
        Self {
            obj: Obj {
                kind: ObjType::Native,
                next: None,
            },
            arity,
            function,
        }
    }

    #[inline]
    pub fn function(&self) -> fn(Option<&[Cell<Value>]>) -> Value {
        self.function
    }

    #[inline]
    pub fn arity(&self) -> usize {
        self.arity
    }

    #[inline]
    pub fn into_ptr(self) -> NonNull<Self> {
        NonNull::new(Box::into_raw(Box::new(self))).unwrap()
    }
}

#[repr(C)]
pub struct ObjClosure {
    obj: Obj,
    pub function: NonNull<ObjFunction>,
    pub upvalues: Vec<*const ObjUpvalue>,
}

impl std::fmt::Display for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", self.function.as_ref()) }
    }
}

impl ObjClosure {
    #[inline]
    pub fn new(function: NonNull<ObjFunction>) -> Self {
        unsafe {
            Self {
                obj: Obj::closure(),
                function,
                upvalues: vec![std::ptr::null(); (*function.as_ptr()).upvalue_count as usize],
            }
        }
    }

    #[inline]
    pub fn function(&self) -> &ObjFunction {
        unsafe { self.function.as_ref() }
    }

    #[inline]
    pub fn into_ptr(self) -> NonNull<Self> {
        NonNull::new(Box::into_raw(Box::new(self))).unwrap()
    }
}

#[repr(C)]
pub struct ObjUpvalue {
    obj: Obj,
    pub location: *const Cell<Value>,
}

impl ObjUpvalue {
    pub fn new(value: &Cell<Value>) -> Self {
        Self {
            obj: Obj::upvalue(),
            location: value as *const Cell<Value>,
        }
    }

    pub fn set_value(&mut self, value: Value) {
        unsafe {
            (*self.location).set(value);
        }
    }

    #[inline]
    pub fn into_ptr(self) -> NonNull<Self> {
        NonNull::new(Box::into_raw(Box::new(self))).unwrap()
    }
}
