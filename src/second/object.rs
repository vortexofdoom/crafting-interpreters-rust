use std::{cell::Cell, ptr::NonNull};

use fnv::FnvHasher;
use prehash::{new_prehashed_map, Prehashed, PrehashedMap};

use super::{chunk::Chunk, value::Value};

pub trait IsObj: Sized {
    fn kind(&self) -> ObjType;

    fn as_obj_ptr(&self) -> NonNull<Obj> {
        NonNull::from(self).cast()
    }

    fn obj(&mut self) -> &mut Obj;
}

macro_rules! impl_IsObj {
    ($name:tt, $struct:ty, $fun:tt) => {
        impl IsObj for $struct {
            fn kind(&self) -> ObjType {
                ObjType::$name
            }

            fn obj(&mut self) -> &mut Obj {
                &mut self.obj
            }
        }

        impl Obj {
            fn $fun() -> Self {
                Self {
                    kind: ObjType::$name,
                    is_marked: false,
                    next: None,
                }
            }
        }
    };
}

impl_IsObj!(Closure, ObjClosure, closure);
impl_IsObj!(Function, ObjFunction, function);
impl_IsObj!(String, ObjString, string);
impl_IsObj!(Native, ObjNative, native);
impl_IsObj!(Upvalue, ObjUpvalue, upvalue);
impl_IsObj!(Class, ObjClass, class);
impl_IsObj!(Instance, ObjInstance, instance);
impl_IsObj!(BoundMethod, ObjBoundMethod, bound_method);

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ObjType {
    BoundMethod,
    Class,
    Closure,
    Function,
    Instance,
    Native,
    String,
    Upvalue,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Obj {
    pub kind: ObjType,
    pub is_marked: bool,
    pub next: Option<NonNull<Obj>>,
}

/// The base type for Lox Strings
/// Inlining length and turning it into a dynamically sized type could increase performance
/// TODO: Figure out how to format raw strs/byte arrays
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjString {
    pub obj: Obj,
    // This is an extra word in heap memory vs the book's representation,
    // but it comes with ergonomics (and potential optimization later)
    pub hash: u64,
    string: String,
}

impl Eq for ObjString {}

impl std::hash::Hash for ObjString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl ObjString {
    #[inline]
    pub fn new(string: String) -> Self {
        Self {
            obj: Obj::string(),
            hash: *Prehashed::as_hash(&Prehashed::with_hasher::<FnvHasher>(&string)),
            string,
        }
    }
}

impl AsRef<str> for ObjString {
    fn as_ref(&self) -> &str {
        &self.string
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
    Method,
    Initializer,
}

#[repr(C)]
#[derive(Debug)]
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
    pub fn new(name: Option<String>) -> Self {
        Self {
            obj: Obj::function(),
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjNative {
    pub obj: Obj,
    arity: usize,
    function: fn(Option<&[Cell<Value>]>) -> Value,
}

impl ObjNative {
    #[inline]
    pub fn new(arity: usize, function: fn(Option<&[Cell<Value>]>) -> Value) -> Self {
        Self {
            obj: Obj::native(),
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
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjClosure {
    pub obj: Obj,
    pub function: NonNull<ObjFunction>,
    pub upvalues: Vec<NonNull<ObjUpvalue>>,
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
                upvalues: Vec::with_capacity((*function.as_ptr()).upvalue_count as usize),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(Cell<Value>),
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjUpvalue {
    pub obj: Obj,
    pub value: Upvalue,
    pub next: Option<NonNull<Self>>,
}

impl ObjUpvalue {
    pub fn new(value: usize) -> Self {
        Self {
            obj: Obj::upvalue(),
            value: Upvalue::Open(value),
            next: None,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjClass {
    pub obj: Obj,
    pub name: NonNull<ObjString>,
    pub methods: PrehashedMap<Value, Value>,
}

impl ObjClass {
    pub fn new(name: NonNull<ObjString>) -> Self {
        Self {
            obj: Obj::class(),
            name,
            methods: new_prehashed_map(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjInstance {
    pub obj: Obj,
    pub class: NonNull<ObjClass>,
    pub fields: PrehashedMap<Value, Value>,
}

impl ObjInstance {
    pub fn new(class: NonNull<ObjClass>) -> Self {
        Self {
            obj: Obj::instance(),
            class,
            fields: new_prehashed_map(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjBoundMethod {
    pub obj: Obj,
    pub receiver: Value,
    pub method: NonNull<ObjClosure>,
}

impl ObjBoundMethod {
    pub fn new(receiver: Value, method: NonNull<ObjClosure>) -> Self {
        Self {
            obj: Obj::bound_method(),
            receiver,
            method,
        }
    }
}
