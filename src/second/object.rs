use std::{cell::Cell, ptr::NonNull};

use datasize::DataSize;
use fnv::FnvHashMap;

use super::{chunk::Chunk, value::Value};

pub trait IsObj: DataSize + Sized {
    fn kind(&self) -> ObjType;

    fn as_obj_ptr(&self) -> NonNull<Obj> {
        NonNull::from(self).cast()
    }

    fn obj(&mut self) -> &mut Obj;

    fn size(&self) -> usize {
        std::mem::size_of::<Self>() + self.estimate_heap_size()
    }
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

impl_IsObj!(Closure, ObjClosure<'_>, closure);
impl_IsObj!(Function, ObjFunction<'_>, function);
impl_IsObj!(String, ObjString, string);
impl_IsObj!(Native, ObjNative, native);
impl_IsObj!(Upvalue, ObjUpvalue, upvalue);
impl_IsObj!(Class, ObjClass, class);
impl_IsObj!(Instance, ObjInstance, instance);
impl_IsObj!(BoundMethod, ObjBoundMethod<'_>, bound_method);

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

impl DataSize for Obj {
    const IS_DYNAMIC: bool = false;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        0
    }
}

impl Obj {
    #[inline]
    pub fn set_next(&mut self, next: Option<NonNull<Self>>) {
        self.next = next;
    }
}

/// The base type for Lox Strings
/// Inlining length and turning it into a dynamically sized type could increase performance
/// TODO: Figure out how to format raw strs/byte arrays
#[repr(C)]
#[derive(Debug, Clone, DataSize)]
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

impl ObjString {
    #[inline]
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
    Method,
    Initializer,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjFunction<'a> {
    pub obj: Obj,
    pub arity: u8,
    pub upvalue_count: u8,
    pub chunk: Chunk,
    pub name: Option<&'a str>,
}

impl<'a> DataSize for ObjFunction<'a> {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        self.chunk.estimate_heap_size()
    }
}

impl std::fmt::Display for ObjFunction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<fn>"),
        }
    }
}

impl PartialEq for ObjFunction<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.name == other.name
    }
}

impl<'a> ObjFunction<'a> {
    #[inline]
    pub fn new(name: Option<&'a str>) -> Self {
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
    obj: Obj,
    arity: usize,
    function: fn(Option<&[Cell<Value>]>) -> Value,
}

impl DataSize for ObjNative {
    const IS_DYNAMIC: bool = false;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        0
    }
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
pub struct ObjClosure<'a> {
    obj: Obj,
    pub function: NonNull<ObjFunction<'a>>,
    pub upvalues: Vec<NonNull<ObjUpvalue>>,
}

impl DataSize for ObjClosure<'_> {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        (&self.upvalues).estimate_heap_size()
    }
}

impl std::fmt::Display for ObjClosure<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", self.function.as_ref()) }
    }
}

impl<'a> ObjClosure<'a> {
    #[inline]
    pub fn new(function: NonNull<ObjFunction<'a>>) -> Self {
        unsafe {
            Self {
                obj: Obj::closure(),
                function,
                upvalues: Vec::with_capacity((*function.as_ptr()).upvalue_count as usize),
            }
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjUpvalue {
    obj: Obj,
    pub location: *const Cell<Value>,
    pub closed: Cell<Value>,
    pub next: Option<NonNull<Self>>,
}

impl DataSize for ObjUpvalue {
    fn estimate_heap_size(&self) -> usize {
        match self.closed.get() {
            Value::Obj(o) => unsafe { (*o.as_ptr()).estimate_heap_size() },
            _ => 0,
        }
    }

    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;
}

impl ObjUpvalue {
    pub fn new(value: *const Cell<Value>) -> Self {
        Self {
            obj: Obj::upvalue(),
            location: value,
            closed: Cell::new(Value::Nil),
            next: None,
        }
    }

    pub fn set_value(&mut self, value: Value) {
        unsafe {
            (*self.location).set(value);
        }
    }

    pub fn value(&self) -> Value {
        unsafe { (*self.location).get() }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjClass {
    pub obj: Obj,
    pub name: NonNull<ObjString>,
    pub methods: FnvHashMap<Value, Value>,
}

impl DataSize for ObjClass {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        unsafe { (*self.name.as_ptr()).estimate_heap_size() + self.methods.estimate_heap_size() }
    }
}

impl ObjClass {
    pub fn new(name: NonNull<ObjString>) -> Self {
        Self {
            obj: Obj::class(),
            name: name,
            methods: FnvHashMap::default(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjInstance {
    pub obj: Obj,
    pub class: NonNull<ObjClass>,
    pub fields: FnvHashMap<Value, Value>,
}

impl ObjInstance {
    pub fn new(class: NonNull<ObjClass>) -> Self {
        Self {
            obj: Obj::instance(),
            class,
            fields: FnvHashMap::default(),
        }
    }
}

impl DataSize for ObjInstance {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        self.fields.estimate_heap_size()
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjBoundMethod<'a> {
    pub obj: Obj,
    pub receiver: Value,
    pub method: NonNull<ObjClosure<'a>>,
}

impl DataSize for ObjBoundMethod<'_> {
    const IS_DYNAMIC: bool = false;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        0
    }
}

impl<'a> ObjBoundMethod<'a> {
    pub fn new(receiver: Value, method: NonNull<ObjClosure<'a>>) -> Self {
        Self {
            obj: Obj::bound_method(),
            receiver,
            method,
        }
    }
}
