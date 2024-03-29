use std::{
    cell::Cell,
    hash::Hash,
    ops::Deref,
    ptr::NonNull,
    time::{SystemTime, UNIX_EPOCH},
};

use anyhow::Result;
use prehash::Prehashed;

use super::{
    object::{
        Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjString, ObjType,
    },
    vm::RuntimeError,
};

#[cfg(not(feature = "nan-boxing"))]
#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Obj(NonNull<Obj>),
    Nil,
}

#[cfg(feature = "nan-boxing")]
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Value(u64);

#[cfg(feature = "nan-boxing")]
impl Value {
    #[allow(non_upper_case_globals)]
    pub const Nil: Self = Self(QNAN | NIL_TAG);
    pub const FALSE: Self = Self(QNAN | FALSE_TAG);
    pub const TRUE: Self = Self(QNAN | TRUE_TAG);
}

/// NAN bits for IEEE 754 + quiet bit + Intel FP Indefinate value
/// Used along with other tag values to denote the non-numeric Value types
const QNAN: u64 = 0b0111111111111100000000000000000000000000000000000000000000000000;
/// Sign bit is used to deno
const SIGN_BIT: u64 = 0b1000000000000000000000000000000000000000000000000000000000000000;
const OBJ_TAG: u64 = SIGN_BIT | QNAN;
const NIL_TAG: u64 = 0b01;
const FALSE_TAG: u64 = 0b10;
const TRUE_TAG: u64 = 0b11;

#[cfg(feature = "nan-boxing")]
impl From<Value> for u64 {
    fn from(value: Value) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueType {
    Number,
    Bool,
    Obj,
    Nil,
}

impl From<Value> for ValueType {
    fn from(value: Value) -> Self {
        if value.as_num().is_some() {
            ValueType::Number
        } else if value.as_bool().is_some() {
            ValueType::Bool
        } else if value.as_obj().is_some() {
            ValueType::Obj
        } else {
            ValueType::Nil
        }
    }
}

impl Value {
    /// Returns an `f64` if the Value is of type `Number`, otherwise returns `None`.
    /// Serves as a convenient access for the contained value with a validity check for safety.
    #[inline]
    pub fn as_num(self) -> Option<f64> {
        #[cfg(feature = "nan-boxing")]
        match self.0 & QNAN != QNAN {
            true => Some(f64::from_bits(self.0)),
            false => None,
        }

        #[cfg(not(feature = "nan-boxing"))]
        match self {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }

    /// Returns a `bool` if the Value is of type `Bool`, otherwise returns `None`.
    /// Serves as a convenient access for the contained value with a validity check for safety.
    #[inline]
    pub fn as_bool(self) -> Option<bool> {
        #[cfg(feature = "nan-boxing")]
        match self & Self::FALSE == Self::FALSE {
            true => Some(self == Self::TRUE),
            false => None,
        }
        #[cfg(not(feature = "nan-boxing"))]
        match self {
            Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    /// Returns a `Nonnull<Obj>` if the Value is of type `Obj`, otherwise returns `None`.
    /// Serves as a convenient access for the contained value with a validity check for safety.
    #[inline]
    pub fn as_obj(self) -> Option<NonNull<Obj>> {
        #[cfg(feature = "nan-boxing")]
        match self.0 & OBJ_TAG == OBJ_TAG {
            true => Some(NonNull::new((self.0 & !OBJ_TAG) as *mut Obj).unwrap()),
            false => None,
        }
        #[cfg(not(feature = "nan-boxing"))]
        match self {
            Value::Obj(o) => Some(o),
            _ => None,
        }
    }

    /// Constructs a Prehashed from a `Value` and its included hash.
    /// If the `Value` is not of type `ObjString`, this will return `None`.
    #[inline]
    pub fn hashed(&self) -> Option<Prehashed<Value, u64>> {
        unsafe {
            self.as_obj().and_then(|obj| match (*obj.as_ptr()).kind {
                ObjType::String => {
                    let hash = obj.cast::<ObjString>().as_ref().hash;
                    Some(Prehashed::new(*self, hash))
                }
                _ => None,
            })
        }
    }
}

// Basically just making concrete that `Nil` is equivalent to `Option<Value>::None`
impl<T> From<Option<T>> for Value
where
    T: Into<Value>,
{
    fn from(value: Option<T>) -> Self {
        value.map(|v| v.into()).unwrap_or(Self::Nil)
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match (self.as_bool(), self.as_num(), self.as_obj()) {
            (Some(b), _, _) => b.hash(state),
            (_, Some(n), _) => n.to_bits().hash(state),
            (_, _, Some(o)) => unsafe {
                (*o.cast::<ObjString>().as_ptr()).hash(state);
            },
            _ => (),
        }
    }
}

impl Value {
    /// In Lox, False and Nil are falsey, and all other values (even 0) are truthy.
    pub fn is_truthy(self) -> bool {
        self.as_bool().unwrap_or(self != Self::Nil)
    }

    /// Returns the seconds since January 1, 1970 as a 64 bit floating point integer
    /// This is the only native function defined in the Lox standard
    pub fn clock(_: Option<&[Cell<Self>]>) -> Self {
        Self::from(SystemTime::elapsed(&UNIX_EPOCH).unwrap().as_secs_f64())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //#[cfg(not(feature = "nan-boxing"))]
        match (self.as_bool(), self.as_num(), self.as_obj()) {
            (Some(b), _, _) => write!(f, "{b}"),
            (_, Some(n), _) => write!(f, "{n}"),
            // SAFETY: The ObjType enum's entire usage is to validate these pointer casts.
            // the only Obj with ObjType::String will be part of an ObjString
            (_, _, Some(o)) => unsafe {
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
                        Value::from((*o.cast::<ObjInstance>().as_ptr()).class.cast())
                    ),
                    ObjType::BoundMethod => write!(
                        f,
                        "{}",
                        *(*o.cast::<ObjBoundMethod>().as_ptr()).method.as_ptr()
                    ),
                }
            },
            _ => write!(f, "nil"),
        }
    }
}

// impl std::ops::Add for Value {
//     type Output = Result<Self>;

//     fn add(self, rhs: Self) -> Self::Output {
//         unsafe {
//             match (self, rhs) {
//                 (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
//                 (Value::Obj(o), _) | (_, Value::Obj(o))
//                     if (*o.as_ptr()).kind == ObjType::String =>
//                 {
//                     // this memory is leaked atm, either need a custom allocator or a hack.
//                     let new_string = ObjString::new(format!("{self}{rhs}"));
//                     let ptr = NonNull::new(Box::into_raw(Box::new(new_string)))
//                         .unwrap()
//                         .cast();
//                     Ok(Value::Obj(ptr))
//                 }
//                 _ => Err(anyhow!("cannot add {self} and {rhs}")),
//             }
//         }
//     }
// }

impl std::ops::Sub for Value {
    type Output = Result<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self.as_num(), rhs.as_num()) {
            (Some(x), Some(y)) => Ok(Value::from(x - y)),
            _ => Err(
                RuntimeError::BinaryOpError("subtract", self.to_string(), rhs.to_string()).into(),
            ),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Result<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.as_num(), rhs.as_num()) {
            (Some(x), Some(y)) => Ok(Value::from(x / y)),
            _ => {
                Err(RuntimeError::BinaryOpError("divide", self.to_string(), rhs.to_string()).into())
            }
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.as_num(), rhs.as_num()) {
            (Some(x), Some(y)) => Ok(Value::from(x * y)),
            _ => Err(
                RuntimeError::BinaryOpError("multiply", self.to_string(), rhs.to_string()).into(),
            ),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Result<Self>;

    fn neg(self) -> Self::Output {
        match self.as_num() {
            Some(n) => Ok(Self::from(-n)),
            _ => Err(RuntimeError::NegationError(self.to_string()).into()),
        }
    }
}

impl std::ops::Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::from(!self.is_truthy())
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if let (Some(l), Some(r)) = (self.as_obj(), other.as_obj()) {
            unsafe {
                (*l.as_ptr()).kind == (*r.as_ptr()).kind
                    && match (*l.as_ptr()).kind {
                        ObjType::String => {
                            l.cast::<ObjString>().as_ref() == r.cast::<ObjString>().as_ref()
                        }
                        // Could make this more robust
                        _ => false,
                    }
            }
        } else {
            #[cfg(not(feature = "nan-boxing"))]
            match (self, other) {
                (Self::Bool(l), Self::Bool(r)) => l == r,
                (Self::Number(l), Self::Number(r)) => l == r,
                (Self::Nil, Self::Nil) => true,
                _ => false,
            }
            #[cfg(feature = "nan-boxing")]
            match (*self & QNAN, *other & QNAN) {
                (Self(QNAN), Self(QNAN)) => self.0 == other.0,
                _ => f64::from_bits(self.0) == f64::from_bits(other.0),
            }
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.as_num(), other.as_num()) {
            (Some(x), Some(y)) => x.partial_cmp(&y),
            _ => None,
        }
    }
}

impl PartialEq<str> for Value {
    fn eq(&self, other: &str) -> bool {
        self.as_obj()
            .map(|o| unsafe {
                (*o.as_ptr()).kind == ObjType::String
                    && o.cast::<ObjString>().as_ref().deref() == other
            })
            .unwrap_or(false)
    }
}

#[cfg(feature = "nan-boxing")]
mod bitwise_ops {
    use super::Value;
    use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

    macro_rules! impl_op {
        ($name:ident<$t:ident>, $fun:tt, $op:tt) => {
            impl<$t> $name<$t> for Value
            where $t: Into<u64> {
                type Output = Self;

                fn $fun(self, rhs: $t) -> Self::Output {
                    Self(self.0 $op rhs.into())
                }
            }
        };

        ($name:tt, $fun:tt, $op:tt) => {
            impl $name for Value {
                fn $fun(&mut self, rhs: Self) {
                    self.0 $op rhs.0;
                }
            }
        };
    }

    impl_op!(BitAnd<T>, bitand, &);
    impl_op!(BitOr<T>, bitor, |);
    impl_op!(BitOrAssign, bitor_assign, |=);
    impl_op!(BitAndAssign, bitand_assign, &=);
}

macro_rules! impl_from {
    ($ty:ty, $var:tt, $value:ident, $expr:expr$(,)?) => {
        impl From<$ty> for Value {
            #[cfg(feature = "nan-boxing")]
            fn from($value: $ty) -> Self {
                $expr
            }

            #[cfg(not(feature = "nan-boxing"))]
            fn from(value: $ty) -> Self {
                Self::$var(value)
            }
        }
    };
}

impl_from!(bool, Bool, value, Value::FALSE | value);
impl_from!(f64, Number, value, Self(value.to_bits()));
impl_from!(
    NonNull<Obj>,
    Obj,
    value,
    Self(SIGN_BIT | QNAN | value.as_ptr() as u64),
);

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_type() {
        let obj = Value::from(
            NonNull::new(Box::into_raw(Box::new(ObjString::new(String::new()))))
                .unwrap()
                .cast(),
        );
        let bool = Value::from(true);
        let num = Value::from(5.0);
        assert!(obj.as_obj().is_some());
        assert!(bool.as_bool().unwrap());
        assert!(num.as_num().is_some());
        assert_eq!(ValueType::from(obj), ValueType::Obj);
        assert_eq!(ValueType::from(bool), ValueType::Bool);
        assert_eq!(ValueType::from(num), ValueType::Number);
        assert_eq!(ValueType::from(Value::Nil), ValueType::Nil);
        unsafe {
            drop(Box::from_raw(
                obj.as_obj().unwrap().cast::<ObjString>().as_ptr(),
            ));
        }
    }
}
