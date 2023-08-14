pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod object;
pub mod scanner;
pub mod value;

use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{Read, Write},
    mem::MaybeUninit,
    path::Path,
    ptr::NonNull,
};

use anyhow::{anyhow, Result};
use chunk::{Chunk, OpCode};
use compiler::compile;

use self::{
    debug::disassemble_instruction,
    object::{Obj, ObjFunction, ObjNative, ObjString, ObjType},
    value::Value,
};

const STACK_MAX: usize = FRAMES_MAX * 256;
const FRAMES_MAX: usize = 64;

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    function: *const ObjFunction,
    ip: *const u8,
    starting_slot: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            function: std::ptr::null(),
            ip: std::ptr::null(),
            starting_slot: Default::default(),
        }
    }
}

impl CallFrame {
    #[inline]
    fn read_byte(&mut self) -> u8 {
        unsafe {
            let byte = *self.ip;
            self.ip = self.ip.add(1);
            byte
        }
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        unsafe {
            let i = self.read_byte() as usize;
            (*self.function).chunk.constants()[i]
        }
    }

    #[inline]
    fn op_from_byte(&mut self) -> Result<OpCode> {
        OpCode::try_from(self.read_byte()).map_err(|_| anyhow!(InterpretError::Runtime))
    }
}

#[derive(Debug)]
pub enum InterpretError {
    Compile,
    Runtime,
}

impl std::fmt::Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpretError::Compile => write!(f, "compilation error"),
            InterpretError::Runtime => write!(f, "runtime error"),
        }
    }
}

pub struct Vm {
    //chunk: Option<Chunk>,
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,
    stack: [Value; STACK_MAX],
    sp: usize,
    //sp: usize,
    //ip: usize,
    // With the rest of the architecture being what it is, clox's linked list object model was difficult,
    // since the only reliable time to append to the list was on every push, which means double free if not careful
    // Probably worth revisiting for performance, but maybe it's not even that bad, as iteration only has
    // a little more overhead, and the same amount of pointer indirection?
    objects: HashSet<*mut Obj>,
    globals: HashMap<Value, Value>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            frames: [CallFrame::default(); FRAMES_MAX],
            frame_count: 0,
            stack: [Value::Nil; STACK_MAX],
            sp: 0,
            objects: HashSet::new(),
            globals: HashMap::new(),
        };

        vm.define_native("clock", 0, Value::clock);
        vm
    }

    fn define_native(&mut self, name: &str, arity: usize, function: fn(Option<&[Value]>) -> Value) {
        let n = ObjString::new(String::from(name));
        let f = ObjNative::new(arity, function);
        let name = Box::into_raw(Box::new(n)).cast();
        let native = Box::into_raw(Box::new(f)).cast();
        self.objects.insert(name);
        self.objects.insert(native);
        let name = NonNull::new(name).unwrap();
        let native = NonNull::new(native).unwrap();
        self.globals.insert(Value::Obj(name), Value::Obj(native));
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let mut function = compile(source)?;
        self.push(Value::Obj(function.cast()));
        self.call(function, 0)?;
        self.run(true)?;
        // Might need to generalize this when it comes time to implement GC
        unsafe {
            let function = function.as_ptr();
            for constant in (*function).chunk.constants().iter() {
                if let Value::Obj(o) = constant {
                    self.objects.insert(o.as_ptr());
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn push(&mut self, value: Value) {
        // SAFETY: the stack pointer is defined in terms of the stack, and we check bounds at compile time
        //println!("pushing {value}, sp: {}", self.sp);
        self.stack[self.sp] = value;
        self.sp += 1;
        // Every time we push a value, we add it to the linked list
        if let Value::Obj(mut o) = value {
            self.objects.insert(o.as_ptr());
            // unsafe {
            //     println!("push curr: {:?}, new: {:?}", self.objects, o);
            //     (*o.as_ptr()).next = self.objects.take();
            //     self.objects = Some(o);
            // }
        }
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.sp -= 1;
        //println!("popping {}, sp: {}", self.stack[self.sp], self.sp);
        self.stack[self.sp]
    }

    #[inline]
    fn peek(&self, dist: usize) -> Value {
        self.stack[self.sp - 1 - dist]
    }

    pub fn free_objects(&mut self) {
        for obj in (&mut self.objects).iter() {
            unsafe {
                match (*(*obj)).kind {
                    ObjType::String => drop(Box::from_raw(obj.cast::<ObjString>())),
                    ObjType::Function => drop(Box::from_raw(obj.cast::<ObjFunction>())),
                    ObjType::Native => drop(Box::from_raw(obj.cast::<ObjNative>())),
                };
            }
        }
        self.objects.clear();
    }

    unsafe fn free_object(&mut self, obj: *mut Obj) {
        match (*obj).kind {
            ObjType::String => drop(Box::from_raw(obj.cast::<ObjString>())),
            ObjType::Function => drop(Box::from_raw(obj.cast::<ObjFunction>())),
            ObjType::Native => drop(Box::from_raw(obj.cast::<ObjNative>())),
        };
        self.objects.remove(&obj);
    }

    #[inline]
    fn reset_stack(&mut self) {
        self.stack.fill(Value::Nil);
        self.sp = 0;
    }

    pub fn run_file(&mut self, path: &str) -> Result<()> {
        let mut file = File::open(Path::new(path)).expect("valid files only");
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        if let Err(e) = self.interpret(&source) {
            println!("{e}");
        }
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        let mut input = String::new();
        loop {
            print!("> ");
            std::io::stdout().flush().expect("new prompt failed");
            let start = input.len();
            if std::io::stdin().read_line(&mut input).is_ok() {
                if matches!(&input[start..], "\r\n" | "\n") {
                    break;
                } else {
                    if let Err(e) = self.interpret(&input[start..]) {
                        println!("{e}");
                    }
                }
            }
        }
        Ok(())
    }

    pub fn call(&mut self, function: NonNull<ObjFunction>, arg_count: u8) -> Result<()> {
        unsafe {
            if (*function.as_ptr()).arity != arg_count {
                return Err(anyhow!(
                    "Expected {} arguments, but found {}.",
                    (*function.as_ptr()).arity,
                    arg_count
                ));
            }

            if self.frame_count == FRAMES_MAX {
                // TODO: print stack trace
                return Err(anyhow!("Stack overflow."));
            }

            let frame = CallFrame {
                function: function.as_ptr() as *const ObjFunction,
                ip: (*function.as_ptr()).chunk.code().as_ptr(),
                starting_slot: (self.sp - arg_count as usize).saturating_sub(1),
            };

            self.frames[self.frame_count] = frame;
            self.frame_count += 1;
        }
        Ok(())
    }

    fn call_value(&mut self, value: Value, arg_count: u8) -> Result<()> {
        match value {
            Value::Obj(o) => unsafe {
                match o.as_ref().kind {
                    ObjType::Function => return self.call(o.cast(), arg_count),
                    ObjType::Native => {
                        let native = o.cast::<ObjNative>().as_ref();
                        let arity = native.arity();
                        if arity != arg_count as usize {
                            return Err(anyhow!(
                                "expected {arity} arguments, but found {arg_count}."
                            ));
                        }
                        let args = match arity {
                            0 => None,
                            _ => Some(&self.stack[self.sp - arity..self.sp]),
                        };
                        let result = native.function()(args);
                        self.sp -= arg_count as usize + 1;
                        self.push(result);
                        return Ok(());
                    }
                    _ => {}
                }
            },
            _ => {}
        }
        Err(anyhow!("can only call functions and classes."))
    }

    pub fn run(&mut self, debug_trace: bool) -> Result<()> {
        unsafe {
            let mut frame = self.frames[self.frame_count - 1];

            macro_rules! read_byte {
                () => {
                    unsafe {
                        let byte = *frame.ip;
                        frame.ip = frame.ip.add(1);
                        byte
                    }
                };
            }

            macro_rules! read_constant {
                () => {{
                    let i = read_byte!() as usize;
                    (*frame.function).chunk.constants()[i]
                }};
            }

            macro_rules! bin_op {
                ($op:tt) => {{
                    let r = self.pop();
                    let l = self.pop();
                    self.push((l $op r)?);
                }};
            }

            macro_rules! compare {
                ($op:tt) => {{
                    let r = self.pop();
                    let l = self.pop();
                    self.push(Value::Bool(l $op r));
                }};
            }

            macro_rules! read_i16 {
                () => {{
                    let hi = read_byte!();
                    let lo = read_byte!();
                    u16::from_le_bytes([lo, hi])
                }};
            }

            loop {
                if debug_trace {
                    let chunk = &(*frame.function).chunk;
                    let code = chunk.code();
                    let offset = frame.ip.offset_from(code.as_ptr()) as usize;
                    if offset >= code.len() {
                        break;
                    }

                    disassemble_instruction(chunk, offset);
                }

                match frame.op_from_byte()? {
                    OpCode::Constant => {
                        let constant = read_constant!();
                        self.push(constant);
                    }
                    OpCode::Nil => self.push(Value::Nil),
                    OpCode::True => self.push(Value::Bool(true)),
                    OpCode::False => self.push(Value::Bool(false)),
                    OpCode::Pop => _ = self.pop(),
                    OpCode::GetLocal => {
                        let slot = frame.read_byte();
                        self.push(self.stack[frame.starting_slot + slot as usize]);
                    }
                    OpCode::GetGlobal => {
                        let name = read_constant!();
                        if let Some(value) = self.globals.get(&name) {
                            self.push(*value);
                        } else {
                            return Err(anyhow!(InterpretError::Runtime));
                        }
                    }
                    OpCode::DefineGlobal => {
                        let name = read_constant!();
                        let value = self.peek(0);
                        self.globals.insert(name, value);
                        self.pop();
                    }
                    OpCode::SetLocal => {
                        let slot = read_byte!();
                        self.stack[frame.starting_slot + slot as usize] = self.peek(0);
                    }
                    OpCode::SetGlobal => {
                        let name = read_constant!();
                        let value = self.peek(0);
                        if self.globals.insert(name, value).is_none() {
                            self.globals.remove(&name);
                            return Err(anyhow!(InterpretError::Runtime));
                        }
                    }
                    OpCode::Equal => compare!(==),
                    OpCode::Greater => compare!(>),
                    OpCode::GreaterEqual => compare!(>=),
                    OpCode::Less => compare!(<),
                    OpCode::LessEqual => compare!(<=),
                    OpCode::Add => bin_op!(+),
                    OpCode::Subtract => bin_op!(-),
                    OpCode::Multiply => bin_op!(*),
                    OpCode::Divide => bin_op!(/),
                    OpCode::Not => {
                        let value = !self.pop();
                        self.push(value);
                    }
                    OpCode::Negate => {
                        let value = (-self.pop())?;
                        self.push(value);
                    }
                    OpCode::Print => {
                        println!("{}", self.pop());
                    }
                    OpCode::Jump => {
                        let jump = read_i16!() as usize;
                        frame.ip = frame.ip.add(jump)
                    }
                    OpCode::JumpIfFalse => {
                        let jump = read_i16!() as usize;
                        if !self.peek(0).is_truthy() {
                            frame.ip = frame.ip.add(jump);
                        }
                    }
                    OpCode::Loop => {
                        let jump = read_i16!() as usize;
                        frame.ip = frame.ip.sub(jump)
                    }
                    OpCode::Call => {
                        let arg_count = read_byte!();
                        self.frames[self.frame_count - 1] = frame;
                        self.call_value(self.peek(arg_count as usize), arg_count)?;
                        frame = self.frames[self.frame_count - 1];
                    }
                    OpCode::Return => {
                        let result = self.pop();
                        self.frame_count -= 1;
                        if self.frame_count == 0 {
                            self.pop();
                            break;
                        }
                        self.sp = frame.starting_slot;
                        self.push(result);
                        frame = self.frames[self.frame_count - 1];
                    }
                }
            }
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        collections::{hash_map::DefaultHasher, HashSet},
        hash::Hasher,
    };
    #[test]
    fn test_functions() -> Result<()> {
        let mut vm = Vm::new();
        let source = r#"
        for (var i = 0; i < 5; i = i + 1) {
            print i;
        }"#;
        vm.interpret(source)?;
        vm.free_objects();
        Ok(())
    }

    #[test]
    fn test_hash() {
        let val1 = Value::new_string(String::from("hello"));
        let val2 = Value::new_string(String::from("hello"));
        assert_eq!(val1, val2);
        // let val1 = String::from("hello");
        // let val2 = String::from("hello");
        println!("{}, {}", calc_hash(&val1), calc_hash(&val2));
        let mut set = HashSet::new();
        set.insert(val1);
        assert!(set.get(&val2).is_some());
        // Making miri happy
        let Value::Obj(s1) = val1 else { unreachable!() };
        let Value::Obj(s2) = val2 else { unreachable!() };
        unsafe {
            Box::from_raw(s1.cast::<ObjString>().as_ptr());
            Box::from_raw(s2.cast::<ObjString>().as_ptr());
        }
    }

    fn calc_hash<T: std::hash::Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}
