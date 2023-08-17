pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod memory;
pub mod object;
pub mod scanner;
pub mod value;

use std::{
    cell::Cell,
    collections::{hash_map::RandomState, HashMap, HashSet},
    ffi::OsStr,
    fs::File,
    hash::BuildHasherDefault,
    io::{Read, Write},
    mem::MaybeUninit,
    path::{Path, PathBuf},
    ptr::NonNull,
    sync::atomic::Ordering::Relaxed,
};

use anyhow::{anyhow, Result};
use chunk::{Chunk, OpCode};
use compiler::compile;
use fnv::FnvHashMap;

use crate::second::memory::{ALLOCATED, NEXT_GC};

use self::{
    debug::disassemble_instruction,
    memory::Heap,
    object::{
        IsObj, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative,
        ObjString, ObjType, ObjUpvalue,
    },
    value::Value,
};

const STACK_MAX: usize = FRAMES_MAX * 256;
const FRAMES_MAX: usize = 64;

#[derive(Debug, Clone, Copy)]
struct CallFrame<'a> {
    closure: *const ObjClosure<'a>,
    ip: *const u8,
    starting_slot: usize,
}

impl Default for CallFrame<'_> {
    fn default() -> Self {
        Self {
            closure: std::ptr::null(),
            ip: std::ptr::null(),
            starting_slot: Default::default(),
        }
    }
}

impl CallFrame<'_> {
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
            (*(*self.closure).function.as_ptr()).chunk.constants()[i]
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

pub struct Vm<'a> {
    //chunk: Option<Chunk>,
    frames: [CallFrame<'a>; FRAMES_MAX],
    frame_count: usize,
    stack: [Cell<Value>; STACK_MAX],
    sp: usize,
    //sp: usize,
    //ip: usize,
    // With the rest of the architecture being what it is, clox's linked list object model was difficult,
    // since the only reliable time to append to the list was on every push, which means double free if not careful
    // Probably worth revisiting for performance, but maybe it's not even that bad, as iteration only has
    // a little more overhead, and the same amount of pointer indirection?
    heap: Heap,
    globals: FnvHashMap<Value, Value>,
    open_upvalues: Option<NonNull<ObjUpvalue>>,
    init_string: Value,
}

impl<'a> Vm<'a> {
    const DEFAULT_VAL: Cell<Value> = Cell::new(Value::Nil);
    pub fn new() -> Self {
        let frames = [CallFrame::default(); FRAMES_MAX];
        let mut vm = Self {
            frames: [CallFrame::default(); FRAMES_MAX],
            frame_count: 0,
            stack: [Self::DEFAULT_VAL; STACK_MAX],
            sp: 0,
            heap: Heap::new(),
            globals: FnvHashMap::default(),
            open_upvalues: None,
            init_string: Value::Nil,
        };

        let obj = vm.heap.new_obj(ObjString::new(String::from("init")));
        vm.init_string = Value::Obj(obj);

        vm.define_native("clock", 0, Value::clock);
        vm
    }

    fn new_obj<T: IsObj>(&mut self, obj: T) -> NonNull<Obj> {
        if ALLOCATED.load(Relaxed) >= NEXT_GC.load(Relaxed) {
            self.collect_garbage();
        }
        self.heap.new_obj(obj)
    }

    fn define_native(
        &mut self,
        name: &str,
        arity: usize,
        function: fn(Option<&[Cell<Value>]>) -> Value,
    ) {
        let name = self.new_obj(ObjString::new(String::from(name)));
        let native = self.new_obj(ObjNative::new(arity, function));
        self.globals.insert(Value::Obj(name), Value::Obj(native));
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let function = compile(source, &mut self.heap)?;
        self.push(Value::Obj(function.cast()));
        self.pop();
        let mut closure = self.new_obj(ObjClosure::new(function)).cast();
        self.push(Value::Obj(closure.cast()));
        self.call(closure, 0)?;
        self.run(false)?;
        Ok(())
    }

    #[inline]
    fn push(&mut self, value: Value) {
        // SAFETY: the stack pointer is defined in terms of the stack, and we check bounds at compile time
        //println!("pushing {value}, sp: {}", self.sp);
        self.stack[self.sp].set(value);
        self.sp += 1;
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.sp -= 1;
        //println!("popping {}, sp: {}", self.stack[self.sp].get(), self.sp);
        self.stack[self.sp].get()
    }

    #[inline]
    fn peek(&self, dist: usize) -> Value {
        self.stack[self.sp - 1 - dist].get()
    }

    pub fn collect_garbage(&mut self) {
        //println!("starting garbage collection");
        let vm = self as *const Vm;
        self.heap.mark_vm_roots(vm);
        self.heap.trace_references();
        self.heap.sweep();
    }

    pub fn free_objects(&mut self) {
        self.heap.free_objects();
    }

    #[inline]
    fn reset_stack(&mut self) {
        self.stack.fill(Self::DEFAULT_VAL);
        self.sp = 0;
    }

    pub fn run_file(&mut self, path: PathBuf) -> Result<()> {
        let mut files = vec![];
        let path = Path::new(&path);
        if path.extension().is_some_and(|s| s == OsStr::new("lox")) {
            files.push(path.to_path_buf());
        } else if path.is_dir() {
            println!("===== Running Files in {} =====", path.display());
            for entry in path.read_dir().unwrap() {
                self.run_file(entry.as_ref().unwrap().path())?;
                files.push(entry.as_ref().unwrap().path());
            }
        }
        for f in files {
            println!("~~ Running {} ~~", f.display());
            let mut file = File::open(Path::new(&f)).expect("valid files only");
            let mut source = String::new();
            file.read_to_string(&mut source)?;
            if let Err(e) = self.interpret(&source) {
                println!("{e}");
            }
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

    pub fn call(&mut self, closure: NonNull<ObjClosure<'a>>, arg_count: u8) -> Result<()> {
        unsafe {
            let function = (*closure.as_ptr()).function.as_ptr();
            if (*function).arity != arg_count {
                return Err(anyhow!(
                    "Expected {} arguments, but found {}.",
                    (*function).arity,
                    arg_count
                ));
            }

            if self.frame_count == FRAMES_MAX {
                // TODO: print stack trace
                return Err(anyhow!("Stack overflow."));
            }

            let frame = CallFrame {
                closure: closure.as_ptr() as *const ObjClosure,
                ip: (*function).chunk.code().as_ptr(),
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
                    ObjType::Closure => return self.call(o.cast(), arg_count),
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
                    ObjType::Class => {
                        let class = o.cast::<ObjClass>();
                        let obj = self.new_obj(ObjInstance::new(class));
                        self.stack[self.sp - arg_count as usize - 1].set(Value::Obj(obj));
                        if let Some(Value::Obj(init)) =
                            (*class.as_ptr()).methods.get(&self.init_string)
                        {
                            return self.call(init.cast(), arg_count);
                        } else if arg_count != 0 {
                            return Err(anyhow!("expected 0 arguments but got {arg_count}"));
                        }
                        return Ok(());
                    }
                    ObjType::BoundMethod => {
                        let bound = o.cast::<ObjBoundMethod>().as_ptr();
                        self.stack[self.sp - arg_count as usize - 1].set((*bound).receiver);
                        return self.call((*bound).method, arg_count);
                    }
                    _ => {}
                }
            },
            _ => {}
        }
        Err(anyhow!("can only call functions and classes."))
    }

    fn capture_upvalue(&mut self, local: *const Cell<Value>) -> *mut ObjUpvalue {
        unsafe {
            let mut prev = None;
            let mut upvalue = self.open_upvalues;
            while let Some(uv) = upvalue
            && (*uv.as_ptr()).location > local {
                prev = upvalue;
                upvalue = (*uv.as_ptr()).next;
            }

            if let Some(uv) = upvalue
            && (*uv.as_ptr()).location == local {
                return uv.as_ptr();
            }

            self.collect_garbage();
            let mut created = ObjUpvalue::new(local);
            created.next = upvalue;
            let created = self.new_obj(created).cast();

            match prev {
                Some(uv) => (*uv.as_ptr()).next = Some(created),
                None => self.open_upvalues = Some(created),
            }

            created.as_ptr()
        }
    }

    fn close_upvalues(&mut self, last: *const Cell<Value>) {
        unsafe {
            while let Some(ref uv) = self.open_upvalues
            && (*uv.as_ptr()).location >= last {
                let val = (*(*uv.as_ptr()).location).get().clone();
                (*uv.as_ptr()).closed.set(val);
                (*uv.as_ptr()).location = &(*uv.as_ptr()).closed;
                self.open_upvalues = (*uv.as_ptr()).next;
            }
        }
    }

    fn bind_method(&mut self, class: NonNull<ObjClass>, name: Value) -> Result<()> {
        unsafe {
            let Value::Obj(method) = (*class.as_ptr())
                .methods
                .get(&name)
                .ok_or(anyhow!(InterpretError::Runtime))?
            else {
                unreachable!()
            };
            let bound = self
                .heap
                .new_obj(ObjBoundMethod::new(self.peek(0), method.cast()));
            self.pop();
            self.push(Value::Obj(bound));
            Ok(())
        }
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
                    (*(*frame.closure).function.as_ptr()).chunk.constants()[i]
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
                let chunk = &(*(*frame.closure).function.as_ptr()).chunk;
                let code = chunk.code();
                let offset = frame.ip.offset_from(code.as_ptr()) as usize;
                if offset >= code.len() {
                    break;
                }

                if debug_trace {
                    disassemble_instruction(chunk, offset);
                }

                if ALLOCATED.load(std::sync::atomic::Ordering::Relaxed)
                    >= NEXT_GC.load(std::sync::atomic::Ordering::Relaxed)
                {
                    self.collect_garbage();
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
                        self.push(self.stack[frame.starting_slot + slot as usize].get());
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
                        self.stack[frame.starting_slot + slot as usize].set(self.peek(0));
                    }
                    OpCode::SetGlobal => {
                        let name = read_constant!();
                        let value = self.peek(0);
                        if self.globals.insert(name, value).is_none() {
                            self.globals.remove(&name);
                            return Err(anyhow!(InterpretError::Runtime));
                        }
                    }
                    OpCode::GetUpvalue => {
                        let slot = read_byte!() as usize;
                        self.push((*(*(*frame.closure).upvalues[slot].as_ptr()).location).get());
                    }
                    OpCode::SetUpvalue => {
                        let slot = read_byte!() as usize;
                        let upvalue = (*frame.closure).upvalues[slot].as_ptr();
                        (*upvalue).set_value(self.peek(0));
                    }
                    OpCode::GetProperty => {
                        let Value::Obj(instance) = self.peek(0) else {
                            return Err(anyhow!("Only instances have properties."));
                        };
                        let instance = instance.as_ptr();
                        if (*instance).kind != ObjType::Instance {
                            return Err(anyhow!("Only instances have properties."));
                        }
                        let name = read_constant!();
                        if let Some(val) = (*instance.cast::<ObjInstance>()).fields.get(&name) {
                            self.pop();
                            self.push(*val);
                            continue;
                        }
                        self.bind_method((*instance.cast::<ObjInstance>()).class, name)?;
                        continue;
                        return Err(anyhow!(InterpretError::Runtime));
                    }
                    OpCode::SetProperty => {
                        let instance = self.peek(1);
                        if let Value::Obj(obj) = instance {
                            let obj = obj.as_ptr();
                            if (*obj).kind == ObjType::Instance {
                                let instance = obj.cast::<ObjInstance>();
                                let name = read_constant!();
                                (*instance).fields.insert(name, self.peek(0));
                                let value = self.pop();
                                self.pop();
                                self.push(value);
                                continue;
                            }
                        }
                        return Err(anyhow!(InterpretError::Runtime));
                    }
                    OpCode::Equal => compare!(==),
                    OpCode::Greater => compare!(>),
                    OpCode::GreaterEqual => compare!(>=),
                    OpCode::Less => compare!(<),
                    OpCode::LessEqual => compare!(<=),
                    OpCode::Add => {
                        let r = self.pop();
                        let l = self.pop();
                        let result = match (l, r) {
                            (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
                            (Value::Obj(o), _) | (_, Value::Obj(o))
                                if (*o.as_ptr()).kind == ObjType::String =>
                            {
                                let obj = self.new_obj(ObjString::new(format!("{l}{r}")));
                                Value::Obj(obj)
                            }
                            _ => return Err(anyhow!("cannot add {l} and {r}")),
                        };
                        self.push(result);
                    }
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
                    OpCode::Closure => {
                        self.collect_garbage();
                        match read_constant!() {
                            Value::Obj(o) => {
                                let closure = self
                                    .heap
                                    .new_obj(ObjClosure::new(o.cast()))
                                    .cast::<ObjClosure>();
                                self.push(Value::Obj(closure.cast()));
                                let capacity = (*closure.as_ptr()).upvalues.capacity();
                                for _ in 0..capacity {
                                    let is_local = read_byte!() == 1;
                                    let index = read_byte!() as usize;
                                    if is_local {
                                        let val = &self.stack[frame.starting_slot + index]
                                            as *const Cell<Value>;
                                        let upvalue = self.capture_upvalue(val);
                                        (*closure.as_ptr())
                                            .upvalues
                                            .push(NonNull::new(upvalue).unwrap());
                                    } else {
                                        (*closure.as_ptr())
                                            .upvalues
                                            .push((*frame.closure).upvalues[index]);
                                    }
                                }
                            }
                            _ => return Err(anyhow!(InterpretError::Runtime)),
                        }
                    }
                    OpCode::CloseUpvalue => {
                        let last = &self.stack[self.sp - 1] as *const _;
                        while let Some(ref uv) = self.open_upvalues
                        && (*uv.as_ptr()).location >= last {
                            let val = (*(*uv.as_ptr()).location).get().clone();
                            (*uv.as_ptr()).closed.set(val);
                            (*uv.as_ptr()).location = &(*uv.as_ptr()).closed;
                            self.open_upvalues = (*uv.as_ptr()).next;
                        }
                        self.pop();
                    }
                    OpCode::Return => {
                        let result = self.pop();
                        let last = &self.stack[frame.starting_slot] as *const _;
                        while let Some(ref uv) = self.open_upvalues
                        && (*uv.as_ptr()).location >= last {
                            let val = (*(*uv.as_ptr()).location).get().clone();
                            (*uv.as_ptr()).closed.set(val);
                            (*uv.as_ptr()).location = &(*uv.as_ptr()).closed;
                            self.open_upvalues = (*uv.as_ptr()).next;
                        }
                        self.frame_count -= 1;
                        if self.frame_count == 0 {
                            self.pop();
                            break;
                        }
                        self.sp = frame.starting_slot;
                        self.push(result);
                        frame = self.frames[self.frame_count - 1];
                    }
                    OpCode::Class => {
                        let name = read_constant!();
                        let Value::Obj(o) = name else { unreachable!() };
                        let obj = self.new_obj(ObjClass::new(o.cast()));
                        self.push(Value::Obj(obj));
                    }
                    OpCode::Method => {
                        let name = read_constant!();
                        let method = self.peek(0);
                        let Value::Obj(class) = self.peek(1) else {
                            unreachable!()
                        };

                        let class = class.cast::<ObjClass>().as_ptr();
                        (*class).methods.insert(name, method);
                        self.pop();
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
        class CoffeeMaker {
            init(coffee) {
              this.coffee = coffee;
            }
          
            brew() {
              print "Enjoy your cup of " + this.coffee;
          
              // No reusing the grounds!
              this.coffee = nil;
            }
          }
          
          var maker = CoffeeMaker("coffee and chicory");
          maker.brew();
        "#;
        let _ = vm.interpret(source);
        vm.collect_garbage();
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
