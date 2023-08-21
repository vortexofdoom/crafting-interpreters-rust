pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod memory;
pub mod object;
pub mod scanner;
pub mod value;

use std::{
    cell::Cell,
    ffi::OsStr,
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    ptr::NonNull,
    sync::atomic::Ordering::Relaxed,
};

use anyhow::{anyhow, Result};
use chunk::OpCode;
use compiler::compile;
use fnv::FnvHashMap;

use crate::{
    second::{memory::NEXT_GC, object::Upvalue},
    GLOBAL,
};

use self::{
    debug::disassemble_instruction,
    memory::Heap,
    object::{
        IsObj, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjInstance, ObjNative, ObjString,
        ObjType, ObjUpvalue,
    },
    value::Value,
};

const STACK_MAX: usize = FRAMES_MAX * 256;
const FRAMES_MAX: usize = 64;

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    closure: *const ObjClosure,
    ip: *const u8,
    starting_slot: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            closure: std::ptr::null(),
            ip: std::ptr::null(),
            starting_slot: Default::default(),
        }
    }
}

pub struct Vm {
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,
    stack: [Cell<Value>; STACK_MAX],
    sp: usize,
    globals: FnvHashMap<Value, Value>,
    natives: FnvHashMap<Value, Value>,
    open_upvalues: Option<NonNull<ObjUpvalue>>,
    init_string: Value,
    heap: Heap,
}

impl Vm {
    pub fn new() -> Self {
        let mut heap = Heap::new();
        let obj = heap.new_obj(ObjString::new(String::from("init")));
        let mut vm = Self {
            frames: [CallFrame::default(); FRAMES_MAX],
            frame_count: 0,
            stack: std::array::from_fn(|_| Cell::new(Value::NIL)),
            sp: 0,
            globals: FnvHashMap::default(),
            natives: FnvHashMap::default(),
            open_upvalues: None,
            init_string: Value::from(obj),
            heap,
        };

        vm.define_native("clock", 0, Value::clock);
        vm
    }

    fn new_obj<T: IsObj>(&mut self, obj: T) -> NonNull<Obj> {
        if GLOBAL.stats().bytes_reallocated > NEXT_GC.load(Relaxed) {
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
        self.push(Value::from(name));
        let native = self.new_obj(ObjNative::new(arity, function));
        self.push(Value::from(native));
        self.natives.insert(Value::from(name), Value::from(native));
        self.pop();
        self.pop();
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        if let Some(function) = compile(source, self as *mut _) {
            self.push(Value::from(function.cast()));
            let closure = self.new_obj(ObjClosure::new(function)).cast();
            self.pop();
            self.push(Value::from(closure.cast()));
            self.call(closure, 0)?;
            self.run(false)?;
        }
        Ok(())
    }

    #[inline]
    fn push(&mut self, value: Value) {
        self.stack[self.sp].set(value);
        self.sp += 1;
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp].get()
    }

    #[inline]
    fn peek(&self, dist: usize) -> Value {
        self.stack[self.sp - 1 - dist].get()
    }

    pub fn collect_garbage(&mut self) {
        //println!("starting garbage collection from VM");
        self.mark_roots();
        self.heap.trace_references();
        self.heap.sweep();
    }

    fn mark_roots(&mut self) {
        self.heap.mark_value(self.init_string);

        for val in &self.stack[..self.sp] {
            self.heap.mark_value(val.get());
        }

        for (name, value) in self.globals.iter() {
            self.heap.mark_value(*name);
            self.heap.mark_value(*value);
        }

        for (name, value) in self.natives.iter() {
            self.heap.mark_value(*name);
            self.heap.mark_value(*value);
        }

        for frame in &self.frames[..self.frame_count] {
            let obj = NonNull::new(frame.closure.cast_mut().cast()).unwrap();
            self.heap.mark_obj(obj);
        }

        let mut upvalue = self.open_upvalues;
        while let Some(uv) = upvalue {
            self.heap.mark_obj(uv.cast());
            upvalue = Some(uv);
        }
    }

    pub fn free_objects(&mut self) {
        self.heap.free_objects();
    }
    
    fn reset(&mut self) {
        self.globals.clear();
        self.frame_count = 0;
        self.sp = 0;
        self.open_upvalues = None;
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
            self.reset();
            source.clear();
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
                } else if let Err(e) = self.interpret(&input[start..]) {
                    println!("{e}");
                }
            }
        }
        Ok(())
    }

    pub fn call(&mut self, closure: NonNull<ObjClosure>, arg_count: u8) -> Result<()> {
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
        unsafe {
            let obj = value
                .as_obj()
                .map_err(|_| anyhow!("can only call functions and classes."))?;
            match obj.as_ref().kind {
                ObjType::Closure => self.call(obj.cast(), arg_count),
                ObjType::Native => {
                    let native = obj.cast::<ObjNative>().as_ref();
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
                    Ok(())
                }
                ObjType::Class => {
                    let class = obj.cast::<ObjClass>();
                    let obj = self.new_obj(ObjInstance::new(class));
                    self.stack[self.sp - arg_count as usize - 1].set(Value::from(obj));
                    if let Some(init) = (*class.as_ptr()).methods.get(&self.init_string) {
                        return self.call((*init).as_obj()?.cast(), arg_count);
                    } else if arg_count != 0 {
                        return Err(anyhow!("expected 0 arguments but got {arg_count}"));
                    }
                    Ok(())
                }
                ObjType::BoundMethod => {
                    let bound = obj.cast::<ObjBoundMethod>().as_ptr();
                    self.stack[self.sp - arg_count as usize - 1].set((*bound).receiver);
                    self.call((*bound).method, arg_count)
                }
                _ => Err(anyhow!("can only call functions and classes.")),
            }
        }
    }

    fn capture_upvalue(&mut self, local: usize) -> *mut ObjUpvalue {
        unsafe {
            let mut prev = None;
            let mut upvalue = self.open_upvalues;
            while let Some(uv) = upvalue
            && let Upvalue::Open(slot) = (*uv.as_ptr()).value
            && slot >= local {
                prev = upvalue;
                upvalue = (*uv.as_ptr()).next;
            }

            if let Some(uv) = upvalue
            && let Upvalue::Open(slot) = (*uv.as_ptr()).value
            && slot == local {
                return uv.as_ptr();
            }

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

    fn close_upvalues(&mut self, last: usize) {
        unsafe {
            while let Some(uv) = self.open_upvalues
            && let Upvalue::Open(slot) = (*uv.as_ptr()).value
            && slot >= last {
                let val = self.stack[slot].clone();
                (*uv.as_ptr()).value = Upvalue::Closed(val);
                self.open_upvalues = (*uv.as_ptr()).next;
            }
        }
    }

    fn bind_method(&mut self, class: NonNull<ObjClass>, name: Value) -> Result<()> {
        unsafe {
            let method = (*class.as_ptr())
                .methods
                .get(&name)
                .ok_or(anyhow!("runtime error."))
                .and_then(|v| v.as_obj())?;
            let bound = self
                .heap
                .new_obj(ObjBoundMethod::new(self.peek(0), method.cast()));
            self.pop();
            self.push(Value::from(bound));
            Ok(())
        }
    }

    fn invoke(&mut self, name: Value, arg_count: u8) -> Result<()> {
        let receiver = self.peek(arg_count as usize);
        unsafe {
            if let Ok(o) = receiver.as_obj()
            && (*o.as_ptr()).kind == ObjType::Instance {
                let instance = o.as_ptr().cast::<ObjInstance>();
                if let Some(val) = (*instance).fields.get(&name) {
                    self.stack[self.sp - arg_count as usize - 1].set(*val);
                    self.call_value(*val, arg_count)
                } else {
                    self.invoke_from_class((*o.as_ptr().cast::<ObjInstance>()).class, name, arg_count)
                }
            } else {
                Err(anyhow!("Only instances have methods."))
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        class: NonNull<ObjClass>,
        name: Value,
        arg_count: u8,
    ) -> Result<()> {
        unsafe {
            let method = (*class.as_ptr())
                .methods
                .get(&name)
                .ok_or(anyhow!("Undefined property {name}."))
                .and_then(|v| v.as_obj())?;
            self.call(method.cast(), arg_count)
        }
    }

    pub fn run(&mut self, debug_trace: bool) -> Result<()> {
        unsafe {
            let mut frame = self.frames[self.frame_count - 1];

            macro_rules! read_byte {
                () => {{
                    let byte = *frame.ip;
                    frame.ip = frame.ip.add(1);
                    byte
                }};
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
                    self.push(Value::from(l $op r));
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

                let byte = read_byte!();

                match OpCode::try_from(byte).map_err(|_| anyhow!("invalid opcode {byte}"))? {
                    OpCode::Constant => {
                        let constant = read_constant!();
                        self.push(constant);
                    }
                    OpCode::Nil => self.push(Value::NIL),
                    OpCode::True => self.push(Value::from(true)),
                    OpCode::False => self.push(Value::from(false)),
                    OpCode::Pop => _ = self.pop(),
                    OpCode::GetLocal => {
                        let slot = read_byte!() as usize;
                        self.push(self.stack[frame.starting_slot + slot].get());
                    }
                    OpCode::GetGlobal => {
                        let name = read_constant!();
                        if let Some(value) =
                            self.globals.get(&name).or_else(|| self.natives.get(&name))
                        {
                            self.push(*value);
                        } else {
                            return Err(anyhow!("Undefined Global '{name}'"));
                        }
                    }
                    OpCode::DefineGlobal => {
                        let name = read_constant!();
                        let value = self.peek(0);
                        self.globals.insert(name, value);
                        self.pop();
                    }
                    OpCode::SetLocal => {
                        let slot = read_byte!() as usize;
                        self.stack[frame.starting_slot + slot].set(self.peek(0));
                    }
                    OpCode::SetGlobal => {
                        let name = read_constant!();
                        let value = self.peek(0);
                        if self.globals.insert(name, value).is_none() {
                            self.globals.remove(&name);
                            return Err(anyhow!("Undefined Global '{name}'"));
                        }
                    }
                    OpCode::GetUpvalue => {
                        let slot = read_byte!() as usize;
                        let value = match &mut (*(*frame.closure).upvalues[slot].as_ptr()).value {
                            Upvalue::Open(stack_slot) => self.stack[*stack_slot].get(),
                            Upvalue::Closed(val) => val.get(),
                        };
                        self.push(value);
                    }
                    OpCode::SetUpvalue => {
                        let slot = read_byte!() as usize;
                        let value = self.peek(0);
                        match &mut (*(*frame.closure).upvalues[slot].as_ptr()).value {
                            Upvalue::Closed(val) => val.set(value),
                            Upvalue::Open(stack_slot) => self.stack[*stack_slot].set(value),
                        }
                    }
                    OpCode::GetProperty => {
                        let instance = self.peek(0).as_obj().and_then(|obj| {
                            let obj = obj.as_ptr();
                            match (*obj).kind {
                                ObjType::Instance => Ok(obj.cast::<ObjInstance>()),
                                _ => Err(anyhow!("Only instances have properties.")),
                            }
                        })?;
                        let name = read_constant!();
                        if let Some(val) = (*instance).fields.get(&name) {
                            self.pop();
                            self.push(*val);
                            continue;
                        }
                        self.bind_method((*instance).class, name)?;
                    }
                    OpCode::SetProperty => {
                        let instance = self.peek(1);
                        if let Ok(obj) = instance.as_obj() {
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
                        return Err(anyhow!("Undefined property."));
                    }
                    OpCode::GetSuper => {
                        let name = read_constant!();
                        let superclass = self.pop().as_obj()?;
                        self.bind_method(superclass.cast(), name)?;
                    }
                    OpCode::Equal => compare!(==),
                    OpCode::Greater => compare!(>),
                    OpCode::GreaterEqual => compare!(>=),
                    OpCode::Less => compare!(<),
                    OpCode::LessEqual => compare!(<=),
                    OpCode::Add => {
                        let r = self.peek(0);
                        let l = self.peek(1);
                        let result = if let (Ok(x), Ok(y)) = (l.as_num(), r.as_num()) {
                            Value::from(x + y)
                        } else if let (Ok(o), _) | (_, Ok(o)) = (l.as_obj(), r.as_obj())
                        && (*o.as_ptr()).kind == ObjType::String {
                            let obj = self.new_obj(ObjString::new(format!("{l}{r}")));
                            Value::from(obj)
                        } else {
                            return Err(anyhow!("cannot add {l} and {r}"));
                        };
                        self.pop();
                        self.pop();
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
                    OpCode::Invoke => {
                        let method = read_constant!();
                        let arg_count = read_byte!();
                        self.frames[self.frame_count - 1] = frame;
                        self.invoke(method, arg_count)?;
                        frame = self.frames[self.frame_count - 1];
                    }
                    OpCode::SuperInvoke => {
                        let method = read_constant!();
                        let arg_count = read_byte!();
                        let superclass = self.pop().as_obj()?;
                        self.frames[self.frame_count - 1] = frame;
                        self.invoke_from_class(superclass.cast(), method, arg_count)?;
                        frame = self.frames[self.frame_count - 1];
                    }
                    OpCode::Closure => {
                        read_constant!().as_obj().map(|o| {
                            let closure = self
                                .heap
                                .new_obj(ObjClosure::new(o.cast()))
                                .cast::<ObjClosure>();
                            self.push(Value::from(closure.cast()));
                            let capacity = (*closure.as_ptr()).upvalues.capacity();
                            for _ in 0..capacity {
                                let is_local = read_byte!() == 1;
                                let index = read_byte!() as usize;
                                if is_local {
                                    let upvalue = self.capture_upvalue(frame.starting_slot + index);
                                    (*closure.as_ptr())
                                        .upvalues
                                        .push(NonNull::new(upvalue).unwrap());
                                } else {
                                    (*closure.as_ptr())
                                        .upvalues
                                        .push((*frame.closure).upvalues[index]);
                                }
                            }
                        })?;
                    }
                    OpCode::CloseUpvalue => {
                        let last = self.sp - 1;
                        self.close_upvalues(last);
                        self.pop();
                    }
                    OpCode::Return => {
                        let result = self.pop();
                        let last = frame.starting_slot;
                        self.close_upvalues(last);
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
                        let name = read_constant!().as_obj()?.cast();
                        let class = self.new_obj(ObjClass::new(name));
                        self.push(Value::from(class));
                    }
                    OpCode::Inherit => {
                        let superclass = self.peek(1);
                        let subclass = self.peek(0);
                        let sup = superclass
                            .as_obj()
                            .map_err(|_| anyhow!("Superclass must be a class."))?;
                        let sub = subclass
                            .as_obj()
                            .map_err(|_| anyhow!("Subclass must be a class."))?;
                        match (*sup.as_ptr()).kind {
                            ObjType::Class => {
                                let (sup, sub) = (sup.cast::<ObjClass>(), sub.cast::<ObjClass>());
                                (*sub.as_ptr()).methods = (*sup.as_ptr()).methods.clone();
                                self.pop();
                            }
                            _ => return Err(anyhow!("Superclass must be a class.")),
                        }
                    }
                    OpCode::Method => {
                        let name = read_constant!();
                        let method = self.peek(0);
                        let class = self.peek(1).as_obj()?.cast::<ObjClass>().as_ptr();
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
    fn test_functions() {
        let mut vm = Vm::new();
        let source = r#"../craftinginterpreters/test/"#;
        let _ = vm.interpret(source);
        vm.collect_garbage();
        vm.free_objects();
    }

    #[test]
    fn test_hash() {
        let mut vm = Vm::new();
        let val1 = Value::from(vm.new_obj(ObjString::new(String::from("hello"))));
        let val2 = Value::from(vm.new_obj(ObjString::new(String::from("hello"))));
        assert_eq!(val1, val2);
        println!("{}, {}", calc_hash(&val1), calc_hash(&val2));
        let mut set = HashSet::new();
        set.insert(val1);
        assert!(set.get(&val2).is_some());
        vm.free_objects();
    }

    fn calc_hash<T: std::hash::Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}
