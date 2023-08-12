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
    object::{Obj, ObjFunction, ObjString, ObjType},
    value::Value,
};

const STACK_MAX: usize = FRAMES_MAX * 256;
const FRAMES_MAX: usize = 64;

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    function: NonNull<ObjFunction>,
    ip: *const u8,
    slots: *mut Value,
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
            (*self.function.as_ptr()).chunk.constants()[i]
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
    frames: [MaybeUninit<CallFrame>; FRAMES_MAX],
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
    pub fn init(&mut self) {
        //self.chunk = None;
        self.stack.fill(Value::Nil);
        self.sp = 0;
        self.free_objects();
        self.globals.clear();
        //self.ip = 0;
    }

    pub fn new_frame(&mut self, function: NonNull<ObjFunction>) {
        unsafe {
            let frame = CallFrame {
                function,
                ip: (*function.as_ptr()).chunk.code().as_ptr(),
                slots: self.stack.split_at_mut(self.sp).1.as_mut_ptr(),
            };
        }
    }

    pub fn new() -> Self {
        Self {
            //chunk: None,
            frames: [MaybeUninit::uninit(); FRAMES_MAX],
            frame_count: 0,
            stack: [Value::Nil; STACK_MAX],
            sp: 0,
            //ip: 0,
            objects: HashSet::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        self.init();
        let mut function = compile(source)?;
        let ip = function.chunk.code().as_ptr();
        let mut frame = CallFrame {
            function: NonNull::new(&mut function as *mut ObjFunction).unwrap(),
            ip,
            slots: self.stack.split_at_mut(self.sp).1.as_mut_ptr(),
        };
        self.frames[self.frame_count] = MaybeUninit::new(frame);
        self.frame_count += 1;

        self.run(true)
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
        // unsafe {
        //     let value = *self.sp;
        //     self.sp.sub(1);
        //     value
        // }
        self.sp -= 1;
        //println!("popping {}, sp: {}", self.stack[self.sp], self.sp);
        self.stack[self.sp]
    }

    #[inline]
    fn peek(&self, dist: usize) -> Value {
        //unsafe { *self.sp.sub(1 + dist) }
        self.stack[self.sp - 1 - dist]
    }

    pub fn free_objects(&mut self) {
        for obj in (&mut self.objects).iter() {
            unsafe {
                match (*(*obj)).kind {
                    ObjType::String => drop(Box::from_raw(obj.cast::<ObjString>())),
                    ObjType::Function => drop(Box::from_raw(obj.cast::<ObjFunction>())),
                };
            }
        }
        self.objects.clear();
    }

    unsafe fn free_object(&mut self, obj: *mut Obj) {
        match (*obj).kind {
            ObjType::String => Box::from_raw(obj.cast::<ObjString>()),
            ObjType::Function => todo!(),
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

    pub fn run(&mut self, debug_trace: bool) -> Result<()> {
        unsafe {
            let mut frame = self.frames[self.frame_count - 1].assume_init();

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
                    (*frame.function.as_ptr()).chunk.constants()[i]
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
                    frame.ip = frame.ip.add(2);
                    let hi = *frame.ip.offset(-2);
                    let lo = *frame.ip.offset(-1);
                    u16::from_be_bytes([hi, lo])
                }};
            }

            loop {
                if debug_trace {
                    let chunk = &(*frame.function.as_ptr()).chunk;
                    let code = chunk.code();
                    let offset = frame.ip.offset_from(code.as_ptr()) as usize;
                    if offset >= code.len() {
                        break;
                    }

                    disassemble_instruction(chunk, offset);
                }

                match frame.op_from_byte()? {
                    OpCode::Constant => self.push(read_constant!()),
                    OpCode::Nil => self.push(Value::Nil),
                    OpCode::True => self.push(Value::Bool(true)),
                    OpCode::False => self.push(Value::Bool(false)),
                    OpCode::Pop => _ = self.pop(),
                    OpCode::GetLocal => {
                        let slot = frame.read_byte();
                        self.push(*frame.slots.offset(slot as isize));
                    }
                    OpCode::GetGlobal => {
                        let name = read_constant!();
                        //println!("{name:?}");
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
                        *frame.slots.offset(slot as isize) = self.peek(0);
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
                    OpCode::Jump => frame.ip = frame.ip.add(read_i16!() as usize),
                    OpCode::JumpIfFalse => {
                        if !self.peek(0).is_truthy() {
                            frame.ip = frame.ip.add(read_i16!() as usize);
                        }
                    }
                    OpCode::Loop => frame.ip = frame.ip.sub(read_i16!() as usize),
                    OpCode::Return => break,
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
        let source = "5 >= 6\n\"hello\"\n\"hi I'm dkflajf\"\n\"more strings here\"";
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
    }

    fn calc_hash<T: std::hash::Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}
