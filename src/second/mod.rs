pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod scanner;
pub mod value;

use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{Read, Write},
    path::Path,
    ptr::NonNull,
};

use anyhow::{anyhow, Result};
use chunk::{Chunk, OpCode};
use compiler::compile;

use self::{
    debug::disassemble_instruction,
    value::{Obj, ObjFunction, ObjString, ObjType, Value},
};

const STACK_MAX: usize = 256;

#[derive(Debug, Clone)]
struct CallFrame<'a> {
    function: &'a ObjFunction,
    ip: usize,
    slots: &'a [Value],
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
    chunk: Option<Chunk>,
    stack: [Value; STACK_MAX],
    sp: usize,
    ip: usize,
    // With the rest of the architecture being what it is, clox's linked list object model was difficult,
    // since the only reliable time to append to the list was on every push, which means double free if not careful
    // Probably worth revisiting for performance, but maybe it's not even that bad, as iteration only has
    // a little more overhead, and the same amount of pointer indirection?
    objects: HashSet<*mut Obj>,
    globals: HashMap<Value, Value>,
}

impl Vm {
    pub fn init(&mut self) {
        self.chunk = None;
        self.stack.fill(Value::Nil);
        self.sp = 0;
        self.ip = 0;
    }

    pub fn new() -> Self {
        Self {
            chunk: None,
            stack: [Value::Nil; STACK_MAX],
            sp: 0,
            ip: 0,
            objects: HashSet::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        self.chunk = Some(compile(source)?);
        self.ip = 0;
        self.run(true)
    }

    #[inline]
    fn push(&mut self, value: Value) {
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
                    ObjType::String => Box::from_raw(obj.cast::<ObjString>()),
                    ObjType::Function => todo!(),
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

    #[inline]
    fn op_from_byte(&mut self) -> Result<OpCode> {
        OpCode::try_from(self.read_byte()).map_err(|_| anyhow!(InterpretError::Runtime))
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk
            .as_ref()
            .expect("only read when initialized")
            .code()[self.ip - 1]
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let byte = self.read_byte();
        self.chunk
            .as_mut()
            .expect("only called after initialization")
            .constants()[byte as usize]
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
                let code = self.chunk.as_ref().unwrap().code();
                self.ip += 2;
                let hi = code[self.ip - 2];
                let lo = code[self.ip - 1];
                u16::from_be_bytes([hi, lo]) as usize
            }};
        }

        loop {
            if debug_trace {
                let chunk = self
                    .chunk
                    .as_ref()
                    .expect("only called after initialization");
                if self.ip >= chunk.code().len() {
                    break;
                }

                disassemble_instruction(chunk, self.ip);
            }

            match self.op_from_byte()? {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Pop => _ = self.pop(),
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    self.push(self.stack[slot as usize]);
                }
                OpCode::GetGlobal => {
                    let name = self.read_constant();
                    //println!("{name:?}");
                    if let Some(value) = self.globals.get(&name) {
                        self.push(*value);
                    } else {
                        return Err(anyhow!(InterpretError::Runtime));
                    }
                }
                OpCode::DefineGlobal => {
                    let name = self.read_constant();
                    let value = self.peek(0);
                    self.globals.insert(name, value);
                    self.pop();
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = self.peek(0);
                }
                OpCode::SetGlobal => {
                    let name = self.read_constant();
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
                OpCode::Jump => self.ip += read_i16!(),
                OpCode::JumpIfFalse => {
                    let offset = read_i16!();
                    if !self.peek(0).is_truthy() {
                        self.ip += offset;
                    }
                }
                OpCode::Loop => self.ip -= read_i16!(),
                OpCode::Return => break,
            }
        }
        Ok(())
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
