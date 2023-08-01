pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod scanner;
pub mod value;

use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use anyhow::{anyhow, Result};
use chunk::{Chunk, OpCode};
use compiler::compile;

use self::{debug::disassemble_instruction, scanner::scan, value::Value};

const STACK_MAX: usize = 256;

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
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.sp -= 1;
        //println!("popping {}, sp: {}", self.stack[self.sp], self.sp);
        self.stack[self.sp]
    }

    #[inline]
    fn reset_stack(&mut self) {
        self.stack.fill(Value::Nil);
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
            if std::io::stdin().read_line(&mut input).is_ok() {
                if input == "\n" {
                    break;
                } else {
                    if let Err(e) = self.interpret(&input) {
                        println!("{e}");
                    }
                    input.clear();
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
                println!("{}", l $op r);
                self.push(Value::Bool(l $op r));
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
                    let value = (!self.pop())?;
                    self.push(value);
                }
                OpCode::Negate => {
                    let value = (-self.pop())?;
                    self.push(value);
                }
                OpCode::Return => {
                    println!("{}", self.pop());
                    break;
                }
            }
        }
        Ok(())
    }
}
