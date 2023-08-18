use datasize::DataSize;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use super::value::Value;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoPrimitive, TryFromPrimitive)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    GetGlobal,
    DefineGlobal,
    SetLocal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Inherit,
    Method,
}

#[derive(Debug, Default, Clone, DataSize)]
pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    #[inline]
    pub fn lines(&self) -> &[usize] {
        &self.lines
    }

    #[inline]
    pub fn code_mut(&mut self) -> &mut [u8] {
        &mut self.code
    }

    #[inline]
    pub fn constants(&self) -> &[Value] {
        //println!("{:?}", self.constants);
        &self.constants
    }

    #[inline]
    pub fn get_line(&self, offset: usize) -> usize {
        let mut curr_line = 0;
        let mut total = 0;
        for line in &self.lines {
            curr_line += 1;
            total += line;
            if total >= offset {
                break;
            }
        }
        curr_line
    }

    pub fn write<T: Into<u8>>(&mut self, op: T, line: usize) {
        // if we don't have enough lines to insert something on this line, push 0s until we do.
        while self.lines.len() <= line {
            self.lines.push(0);
        }
        if self.lines.len() > line {
            self.lines[line] += 1;
        } else {
            self.lines.push(1);
        }
        self.code.push(op.into());
    }

    pub fn write_bytes(&mut self, bytes: &[u8], line: usize) {
        for byte in bytes {
            self.write(*byte, line)
        }
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }
}
