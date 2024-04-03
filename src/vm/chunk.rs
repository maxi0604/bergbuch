use std::fmt::Display;

use num_derive::FromPrimitive;

#[repr(u8)]
#[derive(FromPrimitive, Debug)]
pub enum OpCode {
    Const = 0x0,
    Return
}

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64)
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(x) => write!(f, "{x}")
        }
    }
}

pub struct Chunk {
    consts: Vec<Value>,
    code: Vec<u8>,
    lines: Vec<usize>
}

impl Chunk {
    pub fn write_code(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }
    pub fn add_const(&mut self, val: Value) -> usize {
        self.consts.push(val);
        self.consts.len() - 1
    }

    pub fn read_byte(&self, idx: usize) -> u8 {
        self.code[idx]
    }

    pub fn get_line(&self, idx: usize) -> usize {
        self.lines[idx]
    }

    pub fn get_const(&self, idx: usize) -> Value {
        self.consts[idx].clone()
    }
}
