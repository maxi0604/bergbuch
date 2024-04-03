use std::fmt::Display;

#[repr(u8)]
pub enum OpCode {
    Const = 0x0,
    Return
}

#[derive(Debug)]
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
}
