use num_traits::FromPrimitive;

use super::chunk::{OpCode, Chunk};

struct Vm {
    chunk: Chunk,
    ip: usize
}

impl Vm {
    fn read_byte(&mut self) -> u8 {
        let ret = self.chunk.read_byte(self.ip);
        self.ip += 1;
        ret
    }

    pub fn run(&mut self) {
        loop {
            let instr = OpCode::from_u8(self.read_byte()).expect("Invalid opcode");

            match instr {
                OpCode::Const => {
                    let idx = self.chunk.read_byte(self.ip);
                    let konst = self.chunk.get_const(idx.into());
                    println!("{konst}");
                }
                x => unimplemented!("Unimplemented opcode {x:?}")
            }
        }
    }

}
