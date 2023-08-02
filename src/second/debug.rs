use super::chunk::{Chunk, OpCode};

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("=={name}==");
    let mut i = 0;
    while i < chunk.code().len() {
        i = disassemble_instruction(chunk, i);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");
    let line = chunk.get_line(offset);
    if line == 1 {
        print!("0001 ");
    } else if line == chunk.get_line(offset.saturating_sub(1)) {
        print!("   | ");
    } else {
        print!("{:04} ", line + 1);
    }
    match OpCode::try_from(chunk.code()[offset]).unwrap() {
        OpCode::Constant => constant_inst(chunk, offset),
        _ => simple_inst(chunk, offset),
    }
}

pub fn constant_inst(chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code()[offset + 1] as usize;
    println!(
        "{:16?} {:4} '{}'",
        OpCode::try_from(chunk.code()[offset]).unwrap(),
        constant,
        chunk.constants()[constant]
    );
    offset + 2
}

pub fn simple_inst(chunk: &Chunk, offset: usize) -> usize {
    println!("{:?}", OpCode::try_from(chunk.code()[offset]).unwrap());
    offset + 1
}