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
    if offset == 0 {
        print!("0001 ");
    } else if line == chunk.get_line(offset.saturating_sub(1)) {
        print!("   | ");
    } else {
        print!("{:04} ", line + 1);
    }
    return OpCode::try_from(chunk.code()[offset]).map(|op| match op {
        OpCode::Constant
        | OpCode::DefineGlobal
        | OpCode::GetGlobal
        | OpCode::SetGlobal
        | OpCode::GetProperty
        | OpCode::SetProperty
        | OpCode::Method 
        | OpCode::Class
        | OpCode::GetSuper => constant_inst(chunk, offset),
        OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::SetUpvalue
        | OpCode::GetUpvalue 
        | OpCode::Call => byte_inst(chunk, offset),
        OpCode::Invoke => invoke_inst(chunk, offset),
        OpCode::Jump | OpCode::JumpIfFalse => jump_inst(chunk, 1, offset),
        OpCode::Loop => jump_inst(chunk, -1, offset),
        OpCode::Closure => {
            let constant = chunk.code()[offset + 1];
            println!(
                "{:16?} {:4} -> {}",
                OpCode::try_from(chunk.code()[offset]).unwrap(),
                constant,
                chunk.constants()[constant as usize],
            );
            offset + 2
        }
        _ => simple_inst(chunk, offset),
    }).unwrap()//.unwrap_or({println!("unknown opcode value {:x}", chunk.code()[offset]); 1})
}

fn invoke_inst(chunk: &Chunk, offset: usize) -> usize {
    let code = chunk.code();
    println!("{:16?} ({}) {:4}", OpCode::try_from(code[offset]).unwrap(), code[offset + 1], code[offset + 2]);
    offset + 3
}

pub fn jump_inst(chunk: &Chunk, sign: isize, offset: usize) -> usize {
    let jump = u16::from_be_bytes([chunk.code()[offset + 1], chunk.code()[offset + 2]]) as isize;
    println!(
        "{:16?} {:4} -> {}",
        OpCode::try_from(chunk.code()[offset]).unwrap(),
        offset,
        offset as isize + 3 + sign * jump,
    );
    offset + 3
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

pub fn byte_inst(chunk: &Chunk, offset: usize) -> usize {
    println!(
        "{:16?} {:4}",
        OpCode::try_from(chunk.code()[offset]).unwrap(),
        chunk.code()[offset + 1],
    );
    offset + 2
}
