#![feature(let_chains)]
#![feature(box_into_inner)]
#![allow(non_upper_case_globals, unused)]
mod first;
mod second;

use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use anyhow::Result;
use clap::Parser;
use second::{
    chunk::{Chunk, OpCode},
    scanner::scan,
    InterpretError, Vm,
};

#[derive(Debug, Parser)]
pub struct LoxArgs {
    /// Path to complete lox file.
    /// if empty opens a lox prompt.
    pub path: Option<String>,
}

fn main() -> Result<()> {
    let args = LoxArgs::parse();
    let mut vm = Vm::new();
    if let Some(p) = args.path {
        vm.run_file(&p)?;
    } else {
        vm.run_prompt()?;
    }
    vm.free_objects();
    Ok(())
}
