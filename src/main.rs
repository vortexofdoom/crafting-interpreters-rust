#![feature(let_chains)]
#![feature(allocator_api)]
#![allow(non_upper_case_globals, unused)]
mod first;
mod second;

use anyhow::Result;
use clap::Parser;
use second::Vm;

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
