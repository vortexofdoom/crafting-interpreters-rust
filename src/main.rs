#![feature(let_chains)]
#![allow(non_upper_case_globals)]
mod first;
mod second;

use std::alloc::System;

use anyhow::Result;
use clap::Parser;
use second::vm::Vm;
use stats_alloc::{StatsAlloc, INSTRUMENTED_SYSTEM};

#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

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
        vm.run_file(p.into())?;
    } else {
        vm.run_prompt()?;
    }
    vm.free_objects();
    Ok(())
}
