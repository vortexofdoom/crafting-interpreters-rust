#![feature(let_chains)]
mod first;

use anyhow::Result;
use clap::Parser;
use first::{run_file, run_prompt, LoxArgs};

fn main() -> Result<()> {
    let args = LoxArgs::parse();
    if let Some(p) = args.path {
        run_file(&p)
    } else {
        run_prompt()
    }
}
