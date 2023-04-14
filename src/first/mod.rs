use anyhow::Result;
use clap::Parser as ClapParser;

use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use self::parser::Parser;

mod parser;
mod syntax;

#[derive(Debug, ClapParser)]
pub struct LoxArgs {
    /// Path to complete lox file.
    /// if empty opens a lox prompt.
    pub path: Option<String>,
}

pub fn run_file(path: &str) -> Result<()> {
    let mut file = File::open(Path::new(path)).expect("valid files only");
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    run(&source)
}

pub fn run_prompt() -> Result<()> {
    let mut input = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().expect("new prompt failed");
        if std::io::stdin().read_line(&mut input).is_ok() {
            if input.is_empty() {
                break;
            } else {
                run(&input).unwrap();
            }
        }
    }

    Ok(())
}

fn run(source: &str) -> Result<()> {
    let mut parser = Parser::new();
    // for r in tokens.iter() {
    //     match r {
    //         // just printing for now
    //         ((row, col), Ok(t)) => println!("{row}, {col}: {t}"),
    //         ((row, col), Err(e)) => println!("{row}, {col}: {e}"),
    //     }
    // }

    let mut exprs = parser.parse(source);
    if exprs.all(|r| r.1.is_ok()) {
        // let vals: Vec<_> = exprs
        //     .into_iter()
        //     .map(|Parsed(i, e)| (i, e.and_then(|e| e.evaluate())))
        //     .collect();
        // for val in vals {
        //     println!("{}", val.1.unwrap());
        // }
    } else {
        for parsed in exprs {
            println!("{parsed}");
        }
    }
    Ok(())
}
