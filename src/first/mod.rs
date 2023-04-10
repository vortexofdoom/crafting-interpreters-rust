use anyhow::{anyhow, Result};
use clap::Parser;
use itertools::{Itertools, MultiPeek};
use std::fs::File;
use std::io::{Read, Write};
//use std::iter::Peekable;
use std::path::Path;

#[derive(Debug, Parser)]
pub struct LoxArgs {
    /// Path to complete lox file.
    /// if empty opens a lox prompt.
    pub path: Option<String>,
}

pub enum Token {
    OneChar(char),
    CharThenEqual(char),
    Identifier(String),
    String(String),
    Number(f64),
    Keyword(Keyword),
}

pub enum Keyword {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

struct Scanner {
    line: usize,
}

impl Scanner {}

pub fn scan_tokens(source: &str) -> Vec<Result<Token>> {
    let mut tokens = source
        .lines()
        .enumerate()
        .map(|(i, l)| {
            let mut chars = l.chars().multipeek();
            let mut tokens = vec![];
            while let Some(res) | Some(res) = scan_token(&mut chars) {
                match res {
                    Ok(token) => tokens.push(Ok(token)),
                    Err(e) => tokens.push(Err(anyhow!("error on line {}: {}", i + 1, e))),
                }
            }
            tokens.into_iter()
        })
        .flatten();

    if !tokens.all(|t| t.is_ok()) {
        tokens.filter(|t| t.is_err()).collect()
    } else {
        tokens.filter(|t| t.is_ok()).collect()
    }
}

pub fn scan_token(chars: &mut MultiPeek<impl Iterator<Item = char>>) -> Option<Result<Token>> {
    if let Some(c) = chars.next() {
        match c {
            // Invariably single char tokens
            c @ ('(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*') => {
                Some(Ok(Token::OneChar(c)))
            }
            c @ ('!' | '=' | '<' | '>') => {
                if chars.peek() == Some(&'=') {
                    Some(Ok(Token::CharThenEqual(c)))
                } else {
                    Some(Ok(Token::OneChar(c)))
                }
            }
            '/' => {
                if chars.peek() == Some(&'/') {
                    // another slash means we have a comment and can ignore the rest of the line after this point
                    None
                } else {
                    Some(Ok(Token::OneChar('/')))
                }
            }
            '"' => {
                let string = chars.take_while(|&c| c != '"').collect();
                if chars.next() != Some('"') {
                    Some(Err(anyhow!("no closing '\"' found")))
                } else {
                    Some(Ok(Token::String(string)))
                }
            }
            n if n.is_ascii_digit() => {
                let mut num_chars = chars.enumerate();
                while let Some((_, c)) = num_chars.next() && c.is_ascii_digit() {
                    // we're just cranking the index until we don't see a number here
                }

                if let Some((_, '.')) = num_chars.next()
                && let Some((_, n)) = num_chars.next()
                && n.is_ascii_digit() {
                    while let Some((_, c)) = num_chars.next() && c.is_ascii_digit() {
                        // we're just cranking the index until we don't see a number here
                    }            
                }

                if let Some((i, _)) = num_chars.next() {
                    // we should have advanced to the first non-number character and everything should "Just Work"
                    let string: String = chars.take(i).collect();
                    if let Ok(n) = string.parse::<f64>() {
                        Some(Ok(Token::Number(n)))
                    } else {
                        Some(Err(anyhow!("error parsing number token {string}")))
                    }
                } else {
                    // technically if that if let fails we're guaranteed to get a syntax error
                    // because it means the number is the end of the line, 
                    // but this single digit is still a valid token
                    Some(Ok(Token::Number(c.to_digit(10).unwrap() as f64)))
                }         
            }
            _ => Some(Err(anyhow!("unexpected token {c}"))),
        }
    } else {
        None
    }
}

pub fn run_file(path: &str) -> Result<()> {
    let mut file = File::open(Path::new(path)).expect("valid files only");
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    run(&source)
}

pub fn run_prompt() -> Result<()> {
    print!("> ");
    std::io::stdout().flush().expect("new prompt failed");
    let mut input = String::new();
    while std::io::stdin().read_line(&mut input).is_ok() {
        if input.is_empty() {
            break;
        } else {
            run(&input);
        }
        print!("> ");
        std::io::stdout().flush().expect("new prompt failed");
    }
    Ok(())
}

fn run(source: &str) -> Result<()> {
    Ok(())
}
