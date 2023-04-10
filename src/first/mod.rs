use anyhow::{anyhow, Result};
use clap::Parser;
use itertools::{Itertools, MultiPeek};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

#[derive(Debug, Parser)]
pub struct LoxArgs {
    /// Path to complete lox file.
    /// if empty opens a lox prompt.
    pub path: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Keyword::And => "and",
            Keyword::Class => "class",
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::Fun => "fun",
            Keyword::For => "for",
            Keyword::If => "if",
            Keyword::Nil => "nil",
            Keyword::Or => "or",
            Keyword::Print => "print",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::This => "this",
            Keyword::True => "true",
            Keyword::Var => "var",
            Keyword::While => "while",
        };
        write!(f, "{s}")
    }
}

static KEYWORDS: Lazy<HashMap<&'static str, Keyword>> = Lazy::new(|| {
    use Keyword::*;
    let mut keywords = HashMap::new();
    keywords.insert("and", And);
    keywords.insert("class", Class);
    keywords.insert("else", Else);
    keywords.insert("false", False);
    keywords.insert("for", For);
    keywords.insert("fun", Fun);
    keywords.insert("if", If);
    keywords.insert("nil", Nil);
    keywords.insert("or", Or);
    keywords.insert("print", Print);
    keywords.insert("return", Return);
    keywords.insert("super", Super);
    keywords.insert("this", This);
    keywords.insert("true", True);
    keywords.insert("var", Var);
    keywords.insert("while", While);
    keywords
});

pub enum Token {
    OneChar(char),
    CharThenEqual(char),
    Identifier(String),
    String(String),
    Number(f64),
    Keyword(Keyword),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::OneChar(c) => write!(f, "{c}"),
            Token::CharThenEqual(c) => write!(f, "{c}="),
            Token::Identifier(s) => write!(f, "{s}"),
            Token::String(s) => write!(f, "{s}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::Keyword(k) => write!(f, "{k}"),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnclosedString(usize),
    ParseNumError(usize, String),
    InvalidToken(usize, char),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnclosedString(i) => {
                write!(f, "{i}: end of line reached with no closing '\"' found")
            }
            ParseError::ParseNumError(i, s) => write!(f, "{i}: error parsing number '{s}'"),
            ParseError::InvalidToken(i, c) => write!(f, "{i}: invalid token '{c}'"),
        }
    }
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
    for r in scan_tokens(source) {
        match r {
            // just printing for now
            Ok(t) => println!("{t}"),
            Err(e) => println!("{e}"),
        }
    }
    Ok(())
}

fn scan_tokens(source: &str) -> Vec<Result<Token>> {
    source
        .lines()
        .enumerate()
        .flat_map(|(i, l)| {
            let (tokens, errors): (Vec<_>, Vec<_>) = scan_line(l).partition(|r| r.is_ok());
            if errors.is_empty() {
                tokens
            } else {
                errors
                    .into_iter()
                    .filter_map(|r| {
                        if let Err(e) = r {
                            Some(Err(anyhow!("Error at line {i}, {e}")))
                        } else {
                            None
                        }
                    })
                    .collect()
            }
        })
        .collect()
}

fn scan_line(line: &str) -> impl Iterator<Item = Result<Token>> {
    let mut chars = line.chars().enumerate().multipeek();
    let mut tokens = vec![];
    while let Some(res) = scan_token(&mut chars) {
        tokens.push(res);
    }
    tokens.into_iter()
}

fn scan_token(
    chars: &mut MultiPeek<impl Iterator<Item = (usize, char)>>,
) -> Option<Result<Token>> {
    if let Some((i, c)) = chars.next() {
        match c {
            // Invariably single char tokens
            c @ ('(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*') => {
                Some(Ok(Token::OneChar(c)))
            }
            c @ ('!' | '=' | '<' | '>') => {
                if let Some(&(_, '=')) = chars.peek() {
                    chars.next();
                    Some(Ok(Token::CharThenEqual(c)))
                } else {
                    Some(Ok(Token::OneChar(c)))
                }
            }
            '/' => {
                if let Some(&(_, '/')) = chars.peek() {
                    // another slash means we have a comment and can ignore the rest of the line after this point
                    None
                } else {
                    Some(Ok(Token::OneChar('/')))
                }
            }
            '"' => {
                let string = chars
                    .peeking_take_while(|&(_, c)| c != '"')
                    .map(|(_, c)| c)
                    .collect();
                if let Some((_, '"')) = chars.next() {
                    Some(Ok(Token::String(string)))
                } else {
                    Some(Err(anyhow!(ParseError::UnclosedString(i))))
                }
            }
            x if x.is_ascii_digit() => Some(parse_number(c, i, chars)),
            a if a == '_' || a.is_ascii_alphabetic() => Some(parse_identifier(c, i, chars)),
            w if w.is_whitespace() => scan_token(chars),
            _ => Some(Err(anyhow!(ParseError::InvalidToken(i, c)))),
        }
    } else {
        None
    }
}

fn parse_number(
    first: char,
    start: usize,
    chars: &mut MultiPeek<impl Iterator<Item = (usize, char)>>,
) -> Result<Token> {
    let mut end = start;
    while let Some((_, c)) = chars.peek() && c.is_ascii_digit() {
        // we're just cranking the index until we don't see a number here
    }

    if let Some((_, '.')) = chars.peek()
    && let Some((_, n)) = chars.peek()
    && n.is_ascii_digit() {
        while let Some((_, c)) = chars.next() && c.is_ascii_digit() {
            // we're just cranking the index until we don't see a number here
        }
    }

    if let Some((j, _)) = chars.peek() {
        // we should have advanced to the first non-number character and everything should "Just Work"
        end = *j
    }

    let string: String = std::iter::once(first)
        .chain(chars.take(end - start).map(|(_, c)| c))
        .collect();
    if let Ok(n) = string.parse::<f64>() {
        Ok(Token::Number(n))
    } else {
        Err(anyhow!(ParseError::ParseNumError(start, string)))
    }
}

fn parse_identifier(
    first: char,
    start: usize,
    chars: &mut MultiPeek<impl Iterator<Item = (usize, char)>>,
) -> Result<Token> {
    let mut end = start;
    while let Some(&(i, c)) = chars.peek() && (c.is_ascii_alphanumeric() || c == '_') {
        end = i;
    }
    let string: String = std::iter::once(first)
        .chain(chars.take(end - start).map(|(_, c)| c))
        .collect();
    if let Some(&k) = KEYWORDS.get(string.as_str()) {
        Ok(Token::Keyword(k))
    } else {
        Ok(Token::Identifier(string))
    }
}
