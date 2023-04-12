use anyhow::{anyhow, Result};
use clap::Parser;
use itertools::{Itertools, MultiPeek};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use self::ast::{parse, Precedence};

mod ast;

pub type Parsed<T> = ((usize, usize), Result<T>);

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

#[derive(Debug, PartialEq)]
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

impl Token {
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::OneChar('-' | '!' | '+' | '*' | '/' | '>' | '<') | Token::CharThenEqual(_)
        )
    }

    pub fn is_literal(&self) -> bool {
        use Keyword::*;
        matches!(
            self,
            Token::String(_)
                | Token::Number(_)
                | Token::Keyword(True)
                | Token::Keyword(False)
                | Token::Keyword(Nil)
        )
    }

    pub fn highest_valid_precedence(&self) -> Precedence {
        match self {
            Token::CharThenEqual('=' | '!') => Precedence::Equality,
            Token::CharThenEqual('<' | '>') | Token::OneChar('<' | '>') => Precedence::Comparison,
            Token::OneChar('+') => Precedence::Term,
            Token::OneChar('*' | '/') => Precedence::Factor,
            Token::OneChar('-' | '!') => Precedence::Unary,
            Token::Identifier(_) | Token::String(_) | Token::Number(_) => Precedence::Primary,
            _ => todo!(),
        }
    }

    #[rustfmt::skip]
    pub fn matches_precedence(&self, precedence: Precedence) -> bool {
        self.highest_valid_precedence() >= precedence
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnclosedString,
    ParseNumError(String),
    InvalidToken(char),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnclosedString => {
                write!(f, "end of line reached with no closing '\"' found")
            }
            ParseError::ParseNumError(s) => write!(f, "error parsing number '{s}'"),
            ParseError::InvalidToken(c) => write!(f, "invalid token '{c}'"),
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
    let tokens = scan_tokens(source);

    // for r in tokens.iter() {
    //     match r {
    //         // just printing for now
    //         Ok(t) => println!("{t}"),
    //         Err(e) => println!("{e}"),
    //     }
    // }

    let exprs = parse(&mut tokens.into_iter().multipeek());
    let (ok, err): (Vec<_>, Vec<_>) = exprs.into_iter().partition(|(_, r)| r.is_ok());

    if err.is_empty() {
        for (_, r) in ok {
            println!("{}", r.unwrap());
        }
    } else {
        for ((row, col), r) in err {
            println!("Error at line {row}, {col}: {:?}", r);
        }
    }

    Ok(())
}

fn scan_tokens(source: &str) -> Vec<Parsed<Token>> {
    source
        .lines()
        .enumerate()
        .flat_map(|(i, l)| {
            let (tokens, errors): (Vec<_>, Vec<_>) = scan_line(l)
                .map(|(j, r)| ((i, j), r))
                .partition(|(_, r)| r.is_ok());
            if errors.is_empty() {
                tokens
            } else {
                errors
                    .into_iter()
                    .filter_map(|r| if let Err(_) = r.1 { Some(r) } else { None })
                    .collect()
            }
        })
        .collect()
}

fn scan_line(line: &str) -> impl Iterator<Item = (usize, Result<Token>)> {
    let mut chars = line.char_indices().map(|(i, c)| (i + 1, c)).multipeek();
    let mut tokens = vec![];
    while let Some(res) = scan_token(&mut chars) {
        tokens.push(res);
    }
    tokens.into_iter()
}

fn scan_token(
    chars: &mut MultiPeek<impl Iterator<Item = (usize, char)>>,
) -> Option<(usize, Result<Token>)> {
    if let Some((i, c)) = chars.next() {
        match c {
            // Invariably single char tokens
            '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => {
                Some((i, Ok(Token::OneChar(c))))
            }
            '!' | '=' | '<' | '>' => {
                if let Some(&(_, '=')) = chars.peek() {
                    chars.next();
                    Some((i, Ok(Token::CharThenEqual(c))))
                } else {
                    Some((i, Ok(Token::OneChar(c))))
                }
            }
            '/' => {
                if let Some(&(_, '/')) = chars.peek() {
                    // another slash means we have a comment and can ignore the rest of the line after this point
                    None
                } else {
                    Some((i, Ok(Token::OneChar('/'))))
                }
            }
            '"' => {
                let string = chars
                    .peeking_take_while(|&(_, c)| c != '"')
                    .map(|(_, c)| c)
                    .collect();
                if let Some((_, '"')) = chars.next() {
                    Some((i, Ok(Token::String(string))))
                } else {
                    Some((i, Err(anyhow!(ParseError::UnclosedString))))
                }
            }
            x if x.is_ascii_digit() => Some(parse_number(i, c, chars)),
            a if a == '_' || a.is_ascii_alphabetic() => Some(parse_identifier(i, c, chars)),
            w if w.is_whitespace() => scan_token(chars),
            _ => Some((i, Err(anyhow!(ParseError::InvalidToken(c))))),
        }
    } else {
        None
    }
}

fn parse_number(
    start: usize,
    first: char,
    chars: &mut MultiPeek<impl Iterator<Item = (usize, char)>>,
) -> (usize, Result<Token>) {
    let mut end = start;
    let prefix = chars
        .peeking_take_while(|(_, c)| c.is_ascii_digit())
        .map(|(_, c)| c);
    let mut string: String = std::iter::once(first).chain(prefix).collect();
    chars.reset_peek();

    if let Some((_, '.')) = chars.peek() {
        while let Some((i, n)) = chars.peek()
        && n.is_ascii_digit() {
            end = *i;
        }
    }
    string.extend(chars.peeking_take_while(|(i, _)| *i < end).map(|(_, c)| c));

    if let Ok(n) = string.parse::<f64>() {
        (start, Ok(Token::Number(n)))
    } else {
        (start, Err(anyhow!(ParseError::ParseNumError(string))))
    }
}

fn parse_identifier(
    start: usize,
    first: char,
    chars: &mut MultiPeek<impl Iterator<Item = (usize, char)>>,
) -> (usize, Result<Token>) {
    let mut end = start;
    while let Some(&(i, c)) = chars.peek() && (c.is_ascii_alphanumeric() || c == '_') {
        end = i;
    }
    let string: String = std::iter::once(first)
        .chain(chars.take(end - start).map(|(_, c)| c))
        .collect();
    if let Some(&k) = KEYWORDS.get(string.as_str()) {
        (start, Ok(Token::Keyword(k)))
    } else {
        (start, Ok(Token::Identifier(string)))
    }
}
