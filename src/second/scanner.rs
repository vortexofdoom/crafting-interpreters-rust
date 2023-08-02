use anyhow::{anyhow, Result};
use enum_map::Enum;
use itertools::Itertools;
use num_enum::{FromPrimitive, IntoPrimitive};
use std::{
    iter::{Enumerate, Peekable},
    str::{Bytes, CharIndices, Lines},
};

use super::value::Value;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Token<'a> {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier(&'a str),
    String(&'a str),
    Number(f64),
    // Keywords
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

#[derive(Debug, Clone, Copy, PartialEq, Enum, PartialOrd, Eq, Ord)]
#[repr(u8)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String,
    Number,
    // Keywords
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

impl From<Token<'_>> for TokenType {
    fn from(value: Token<'_>) -> Self {
        match value {
            Token::LeftParen => Self::LeftParen,
            Token::RightParen => Self::RightParen,
            Token::LeftBrace => Self::LeftBrace,
            Token::RightBrace => Self::RightBrace,
            Token::Comma => Self::Comma,
            Token::Dot => Self::Dot,
            Token::Minus => Self::Minus,
            Token::Plus => Self::Plus,
            Token::Semicolon => Self::Semicolon,
            Token::Slash => Self::Slash,
            Token::Star => Self::Star,
            Token::Bang => Self::Bang,
            Token::BangEqual => Self::BangEqual,
            Token::Equal => Self::Equal,
            Token::EqualEqual => Self::EqualEqual,
            Token::Greater => Self::Greater,
            Token::GreaterEqual => Self::GreaterEqual,
            Token::Less => Self::Less,
            Token::LessEqual => Self::LessEqual,
            Token::Identifier(_) => Self::Identifier,
            Token::String(_) => Self::String,
            Token::Number(_) => Self::Number,
            Token::And => Self::And,
            Token::Class => Self::Class,
            Token::Else => Self::Else,
            Token::False => Self::False,
            Token::Fun => Self::Fun,
            Token::For => Self::For,
            Token::If => Self::If,
            Token::Nil => Self::Nil,
            Token::Or => Self::Or,
            Token::Print => Self::Print,
            Token::Return => Self::Return,
            Token::Super => Self::Super,
            Token::This => Self::This,
            Token::True => Self::True,
            Token::Var => Self::Var,
            Token::While => Self::While,
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{{",
            TokenType::RightBrace => "}}",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Minus => "-",
            TokenType::Plus => "+",
            TokenType::Semicolon => ";",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Greater => ">",
            TokenType::GreaterEqual => ">=",
            TokenType::Less => "<",
            TokenType::LessEqual => "<=",
            TokenType::Identifier => "identifier",
            TokenType::String => "string",
            TokenType::Number => "number",
            TokenType::And => "and",
            TokenType::Class => "class",
            TokenType::Else => "else",
            TokenType::False => "false",
            TokenType::Fun => "fun",
            TokenType::For => "for",
            TokenType::If => "if",
            TokenType::Nil => "nil",
            TokenType::Or => "or",
            TokenType::Print => "print",
            TokenType::Return => "return",
            TokenType::Super => "super",
            TokenType::This => "this",
            TokenType::True => "true",
            TokenType::Var => "var",
            TokenType::While => "while",
        };

        write!(f, "{s}")
    }
}

impl PartialEq<Token<'_>> for Token<'_> {
    fn eq(&self, other: &Token) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
    }
}

impl PartialEq<TokenType> for Token<'_> {
    fn eq(&self, other: &TokenType) -> bool {
        let token_type = TokenType::from(*self);
        token_type == *other
    }    
}

impl<'a> Token<'a> {
    fn from_char(c: char) -> Option<Self> {
        match c {
            '(' => Some(Self::LeftParen),
            ')' => Some(Self::RightParen),
            '{' => Some(Self::LeftBrace),
            '}' => Some(Self::RightBrace),
            ',' => Some(Self::Comma),
            '.' => Some(Self::Dot),
            '-' => Some(Self::Minus),
            '+' => Some(Self::Plus),
            ';' => Some(Self::Semicolon),
            '*' => Some(Self::Star),
            '/' => Some(Self::Slash),
            _ => None,
        }
    }

    fn from_str(source: &'a str) -> Self {
        let check = |s: &str| source.len() == s.len() + 1 && &source[1..] == s;
        let mut chars = source.bytes();
        match chars.next().unwrap() as char {
            'a' if check("nd") => Token::And,
            'c' if check("lass") => Token::Class,
            'e' if check("lse") => Token::Else,
            'i' if check("f") => Token::If,
            'n' if check("il") => Token::Nil,
            'o' if check("r") => Token::Or,
            'p' if check("rint") => Token::Print,
            'r' if check("eturn") => Token::Return,
            's' if check("uper") => Token::Super,
            'v' if check("ar") => Token::Var,
            'w' if check("hile") => Token::While,
            'f' if check("alse") => Token::False,
            'f' if check("or") => Token::For,
            'f' if check("un") => Token::Fun,
            't' if check("his") => Token::This,
            't' if check("rue") => Token::True,
            _ => Token::Identifier(source),
        }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: DELET THIS
        //write!(f, "{:?}: ", std::mem::discriminant(self));
        let s = match self {
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{{",
            Token::RightBrace => "}}",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Semicolon => ";",
            Token::Slash => "/",
            Token::Star => "*",
            Token::Bang => "!",
            Token::BangEqual => "!=",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::Greater => ">",
            Token::GreaterEqual => ">=",
            Token::Less => "<",
            Token::LessEqual => "<=",
            Token::Identifier(n) => return write!(f, "{n}"),
            Token::String(s) => return write!(f, "\"{s}\""),
            Token::Number(n) => return write!(f, "{n}"),
            Token::And => "and",
            Token::Class => "class",
            Token::Else => "else",
            Token::False => "false",
            Token::Fun => "fun",
            Token::For => "for",
            Token::If => "if",
            Token::Nil => "nil",
            Token::Or => "or",
            Token::Print => "print",
            Token::Return => "return",
            Token::Super => "super",
            Token::This => "this",
            Token::True => "true",
            Token::Var => "var",
            Token::While => "while",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub enum ScanError<'a> {
    UnterminatedString,
    ParseNumError(&'a str),
    UnrecognizedCharacter(char),
    // ClosingParen,
    // Expected(ExpectedToken, Option<Token>),
    // UnexpectedToken(Token),
    // StatementMissingExpr,
}

impl std::fmt::Display for ScanError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScanError::UnterminatedString => write!(f, "unterminated string"),
            ScanError::ParseNumError(n) => todo!(),
            ScanError::UnrecognizedCharacter(c) => write!(f, "unrecognized character '{c}'"),
        }
    }
}

#[derive(Debug)]
pub struct Parsed<T>(pub (usize, usize), pub Result<T>);

impl<T: std::fmt::Display> std::fmt::Display for Parsed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.1 {
            Ok(t) => write!(f, "({}, {}) {t}", self.0 .0, self.0 .1),
            Err(e) => write!(f, "Error at ({}, {}): {e}", self.0 .0, self.0 .1),
        }
    }
}

impl<T, U> PartialEq<U> for Parsed<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &U) -> bool {
        match self.1.as_ref() {
            Ok(t) => t == other,
            _ => false,
        }
    }
}

pub fn scan(source: &str) -> Peekable<impl Iterator<Item = Parsed<Token>>> {
    source
        .lines()
        .enumerate()
        .flat_map(|(i, line)| scan_line(line).map(move |(j, t)| Parsed((i, j), t)))
        .peekable()
}

struct LineScanner<'a> {
    line: &'a str,
    curr_byte: usize,
}

impl<'a> Iterator for LineScanner<'a> {
    type Item = (usize, Result<Token<'a>>);

    fn next(&mut self) -> Option<Self::Item> {
        let pack = |i, t: Token<'a>| Some((i, Ok(t)));

        //let chars = self.source.as_bytes()[self.curr..].iter().map(|b| *b as char);
        let c = self.advance()?;
        let i = self.curr_byte;
        if i > self.line.len() {
            return None;
        }
        match c {
            '"' => {
                let start = self.curr_byte;
                while !self.is_at_end() {
                    if self.advance()? == '"' {
                        return pack(start, Token::String(&self.line[start..self.curr_byte - 1]));
                    }
                }
                // if we reach this point it means we've reached the end of the line
                Some((start, Err(anyhow!(ScanError::UnterminatedString))))
            }
            // Code specific to handling comments
            '/' => match self.peek() {
                Some('/') => None,
                _ => pack(self.curr_byte, Token::Slash),
            },
            // One or two character tokens
            '!' => pack(i, self.consume_if_eq(Token::Bang)),
            '=' => pack(i, self.consume_if_eq(Token::Equal)),
            '>' => pack(i, self.consume_if_eq(Token::Greater)),
            '<' => pack(i, self.consume_if_eq(Token::Less)),

            // Invariably single character tokens
            c @ ('(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*') => {
                pack(i, Token::from_char(c)?)
            }
            x if x.is_ascii_digit() => Some((i, self.parse_number())),
            a if a == '_' || a.is_ascii_alphabetic() => pack(i, self.parse_word()),
            w if w.is_whitespace() => self.next(),
            c => Some((i, Err(anyhow!(ScanError::UnrecognizedCharacter(c))))),
        }
    }
}

fn scan_line(source: &str) -> LineScanner {
    LineScanner {
        line: source,
        curr_byte: 0,
    }
}

impl<'a> LineScanner<'a> {
    pub fn init(source: &'a str) -> Self {
        Self {
            line: source,
            curr_byte: 0,
        }
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        if self.curr_byte >= self.line.len() {
            return None;
        }
        Some(self.line.as_bytes()[self.curr_byte] as char)
    }

    #[inline]
    fn advance(&mut self) -> Option<char> {
        self.curr_byte += 1;
        if self.curr_byte > self.line.len() {
            return None;
        }
        Some(self.line.as_bytes()[self.curr_byte - 1] as char)
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.curr_byte >= self.line.len()
    }

    fn consume_if_eq(&mut self, token: Token<'a>) -> Token<'a> {
        if self.peek() == Some('=') {
            self.advance();
            match token {
                Token::Bang => Token::BangEqual,
                Token::Equal => Token::EqualEqual,
                Token::Greater => Token::GreaterEqual,
                Token::Less => Token::LessEqual,
                _ => unreachable!(),
            }
        } else {
            token
        }
    }

    fn parse_number(&mut self) -> Result<Token<'a>> {
        let start = self.curr_byte - 1;

        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        if self.peek() == Some('.') {
            println!("found '.'");
            if self.line.as_bytes()[self.curr_byte].is_ascii_digit() {
                self.advance();
                while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    self.advance();
                }
            }
        }
        let num_str = &self.line[start..self.curr_byte];
        num_str
            .trim()
            .parse()
            .map_err(|_| anyhow!("error parsing number {}.", num_str))
            .map(Token::Number)
    }

    // Parses a string of characters that can begin with a letter or '_', and determines whether it is an keyword or an identifier
    // Returns a `Token` as this will always be valid, since we start with one valid character (which is a valid identifier alone) and stop at the first invalid character
    // validating the identifier itself comes in the next pass.
    fn parse_word(&mut self) -> Token<'a> {
        let start = self.curr_byte - 1;
        while self
            .peek()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        {
            self.advance();
        }

        Token::from_str(&self.line[start..self.curr_byte])
    }
}
