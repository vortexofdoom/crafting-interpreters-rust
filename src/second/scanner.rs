use anyhow::{anyhow, Result};
use enum_map::Enum;
use itertools::Itertools;
use std::iter::Peekable;

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
    Strng,
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
            Token::String(_) => Self::Strng,
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
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
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
            TokenType::Strng => "string",
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
        let s = match self {
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
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
pub enum ScanError {
    UnterminatedString,
    //ParseNumError(&'a str),
    UnrecognizedCharacter(char),
    // ClosingParen,
    // Expected(ExpectedToken, Option<Token>),
    // UnexpectedToken(Token),
    // StatementMissingExpr,
}

impl std::fmt::Display for ScanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScanError::UnterminatedString => write!(f, "unterminated string"),
            //ScanError::ParseNumError(n) => write!(f, "could not parse {n} as a number."),
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
    //let mut chars = source.char_indices();
    // weird to let user facing stuff be zero-indexed, even if it is a programming language.
    let mut line = 1;
    let mut col = 0;
    source
        .char_indices()
        .peekable()
        .batching(move |chars| {
            use Token::*;
            macro_rules! advance {
                () => {{
                    col += 1;
                    let next = chars.next();
                    if next.is_some_and(|(_, c)| c == '\n') {
                        line += 1;
                        col = 0;
                    }
                    next
                }};
            }
            loop {
                let (i, c) = advance!()?;
                let start = col;
                macro_rules! pack {
                    ($res:expr) => {
                        return Some(Parsed((line, start), $res))
                    };

                    ($c1:ident, $c2:ident) => {{
                        match chars.peek() {
                            Some((_, '=')) => {
                                advance!();
                                pack!(Ok($c1));
                            }
                            _ => pack!(Ok($c2)),
                        }
                    }};
                }
                match c {
                    '"' => {
                        while let Some((j, next)) = advance!() {
                            if next == '"' {
                                pack!(Ok(String(&source[i + 1..j])));
                            }
                        }
                        // if we reach this point it means we've reached the end of the source code
                        pack!(Err(anyhow!(ScanError::UnterminatedString)));
                    }
                    // Code specific to handling comments
                    '/' => match chars.peek() {
                        Some((_, '/')) => {
                            let curr = line;
                            while line == curr {
                                advance!()?;
                            }
                            continue;
                        }
                        _ => pack!(Ok(Slash)),
                    },
                    // One or two character tokens
                    '!' => pack!(BangEqual, Bang),
                    '=' => pack!(EqualEqual, Equal),
                    '>' => pack!(GreaterEqual, Greater),
                    '<' => pack!(LessEqual, Less),

                    // Invariably single character tokens
                    c @ ('(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*') => {
                        pack!(Ok(Token::from_char(c)?))
                    }
                    x if x.is_ascii_digit() => {
                        let mut end = i + 1;
                        while chars.peek().is_some_and(|(_, c)| c.is_ascii_digit()) {
                            advance!();
                            end += 1;
                        }

                        let mut clone = chars.clone();
                        if let Some((_, '.')) = clone.next() {
                            if clone.next().is_some_and(|(_, c)| c.is_ascii_digit()) {
                                advance!();
                                while chars.peek().is_some_and(|(_, c)| c.is_ascii_digit()) {
                                    end += 1;
                                    advance!();
                                }
                                end += 1;
                            }
                        }
                        let num = source[i..end]
                            .parse()
                            .map_err(|_| anyhow!("error parsing number {}.", &source[i..end]))
                            .map(Token::Number);
                        pack!(num);
                    }
                    a if a == '_' || a.is_ascii_alphabetic() => {
                        let mut end = i + 1;
                        while chars
                            .peek()
                            .is_some_and(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
                        {
                            end += 1;
                            advance!();
                        }

                        pack!(Ok(Token::from_str(&source[i..end])));
                    }
                    w if w.is_whitespace() => continue,
                    c => pack!(Err(anyhow!(ScanError::UnrecognizedCharacter(c)))),
                }
            }
        })
        .peekable()
}
