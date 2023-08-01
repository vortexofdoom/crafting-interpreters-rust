use std::{iter::Peekable, ops::Index};

use anyhow::{anyhow, Result};
use enum_map::{Enum, EnumMap};

use super::{
    chunk::{Chunk, OpCode},
    scanner::{scan, Parsed, Token, TokenType},
    value::Value,
    InterpretError,
};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn next_highest(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::Primary,
        }
    }
}

type ParseFn<'a, T> = Option<fn(&mut Parser<'a, T>) -> Result<()>>;

#[derive(Debug, Clone, Copy)]
struct ParseRule<'a, T: Iterator<Item = Parsed<Token<'a>>>>(
    ParseFn<'a, T>,
    ParseFn<'a, T>,
    Precedence,
);

#[rustfmt::skip]
impl<'a, T: Iterator<Item = Parsed<Token<'a>>>> ParseRule<'a, T> {
    const LEFT_PAREN: Self      = Self(Some(Parser::grouping), None, Precedence::None);
    const RIGHT_PAREN: Self     = Self(None, None, Precedence::None);
    const LEFT_BRACE: Self      = Self(None, None, Precedence::None);
    const RIGHT_BRACE: Self     = Self(None, None, Precedence::None);
    const COMMA: Self           = Self(None, None, Precedence::None);
    const DOT: Self             = Self(None, None, Precedence::None);
    const MINUS: Self           = Self(Some(Parser::unary), Some(Parser::binary), Precedence::Term);
    const PLUS: Self            = Self(None, Some(Parser::binary), Precedence::Term);
    const SEMICOLON: Self       = Self(None, None, Precedence::None);
    const SLASH: Self           = Self(None, Some(Parser::binary), Precedence::Factor);
    const STAR: Self            = Self(None, Some(Parser::binary), Precedence::Factor);
    const BANG: Self            = Self(Some(Parser::unary), None, Precedence::None);
    const BANG_EQUAL: Self      = Self(None, Some(Parser::binary), Precedence::Comparison);
    const EQUAL: Self           = Self(None, None, Precedence::None);
    const EQUAL_EQUAL: Self     = Self(None, Some(Parser::binary), Precedence::Comparison);
    const GREATER: Self         = Self(None, Some(Parser::binary), Precedence::Comparison);
    const GREATER_EQUAL: Self   = Self(None, Some(Parser::binary), Precedence::Comparison);
    const LESS: Self            = Self(None, Some(Parser::binary), Precedence::Comparison);
    const LESS_EQUAL: Self      = Self(None, Some(Parser::binary), Precedence::Comparison);
    const IDENTIFIER: Self      = Self(None, None, Precedence::None);
    const STRING: Self          = Self(None, None, Precedence::None);
    const NUMBER: Self          = Self(Some(Parser::number), None, Precedence::None);
    const AND: Self             = Self(None, None, Precedence::None);
    const CLASS: Self           = Self(None, None, Precedence::None);
    const ELSE: Self            = Self(None, None, Precedence::None);
    const FALSE: Self           = Self(Some(Parser::literal), None, Precedence::None);
    const FOR: Self             = Self(None, None, Precedence::None);
    const FUN: Self             = Self(None, None, Precedence::None);
    const IF: Self              = Self(None, None, Precedence::None);
    const NIL: Self             = Self(Some(Parser::literal), None, Precedence::None);
    const OR: Self              = Self(None, None, Precedence::None);
    const PRINT: Self           = Self(None, None, Precedence::None);
    const RETURN: Self          = Self(None, None, Precedence::None);
    const SUPER: Self           = Self(None, None, Precedence::None);
    const THIS: Self            = Self(None, None, Precedence::None);
    const TRUE: Self            = Self(Some(Parser::literal), None, Precedence::None);
    const VAR: Self             = Self(None, None, Precedence::None);
    const WHILE: Self           = Self(None, None, Precedence::None);

    const RULES: [Self; 38] = [
        ParseRule::LEFT_PAREN,
        ParseRule::RIGHT_PAREN,
        ParseRule::LEFT_BRACE,
        ParseRule::RIGHT_BRACE,
        ParseRule::COMMA,
        ParseRule::DOT,
        ParseRule::MINUS,
        ParseRule::PLUS,
        ParseRule::SEMICOLON,
        ParseRule::SLASH,
        ParseRule::STAR,
        ParseRule::BANG,
        ParseRule::BANG_EQUAL,
        ParseRule::EQUAL,
        ParseRule::EQUAL_EQUAL,
        ParseRule::GREATER,
        ParseRule::GREATER_EQUAL,
        ParseRule::LESS,
        ParseRule::LESS_EQUAL,
        ParseRule::IDENTIFIER,
        ParseRule::STRING,
        ParseRule::NUMBER,
        ParseRule::AND,
        ParseRule::CLASS,
        ParseRule::ELSE,
        ParseRule::FALSE,
        ParseRule::FOR,
        ParseRule::FUN,
        ParseRule::IF,
        ParseRule::NIL,
        ParseRule::OR,
        ParseRule::PRINT,
        ParseRule::RETURN,
        ParseRule::SUPER,
        ParseRule::THIS,
        ParseRule::TRUE,
        ParseRule::VAR,
        ParseRule::WHILE,
    ];

    #[inline]
    fn prefix(token: Token) -> ParseFn<'a, T> {
        Self::RULES[TokenType::from(token).into_usize()].0
    }

    #[inline]
    fn infix(token: Token) -> ParseFn<'a, T> {
        Self::RULES[TokenType::from(token).into_usize()].1
    }

    #[inline]
    fn precedence(token: Token) -> Precedence {
        Self::RULES[TokenType::from(token).into_usize()].2
    }
}

// struct ErrReport {
//     line: usize,
//     column: usize,
//     error: &'static str,
// }

// impl std::fmt::Display for ErrReport {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "Error at {}, {}: {}", self.line, self.column, self.error)
//     }
// }

struct Parser<'a, T: Iterator<Item = Parsed<Token<'a>>>> {
    chunk: Chunk,
    prev: Option<(usize, Token<'a>)>,
    tokens: Peekable<T>,
}

// Can probably do everything with peek() instead of keeping the previous token, but we'll find out
impl<'a, T: Iterator<Item = Parsed<Token<'a>>>> Parser<'a, T> {
    fn consume(&mut self, token: Token<'_>) -> Result<()> {
        match self.tokens.peek() {
            Some(Parsed(_, Ok(t))) if *t == token => self.advance(),
            _ => Err(anyhow!("expected {token}")),
        }
    }

    fn current(&mut self) -> Option<Result<Token>> {
        match self.tokens.peek() {
            Some(Parsed(_, Ok(t))) => Some(Ok(t.clone())),
            Some(Parsed((l, c), Err(e))) => Some(Err(anyhow!("Error at {l}, {c}: {e}"))),
            None => None,
        }
    }

    fn previous(&self) -> Token {
        self.prev
            .expect("should only call previous when there's a token")
            .1
    }

    fn emit_byte(&mut self, byte: impl Into<u8>) {
        self.chunk
            .write(byte, self.prev.expect("only emit bytes from a token").0);
    }

    fn advance(&mut self) -> Result<()> {
        let parsed = self.tokens.next();
        match parsed {
            Some(Parsed((l, c), Err(e))) => {
                while let Some(Parsed(_, Err(_))) = self.tokens.peek() {
                    self.tokens.next();
                }
                Err(anyhow!("Error at {l}, {c}: {e}"))
            }
            _ => {
                self.prev = parsed.map(|Parsed((l, _), res)| (l, res.unwrap()));
                Ok(())
            }
        }
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn unary(&mut self) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call unary when there is a token");
        self.parse_precedence(Precedence::Unary)?;
        let op = match token {
            Token::Minus => OpCode::Negate,
            Token::Bang => OpCode::Not,
            _ => return Err(anyhow!("{token} is not a valid unary operator!")),
        };
        self.chunk.write(op, line);
        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call binary when there is a token");
        self.parse_precedence(ParseRule::<T>::precedence(token).next_highest())?;
        match token {
            Token::Minus => self.chunk.write(OpCode::Subtract, line),
            Token::Plus => self.chunk.write(OpCode::Add, line),
            Token::Slash => self.chunk.write(OpCode::Divide, line),
            Token::Star => self.chunk.write(OpCode::Multiply, line),
            Token::BangEqual => {
                self.chunk.write(OpCode::Equal, line);
                self.chunk.write(OpCode::Not, line);
            }
            Token::EqualEqual => self.chunk.write(OpCode::Equal, line),
            Token::Greater => self.chunk.write(OpCode::Greater, line),
            Token::GreaterEqual => self.chunk.write(OpCode::GreaterEqual, line),
            Token::Less => self.chunk.write(OpCode::Less, line),
            Token::LessEqual => self.chunk.write(OpCode::LessEqual, line),
            _ => return Err(anyhow!("{token} is not a valid binary operator!")),
        }
        Ok(())
    }

    fn number(&mut self) -> Result<()> {
        let (line, Token::Number(n)) = self.prev.expect("only call on numbers") else { unreachable!() };
        self.chunk.write(OpCode::Constant, line);
        let constant = self.chunk.add_constant(Value::Number(n));
        self.chunk.write(constant, line);
        Ok(())
    }

    fn literal(&mut self) -> Result<()> {
        match self.prev {
            Some((i, Token::False)) => self.chunk.write(OpCode::False, i),
            Some((i, Token::Nil)) => self.chunk.write(OpCode::Nil, i),
            Some((i, Token::True)) => self.chunk.write(OpCode::True, i),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::RightParen)?;
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;
        if let Some((_, token)) = self.prev {
            match ParseRule::<T>::prefix(token) {
                Some(prefix_rule) => prefix_rule(self)?,
                None => return Err(anyhow!("expect expression.")),
            }

            while let Some(res) = self.current()
            && ParseRule::<T>::precedence(res?) >= precedence {
                match self.current() {
                    Some(Ok(_)) => {
                        self.advance()?;
                        ParseRule::infix(self.previous()).expect("should be an infix rule.")(self)?;
                    }
                    Some(Err(e)) => return Err(e),
                    None => break,
                }
            }
        }
        Ok(())
    }
}

pub fn compile(source: &str) -> Result<Chunk> {
    for token in scan(source) {
        println!("Token: {token}");
    }
    let mut parser = Parser {
        chunk: Chunk::new(),
        prev: None,
        tokens: scan(source),
    };

    parser.expression()?;
    parser.chunk.write(OpCode::Return, 0);
    Ok(parser.chunk)
}
