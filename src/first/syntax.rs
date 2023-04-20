use anyhow::{anyhow, Result};
use itertools::Either;

use crate::first::parser::ParseError;

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
    pub fn is_binary_operator(&self) -> bool {
        use Keyword::*;
        matches!(
            self,
            Token::OneChar('-' | '!' | '+' | '*' | '/' | '>' | '<')
                | Token::CharThenEqual(_)
                | Token::Keyword(Or | And)
        )
    }

    pub fn is_unary_operator(&self) -> bool {
        matches!(self, Token::OneChar('-' | '!'))
    }

    pub fn try_convert_literal(self) -> Either<LoxVal, Self> {
        use itertools::Either::*;
        use Keyword::*;
        match self {
            Token::String(s) => Left(LoxVal::String(s)),
            Token::Number(n) => Left(LoxVal::Number(n)),
            Token::Keyword(True) => Left(LoxVal::Boolean(true)),
            Token::Keyword(False) => Left(LoxVal::Boolean(false)),
            Token::Keyword(Nil) => Left(LoxVal::Nil),
            _ => Right(self),
        }
    }

    pub fn highest_valid_precedence(&self) -> Precedence {
        match self {
            Token::Keyword(Keyword::Or) => Precedence::Or,
            Token::Keyword(Keyword::And) => Precedence::Or,
            Token::CharThenEqual('=' | '!') => Precedence::Equality,
            Token::CharThenEqual('<' | '>') | Token::OneChar('<' | '>') => Precedence::Comparison,
            Token::OneChar('+') => Precedence::Term,
            Token::OneChar('*' | '/') => Precedence::Factor,
            Token::OneChar('-' | '!') => Precedence::Unary,
            Token::Identifier(_) | Token::String(_) | Token::Number(_) => Precedence::Primary,
            _ => todo!(),
        }
    }

    pub fn matches_precedence(&self, precedence: Precedence) -> bool {
        self.highest_valid_precedence() >= precedence
    }
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

impl Keyword {
    pub fn try_from_str(s: &str) -> Option<Self> {
        use Keyword::*;
        match s {
            "and" => Some(And),
            "class" => Some(Class),
            "else" => Some(Else),
            "false" => Some(False),
            "fun" => Some(Fun),
            "for" => Some(For),
            "if" => Some(If),
            "nil" => Some(Nil),
            "or" => Some(Or),
            "print" => Some(Print),
            "return" => Some(Return),
            "super" => Some(Super),
            "this" => Some(This),
            "true" => Some(True),
            "var" => Some(Var),
            "while" => Some(While),
            _ => None,
        }
    }
}

/// Simple wrapper type for Lox values
/// So that we can take advantage of Rust's type system and built in behavior when possible
/// But have the freedom to diverge and customize where necessary
#[derive(Debug, Clone)]
pub enum LoxVal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl LoxVal {
    /// Lox's rules for conversion into boolean values are simple: False and Nil are falsey, any other value (even 0) is truthy
    pub fn truthy(&self) -> bool {
        // might want to make this return a LoxVal::Boolean
        match self {
            LoxVal::String(_) => true,
            LoxVal::Number(_) => true,
            LoxVal::Boolean(b) => *b,
            LoxVal::Nil => false,
        }
    }
}

impl std::fmt::Display for LoxVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxVal::String(s) => write!(f, "{s}"),
            LoxVal::Number(n) => write!(f, "{n}"),
            LoxVal::Boolean(_) => todo!(),
            LoxVal::Nil => todo!(),
        }
    }
}

/// Addition
/// If either operand is a string, the remaining operand (if not already a string) is concatenated along with it from left to right as a string
/// Otherwise, both operands must be numbers.
impl std::ops::Add<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn add(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::String(_), _) | (_, LoxVal::String(_)) => {
                Ok(LoxVal::String(format!("{self}{rhs}")))
            }
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x + y)),
            _ => Err(anyhow!("cannot add {self} and {rhs}")),
        }
    }
}

// Subtraction
impl std::ops::Sub<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn sub(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x - y)),
            _ => Err(anyhow!("cannot subtract {rhs} from {self}")),
        }
    }
}

// Multiplication
impl std::ops::Mul<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn mul(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x * y)),
            _ => Err(anyhow!("cannot multiply {self} and {rhs}")),
        }
    }
}

// Division
impl std::ops::Div<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn div(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x / y)),
            _ => Err(anyhow!("cannot divide {self} and {rhs}")),
        }
    }
}

/// Equality operators can be called freely on Lox values
/// But they can only possibly be deemed equal if they are of the same type
impl PartialEq for LoxVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxVal::Boolean(l), LoxVal::Boolean(r)) => l == r,
            (LoxVal::Number(l), LoxVal::Number(r)) => l == r,
            (LoxVal::String(l), LoxVal::String(r)) => l == r,
            (LoxVal::Nil, LoxVal::Nil) => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for LoxVal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LoxVal::Number(x), LoxVal::Number(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Precedence {
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Primary,
}

impl Precedence {
    pub fn next_highest(&self) -> Self {
        match self {
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Option<Token>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    Variable(String),
    Literal(LoxVal),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(t) => write!(f, "{t}"),
            Expr::Grouping(e) => write!(f, "{e}"),
            Expr::Binary(l, op, r) => write!(f, "({l} {op} {r})"),
            Expr::Variable(s) => write!(f, "{s}"),
            Expr::Assignment(l, r) => write!(f, "{l} = {r}"),
            Expr::Unary(op, r) => {
                if let Some(op) = op {
                    write!(f, "({op}{r})")
                } else {
                    write!(f, "{r}",)
                }
            }
        }
    }
}

impl Expr {
    // Constructors to avoid Box::new() for every constructed expression
    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }
    pub fn unary(operator: Option<Token>, right: Expr) -> Self {
        Self::Unary(operator, Box::new(right))
    }
    pub fn group(self) -> Self {
        Self::Grouping(Box::new(self))
    }
}

#[derive(Debug)]
pub enum Statement {
    Declaration(String, Option<Expr>),
    Expression(Expr),
    Print(Expr),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(e) => write!(f, "Expr: {e}"),
            Self::Print(e) => write!(f, "Print: {e}"),
            Self::Declaration(name, assign) => {
                if let Some(expr) = assign {
                    write!(f, "Var {name} = {expr}")
                } else {
                    write!(f, "Var {name}")
                }
            }
            _ => todo!(),
        }
    }
}
