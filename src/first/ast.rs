use std::fmt::Display;

use anyhow::{anyhow, Result};
use itertools::{MultiPeek, PeekingNext};

use super::{Parsed, Token};

#[derive(Debug)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Token),
}

#[derive(Debug)]
pub enum AstError {
    Closing,
    UnexpectedToken(Token),
}

impl Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstError::Closing => write!(f, "missing closing ')'"),
            AstError::UnexpectedToken(t) => write!(f, "unexpected token {t}"),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(t) => write!(f, "{t}"),
            Expr::Grouping(e) => write!(f, "{e}"),
            Expr::Binary(b) => write!(f, "({} {} {})", b.left, b.operator, b.right),
            Expr::Unary(u) => {
                if let Some(op) = &u.operator {
                    write!(f, "({}{})", op, u.right)
                } else {
                    write!(f, "{}", u.right)
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    left: Expr,
    operator: Token,
    right: Expr,
}

impl BinaryExpr {
    pub fn evaluate(self) -> Result<Value> {
        match self.operator {
            Token::OneChar('+') => self.left.evaluate()? + self.right.evaluate()?,
            Token::OneChar('-') => self.left.evaluate()? - self.right.evaluate()?,
            Token::OneChar('*') => self.left.evaluate()? * self.right.evaluate()?,
            Token::OneChar('/') => self.left.evaluate()? / self.right.evaluate()?,
            Token::CharThenEqual('=') => Ok(Value::Boolean(
                self.left.evaluate()? == self.right.evaluate()?,
            )),
            Token::CharThenEqual('!') => Ok(Value::Boolean(
                self.left.evaluate()? != self.right.evaluate()?,
            )),
            Token::CharThenEqual('>') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x >= y)),
                (Value::Number(_), val) | (val, Value::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('<') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x <= y)),
                (Value::Number(_), val) | (val, Value::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            _ => Err(anyhow!("{} is not a valid binary operator", self.operator)),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Value {
    fn truthy(&self) -> bool {
        match self {
            Value::String(_) => true,
            Value::Number(_) => true,
            Value::Boolean(b) => *b,
            Value::Nil => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Boolean(_) => todo!(),
            Value::Nil => todo!(),
        }
    }
}

impl std::ops::Add<Value> for Value {
    type Output = Result<Value>;
    fn add(self, rhs: Value) -> Self::Output {
        match (&self, &rhs) {
            (Value::String(_), _) | (_, Value::String(_)) => {
                Ok(Value::String(format!("{self}{rhs}")))
            }
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
            _ => Err(anyhow!("cannot add {self} and {rhs}")),
        }
    }
}

impl std::ops::Sub<Value> for Value {
    type Output = Result<Value>;
    fn sub(self, rhs: Value) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
            _ => Err(anyhow!("cannot subtract {rhs} from {self}")),
        }
    }
}

impl std::ops::Mul<Value> for Value {
    type Output = Result<Value>;
    fn mul(self, rhs: Value) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
            _ => Err(anyhow!("cannot multiply {self} and {rhs}")),
        }
    }
}

impl std::ops::Div<Value> for Value {
    type Output = Result<Value>;
    fn div(self, rhs: Value) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
            _ => Err(anyhow!("cannot divide {self} and {rhs}")),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    operator: Option<Token>,
    right: Expr,
}

impl UnaryExpr {
    pub fn evaluate(self) -> Result<Value> {
        if let Some(t) = self.operator {
            match (t, self.right.evaluate()?) {
                (Token::OneChar('!'), o) => Ok(Value::Boolean(!o.truthy())),
                (Token::OneChar('-'), Value::Number(n)) => Ok(Value::Number(-n)),
                (Token::OneChar('-'), o) => Err(anyhow!("cannot negate {o}")),
                (t, _) => Err(anyhow!("{t} is not a valid unary operator")),
            }
        } else {
            self.right.evaluate()
        }
    }
}

impl Expr {
    fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Binary(Box::new(BinaryExpr {
            left,
            operator,
            right,
        }))
    }

    fn unary(operator: Option<Token>, right: Expr) -> Self {
        Self::Unary(Box::new(UnaryExpr { operator, right }))
    }

    fn group(self) -> Self {
        Self::Grouping(Box::new(self))
    }

    pub fn evaluate(self) -> Result<Value> {
        use super::Keyword::*;
        match self {
            Expr::Grouping(e) => e.evaluate(),
            Expr::Binary(b) => b.evaluate(),
            Expr::Unary(u) => u.evaluate(),
            Expr::Literal(o) => match o {
                Token::String(s) => Ok(Value::String(s)),
                Token::Keyword(True) => Ok(Value::Boolean(true)),
                Token::Keyword(False) => Ok(Value::Boolean(false)),
                Token::Keyword(Nil) => Ok(Value::Nil),
                Token::Number(n) => Ok(Value::Number(n)),
                _ => todo!(),
            },
        }
    }
}

pub fn parse(tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>) -> Vec<Parsed<Expr>> {
    let mut expressions = vec![];
    while let Some(expr) = parse_expr(tokens) {
        expressions.push(expr);
    }
    expressions
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Precedence {
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Primary,
}

impl Precedence {
    fn next_highest(&self) -> Self {
        match self {
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

fn parse_expr(tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>) -> Option<Parsed<Expr>> {
    parse_binary_expr(Precedence::Equality, tokens)
}

fn parse_binary_expr(
    precedence: Precedence,
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Expr>> {
    if precedence < Precedence::Unary {
        parse_binary_expr(precedence.next_highest(), tokens).map(|res| {
            if let (lc, Ok(mut expr)) = res {
                while let Some((_, Ok(op))) = tokens.peeking_next(|(_, r)| {
                    if let Ok(t) = r {
                        t.is_operator() && t.matches_precedence(precedence)
                    } else {
                        false
                    }
                }) {
                    match parse_binary_expr(precedence.next_highest(), tokens) {
                        Some((_, Ok(right))) => expr = Expr::binary(expr, op, right),
                        Some(err) => return err,
                        None => break,
                    }
                }
                (lc, Ok(expr))
            } else {
                res
            }
        })
    } else {
        parse_unary_expr(tokens)
    }
}

fn parse_unary_expr(
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Expr>> {
    if let Some((lc, Ok(op))) = tokens
        .peeking_next(|(_, r)| if let Ok(t) = r {
            t.is_operator() && t.matches_precedence(Precedence::Unary)
        } else {
            false
        })
    && let Some((_, Ok(right))) = parse_unary_expr(tokens) {
        Some((lc, Ok(Expr::unary(Some(op), right))))
    } else {
        match parse_primary_expr(tokens) {
            Some((lc, Ok(expr))) => Some((lc, Ok(Expr::unary(None, expr)))),
            Some(err) => Some(err),
            None => None,
        }
    }
}

fn parse_primary_expr(
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Expr>> {
    if let Some((lc, Ok(t))) = tokens.next() {
        if t.is_literal() {
            Some((lc, Ok(Expr::Literal(t))))
        } else if t == Token::OneChar('(') {
            parse_grouping(lc, tokens)
        } else {
            Some((lc, Err(anyhow!(AstError::UnexpectedToken(t)))))
        }
    } else {
        None
    }
}

fn parse_grouping(
    lc: (usize, usize),
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Expr>> {
    let parsed = parse_expr(tokens);
    if let Some((_, Ok(expr))) = parsed {
        if tokens
            .peeking_next(|(_, r)| {
                if let Ok(t) = r {
                    *t == Token::OneChar(')')
                } else {
                    false
                }
            })
            .is_none()
        {
            Some((lc, Err(anyhow!(AstError::Closing))))
        } else {
            Some((lc, Ok(Expr::group(expr))))
        }
    } else {
        parsed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_precedence() {
        assert!(Precedence::Factor > Token::OneChar('+').highest_valid_precedence())
    }
}
