use std::{collections::HashMap, str::CharIndices};

use anyhow::{anyhow, Result};
use hash_chain::ChainMap;
use itertools::{Itertools, MultiPeek, PeekingNext};

use super::{ParseError, Parsed, Token};

pub struct Ast<'a> {
    names: ChainMap<String, LoxVal>,
    source: &'a str,
}

impl<'a> Ast<'a> {
    pub fn construct(source: &'a str) -> Self {
        Self {
            names: ChainMap::new(HashMap::new()),
            source,
        }
    }

    pub fn chars(&mut self) -> MultiPeek<CharIndices<'a>> {
        self.source.char_indices().multipeek()
    }

    fn parse_var_dec(
        tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
    ) -> Parsed<Statement> {
        match tokens.next() {
            Some(Parsed(lc, Ok(Token::Identifier(name)))) => todo!(),
            _ => todo!(),
        }
        if let Some(Parsed(lc, Ok(t))) = tokens.next() {}
        if let Some(Parsed(lc, Ok(Token::Identifier(s)))) = tokens.next() {}
        todo!()
    }
}

#[derive(Debug)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Token(Token),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Token(t) => write!(f, "{t}"),
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
    pub fn evaluate(self) -> Result<LoxVal> {
        use super::Keyword::*;
        match self.operator {
            Token::OneChar('*') => self.left.evaluate()? * self.right.evaluate()?,
            Token::OneChar('/') => self.left.evaluate()? / self.right.evaluate()?,
            Token::OneChar('+') => self.left.evaluate()? + self.right.evaluate()?,
            Token::OneChar('-') => self.left.evaluate()? - self.right.evaluate()?,
            Token::OneChar('>') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x > y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::OneChar('<') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x < y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('>') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x >= y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('<') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x <= y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('=') => Ok(LoxVal::Boolean(
                self.left.evaluate()? == self.right.evaluate()?,
            )),
            Token::CharThenEqual('!') => Ok(LoxVal::Boolean(
                self.left.evaluate()? != self.right.evaluate()?,
            )),
            Token::Keyword(And) => {
                let left = self.left.evaluate()?;
                if !left.truthy() {
                    Ok(left)
                } else {
                    self.right.evaluate()
                }
            }
            Token::Keyword(Or) => {
                let left = self.left.evaluate()?;
                if left.truthy() {
                    Ok(left)
                } else {
                    self.right.evaluate()
                }
            }
            _ => Err(anyhow!("{} is not a valid binary operator", self.operator)),
        }
    }
}

#[derive(Debug)]
pub enum LoxVal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl LoxVal {
    fn truthy(&self) -> bool {
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

impl std::ops::Sub<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn sub(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x - y)),
            _ => Err(anyhow!("cannot subtract {rhs} from {self}")),
        }
    }
}

impl std::ops::Mul<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn mul(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x * y)),
            _ => Err(anyhow!("cannot multiply {self} and {rhs}")),
        }
    }
}

impl std::ops::Div<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn div(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x / y)),
            _ => Err(anyhow!("cannot divide {self} and {rhs}")),
        }
    }
}

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

#[derive(Debug)]
pub struct UnaryExpr {
    operator: Option<Token>,
    right: Expr,
}

impl UnaryExpr {
    pub fn evaluate(self) -> Result<LoxVal> {
        if let Some(t) = self.operator {
            match (t, self.right.evaluate()?) {
                (Token::OneChar('!'), o) => Ok(LoxVal::Boolean(!o.truthy())),
                (Token::OneChar('-'), LoxVal::Number(n)) => Ok(LoxVal::Number(-n)),
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

    pub fn evaluate(self) -> Result<LoxVal> {
        use super::Keyword::*;
        match self {
            Expr::Grouping(e) => e.evaluate(),
            Expr::Binary(b) => b.evaluate(),
            Expr::Unary(u) => u.evaluate(),
            Expr::Token(o) => match o {
                Token::String(s) => Ok(LoxVal::String(s)),
                Token::Number(n) => Ok(LoxVal::Number(n)),
                Token::Identifier(s) => todo!(),
                Token::Keyword(True) => Ok(LoxVal::Boolean(true)),
                Token::Keyword(False) => Ok(LoxVal::Boolean(false)),
                Token::Keyword(Nil) => Ok(LoxVal::Nil),
                Token::Keyword(This) => todo!(),
                Token::Keyword(Class) => todo!(),
                Token::Keyword(_) => todo!(),
                _ => todo!(),
            },
        }
    }
}

pub fn parse(
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> MultiPeek<impl Iterator<Item = Parsed<Statement>>> {
    use super::Keyword::*;
    // Maybe try to do this with a map instead of pushing to a vec
    let mut statements = vec![];
    while let Some(Parsed(lc, Ok(token))) = tokens.next() {
        statements.push(match token {
            Token::Keyword(Var) => todo!(), //parse_var_dec(tokens),
            Token::Keyword(Print) => todo!(),
            Token::CharThenEqual(_) => todo!(),
            Token::Identifier(_) => todo!(),
            Token::String(_) => todo!(),
            Token::Number(_) => todo!(),
            _ => todo!(),
        });
        // match (parse_expr(tokens), tokens.next()) {
        //     (Some(Parsed(_, Ok(expr))), Some(Parsed(_, Ok(Token::OneChar(';'))))) => if token == Token::Keyword(Print) {
        //         statements.push(Parsed(lc, Ok(Statement::Print(expr))))
        //     } else {
        //         statements.push(Parsed(lc, Ok(Statement::Expression(expr))))
        //     },
        //     (Some(Parsed(lc, Ok(expr))), None) => statements.push(Parsed(lc, Err(anyhow!(ParseError::MissingSemicolon)))),
        //     (Some(Parsed(_, Ok(_))), Some(Parsed(lc, Ok(t)))) => statements.push(Parsed(lc, Err(anyhow!(ParseError::UnexpectedToken(t))))),
        //     (Some(Parsed(_, Ok(_))), Some(err)) => statements.push(Parsed::from_parsed(err)),
        //     (Some(err), _) => statements.push(Parsed::from_parsed(err)),
        //     (None, None) => statements.push(Parsed(lc, Err(anyhow!(ParseError::StatementMissingExpr)))),
        // }
    }
    statements.into_iter().multipeek()
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
    fn next_highest(&self) -> Self {
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

fn parse_expr(tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>) -> Option<Parsed<Expr>> {
    parse_binary_expr(Precedence::Equality, tokens)
}

fn parse_binary_expr(
    precedence: Precedence,
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Expr>> {
    if precedence < Precedence::Unary {
        parse_binary_expr(precedence.next_highest(), tokens).map(|res| {
            if let Parsed(lc, Ok(mut expr)) = res {
                while let Some(Parsed(_, Ok(op))) = tokens.peeking_next(|Parsed(_, r)| {
                    if let Ok(t) = r {
                        t.is_binary_operator() && t.matches_precedence(precedence)
                    } else {
                        false
                    }
                }) {
                    match parse_binary_expr(precedence.next_highest(), tokens) {
                        Some(Parsed(_, Ok(right))) => expr = Expr::binary(expr, op, right),
                        Some(err) => return err,
                        None => break,
                    }
                }
                Parsed(lc, Ok(expr))
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
    if let Some(Parsed(lc, Ok(op))) = tokens
        .peeking_next(|Parsed(_, r)| if let Ok(t) = r {
            t.is_binary_operator() && t.matches_precedence(Precedence::Unary)
        } else {
            false
        })
    && let Some(Parsed(_, Ok(right))) = parse_unary_expr(tokens) {
        Some(Parsed(lc, Ok(Expr::unary(Some(op), right))))
    } else {
        match parse_primary_expr(tokens) {
            Some(Parsed(lc, Ok(expr))) => Some(Parsed(lc, Ok(Expr::unary(None, expr)))),
            Some(err) => Some(err),
            None => None,
        }
    }
}

fn parse_primary_expr(
    tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Expr>> {
    if let Some(Parsed(lc, Ok(t))) = tokens.peeking_next(|Parsed(_, r)| {
        r.is_ok() && r.as_ref().expect("should short circuit first") == &Token::OneChar(';')
    }) {
        if t.is_literal() {
            Some(Parsed(lc, Ok(Expr::Token(t))))
        } else if t == Token::OneChar('(') {
            parse_grouping(lc, tokens)
        } else {
            Some(Parsed(lc, Err(anyhow!(ParseError::UnexpectedToken(t)))))
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
    if let Some(Parsed(_, Ok(expr))) = parsed {
        if tokens
            .peeking_next(|Parsed(_, r)| {
                if let Ok(t) = r {
                    *t == Token::OneChar(')')
                } else {
                    false
                }
            })
            .is_none()
        {
            Some(Parsed(lc, Err(anyhow!(ParseError::ClosingParen))))
        } else {
            Some(Parsed(lc, Ok(Expr::group(expr))))
        }
    } else {
        parsed
    }
}

#[derive(Debug)]
pub enum Statement {
    Declaration(String, Expr),
    Expression(Expr),
    Print(Expr),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(e) | Self::Print(e) => write!(f, "{e}"),
            _ => todo!(),
        }
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
