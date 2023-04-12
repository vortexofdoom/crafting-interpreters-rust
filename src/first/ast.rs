use std::fmt::Display;

use anyhow::anyhow;
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
            Expr::Grouping(e) => write!(f, "({e})"),
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

#[derive(Debug)]
pub struct UnaryExpr {
    operator: Option<Token>,
    right: Expr,
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
}

pub fn parse(tokens: &mut MultiPeek<impl Iterator<Item = Parsed<Token>>>) -> Vec<Parsed<Expr>> {
    let mut expressions = vec![];
    while let Some(expr) = parse_expr(tokens) {
        expressions.push(expr);
    }
    let (ok, err): (Vec<_>, Vec<_>) = expressions.into_iter().partition(|(_, r)| r.is_ok());
    if err.is_empty() {
        ok
    } else {
        err
    }
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
        if let Some(res) = parse_binary_expr(precedence.next_highest(), tokens) {
            match res {
                (lc, Ok(mut expr)) => {
                    while let Some((_, Ok(op))) = tokens
                        .peeking_next(|(_, r)| if let Ok(t) = r {
                            t.is_operator() && t.matches_precedence(precedence)
                        } else {
                            false
                        })
                    && let Some((_, Ok(right))) = parse_binary_expr(precedence.next_highest(), tokens) {
                        expr = Expr::binary(expr, op, right)
                    }
                    Some((lc, Ok(expr)))
                }
                _ => Some(res),
            }
        } else {
            None
        }
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
            let token = parse_expr(tokens);
            if let Some(&(lc, Ok(Token::OneChar(')')))) = tokens.peek() {
                tokens.next();
                token.and_then(|(_, r)| {
                    if let Ok(e) = r {
                        Some((lc, Ok(Expr::group(e))))
                    } else {
                        None
                    }
                })
            } else {
                Some((lc, Err(anyhow!(AstError::Closing))))
            }
        } else {
            Some((lc, Err(anyhow!(AstError::UnexpectedToken(t)))))
        }
    } else {
        None
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
