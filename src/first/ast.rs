use anyhow::{anyhow, Result};
use itertools::{Itertools, MultiPeek};

use super::Token;

#[derive(Debug)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Token),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(t) => write!(f, "{t}"),
            Expr::Grouping(e) => write!(f, "({e})"),
            Expr::Binary(b) => write!(f, "{} {} {}", b.left, b.operator, b.right),
            Expr::Unary(u) => {
                if let Some(op) = &u.operator {
                    write!(f, "({}{})", op, u.right)
                } else {
                    write!(f, "({})", u.right)
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

pub fn parse(tokens: &mut MultiPeek<impl Iterator<Item = Result<Token>>>) -> Vec<Result<Expr>> {
    let mut expressions = vec![];
    while let Some(expr) = parse_expression(tokens) {
        expressions.push(expr);
    }
    let (ok, err): (Vec<_>, Vec<_>) = expressions.into_iter().partition(|r| r.is_ok());
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

fn parse_expression(
    tokens: &mut MultiPeek<impl Iterator<Item = Result<Token>>>,
) -> Option<Result<Expr>> {
    parse_binary_expr(Precedence::Equality, tokens)
}

fn parse_binary_expr(
    precedence: Precedence,
    tokens: &mut MultiPeek<impl Iterator<Item = Result<Token>>>,
) -> Option<Result<Expr>> {
    if precedence < Precedence::Unary
    && let Some(Ok(mut expr)) = parse_binary_expr(precedence.next_highest(), tokens) {
        while let Some(Ok(op)) = tokens
            .peeking_take_while(|r| if let Ok(t) = r {
                t.is_operator() && t.highest_valid_precedence() >= precedence
            } else {
                false
            })
            .next()
        && let Some(Ok(right)) = parse_binary_expr(precedence.next_highest(), tokens) {
            expr = Expr::binary(expr, op, right)
        }
        Some(Ok(expr))
    } else {
        parse_unary_expr(tokens)
    }
}

fn parse_unary_expr(
    tokens: &mut MultiPeek<impl Iterator<Item = Result<Token>>>,
) -> Option<Result<Expr>> {
    if let Some(Ok(op)) = tokens
        .peeking_take_while(|r| if let Ok(t) = r {
            t.is_operator() && t.matches_precedence(Precedence::Unary)
        } else {
            false
        })
        .next()
    && let Some(Ok(right)) = parse_unary_expr(tokens) {
        Some(Ok(Expr::unary(Some(op), right)))
    } else {
        parse_primary(tokens)
    }
}

fn parse_primary(
    tokens: &mut MultiPeek<impl Iterator<Item = Result<Token>>>,
) -> Option<Result<Expr>> {
    if let Some(Ok(t)) = tokens.next() {
        if t.is_literal() {
            Some(Ok(Expr::Literal(t)))
        } else if t == Token::OneChar('(') {
            let token = parse_expression(tokens);
            if let Some(Ok(Token::OneChar(')'))) = tokens.peek() {
                tokens.next();
                token.and_then(|r| {
                    if let Ok(e) = r {
                        Some(Ok(Expr::group(e)))
                    } else {
                        None
                    }
                })
            } else {
                Some(Err(anyhow!("missing closing ')'")))
            }
        } else {
            Some(Err(anyhow!("Unexpected Token {t}")))
        }
    } else {
        None
    }
}
