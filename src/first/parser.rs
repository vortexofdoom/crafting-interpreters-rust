use super::syntax::{Expr, Keyword, LoxVal, Precedence, Statement, Token};
use anyhow::{anyhow, Result};
use once_cell::sync::Lazy;

use hash_chain::ChainMap;
use itertools::{any, Itertools, PeekingNext};
use std::{collections::HashMap, iter::Peekable, str::CharIndices};

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

#[derive(Debug)]
pub enum ParseError {
    UnclosedString,
    ParseNumError(String),
    InvalidToken(char),
    ClosingParen,
    UnexpectedToken(Token),
    MissingSemicolon,
    StatementMissingExpr,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnclosedString => {
                write!(f, "end of line reached with no closing '\"' found")
            }
            ParseError::ParseNumError(s) => write!(f, "error parsing number '{s}'"),
            ParseError::InvalidToken(c) => write!(f, "invalid token '{c}'"),
            ParseError::ClosingParen => write!(f, "missing closing ')'"),
            ParseError::UnexpectedToken(t) => write!(f, "unexpected token {t}"),
            ParseError::MissingSemicolon => write!(f, "missing semicolon"),
            ParseError::StatementMissingExpr => write!(f, "statement requires an expression"),
        }
    }
}

pub struct Parsed<T>(pub (usize, usize), pub Result<T>);

impl<T: std::fmt::Display> std::fmt::Display for Parsed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.1 {
            Ok(t) => write!(f, "{t}"),
            Err(e) => write!(f, "Error at ({}, {}): {e}", self.0 .0, self.0 .1),
        }
    }
}

impl<T> Parsed<T> {
    pub fn from_parsed<U>(other: Parsed<U>) -> Self {
        let result = if let Err(e) = other.1 {
            Err(e)
        } else {
            Err(anyhow!("cannot parse Ok values directly"))
        };
        Self(other.0, result)
    }

    pub fn from_info(at: (usize, usize), result: Result<T>) -> Self {
        Self(at, result)
    }
}

pub struct Parser {
    names: ChainMap<String, LoxVal>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            names: ChainMap::new(HashMap::new()),
        }
    }

    fn parse_var_dec(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Result<Statement> {
        if let Some(Parsed(_, Ok(Token::Identifier(name)))) = tokens.next() 
        && let Some(Parsed(_, token)) = tokens.next()
        && let Some(Parsed(_, expr)) = self.parse_expr(tokens) {
            if token.as_ref().is_ok_and(|t| *t == Token::OneChar('=')) {
                Ok(Statement::Declaration(name, expr?))
            } else {
                Err(anyhow!(ParseError::UnexpectedToken(token?)))
            }
        } else {
            Err(anyhow!(ParseError::StatementMissingExpr))
        }
    }

    fn parse_print_stmt(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Result<Statement> {
        if let Some(Parsed(_, res)) = self.parse_expr(tokens) {
            if tokens
                .peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
                .is_some()
            {
                Ok(Statement::Print(res?))
            } else {
                Err(anyhow!(ParseError::MissingSemicolon))
            }
        } else {
            Err(anyhow!(ParseError::StatementMissingExpr))
        }
    }

    fn parse_expr_statement(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Result<Statement> {
        if let Some(Parsed(_, res)) = self.parse_expr(tokens) {
            if tokens
                .peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
                .is_some()
            {
                Ok(Statement::Expression(res?))
            } else {
                Err(anyhow!(ParseError::MissingSemicolon))
            }
        } else {
            Err(anyhow!(ParseError::StatementMissingExpr))
        }
    }

    fn parse_statement(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Option<Parsed<Statement>> {
        use super::syntax::Keyword::*;
        tokens.next().map(|Parsed(lc, res)| {
            Parsed(
                lc,
                res.and_then(|token| match token {
                    Token::Keyword(Var) => self.parse_var_dec(tokens),
                    Token::Keyword(Print) => self.parse_print_stmt(tokens),
                    Token::CharThenEqual(_) => todo!(),
                    Token::Identifier(_) => todo!(),
                    Token::String(_) => todo!(),
                    Token::Number(_) => todo!(),
                    _ => self.parse_expr_statement(tokens),
                }),
            )
        })
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

    pub fn parse<'a>(
        &'a mut self,
        source: &'a str,
    ) -> impl PeekingNext<Item = Parsed<Statement>> + 'a {
        scan_tokens(source)
            .batching(|tokens| self.parse_statement(tokens))
            .peekable()
    }

    fn parse_expr(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Option<Parsed<Expr>> {
        self.parse_binary_expr(Precedence::Or, tokens)
    }

    fn parse_binary_expr(
        &mut self,
        precedence: Precedence,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Option<Parsed<Expr>> {
        if precedence < Precedence::Unary {
            self.parse_binary_expr(precedence.next_highest(), tokens)
                .map(|res| {
                    if let Parsed(lc, Ok(mut expr)) = res {
                        while let Some(Parsed(_, Ok(op))) = tokens.peeking_next(|Parsed(_, r)| {
                            r.as_ref().is_ok_and(|t| {
                                t.is_binary_operator() && t.matches_precedence(precedence)
                            })
                        }) {
                            match self.parse_binary_expr(precedence.next_highest(), tokens) {
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
            self.parse_unary_expr(tokens)
        }
    }

    fn parse_unary_expr(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Option<Parsed<Expr>> {
        // I really want there to be a way to consolidate the branches, but the different indices make it really difficult
        // let op = tokens.peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| t.is_unary_operator() && t.matches_precedence(Precedence::Unary)));
        if let Some(Parsed(lc, Ok(op))) = tokens
            .peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| t.is_unary_operator() && t.matches_precedence(Precedence::Unary)))
        && let Some(Parsed(_, Ok(right))) = self.parse_unary_expr(tokens) {
            Some(Parsed(lc, Ok(Expr::unary(Some(op), right))))
        } else {
            self.parse_primary_expr(tokens).map(|Parsed(lc, res)| Parsed(lc, res.map(|expr| Expr::unary(None, expr))))
        }
    }

    fn parse_primary_expr(
        &mut self,
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Option<Parsed<Expr>> {
        tokens
            .peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
            .and_then(|Parsed(lc, r)| {
                match r {
                    // any other branch is terminal, so we just early return if this is none
                    Ok(Token::OneChar('(')) => self.parse_grouping(lc, tokens),
                    Ok(Token::Identifier(name)) => {
                        Some(Parsed(lc, Ok(Expr::Token(Token::Identifier(name)))))
                    }
                    Ok(t) if t.is_literal() => Some(Parsed(lc, Ok(Expr::Token(t)))),
                    Ok(t) => Some(Parsed(lc, Err(anyhow!(ParseError::UnexpectedToken(t))))),
                    Err(e) => Some(Parsed(lc, Err(e))),
                }
            })
    }

    fn parse_grouping(
        &mut self,
        lc: (usize, usize),
        tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
    ) -> Option<Parsed<Expr>> {
        self.parse_binary_expr(Precedence::Or, tokens)
            .map(|Parsed(_, res)| {
                Parsed(
                    lc,
                    res.and_then(|expr| {
                        if tokens
                            .peeking_next(|Parsed(_, r)| {
                                r.as_ref().is_ok_and(|t| *t == Token::OneChar(')'))
                            })
                            .is_none()
                        {
                            Err(anyhow!(ParseError::ClosingParen))
                        } else {
                            Ok(Expr::group(expr))
                        }
                    }),
                )
            })
    }
}

fn scan_tokens(source: &str) -> impl PeekingNext<Item = Parsed<Token>> + '_ {
    source
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.char_indices()
                .peekable()
                .batching(|chars| scan_token(chars))
                .map(move |(j, r)| Parsed((i + 1, j + 1), r))
        })
        .peekable()
}

fn scan_token(chars: &mut Peekable<CharIndices>) -> Option<(usize, Result<Token>)> {
    chars.next().and_then(|(i, c)| match c {
        '"' => {
            let string = chars
                .peeking_take_while(|&(_, c)| c != '"')
                .map(|(_, c)| c)
                .collect();
            if let Some((_, '"')) = chars.next() {
                Some((i, Ok(Token::String(string))))
            // if the next char is not '"' as we specified it means we reached the end of the string
            } else {
                Some((i, Err(anyhow!(ParseError::UnclosedString))))
            }
        }
        // Code specific to handling comments
        '/' => chars.peek().and_then(|ci| {
            if ci.1 == '/' {
                // another slash means we have a comment and can ignore the rest of the line after this point
                None
            } else {
                Some((i, Ok(Token::OneChar('/'))))
            }
        }),
        // One or two character tokens
        '!' | '=' | '<' | '>' => Some((
            i,
            Ok(if chars.peeking_next(|ci| ci.1 == '=').is_some() {
                Token::CharThenEqual(c)
            } else {
                Token::OneChar(c)
            }),
        )),

        // Invariably single char tokens
        '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => {
            Some((i, Ok(Token::OneChar(c))))
        }
        x if x.is_ascii_digit() => Some(parse_number(i, c, chars)),
        a if a == '_' || a.is_ascii_alphabetic() => Some((i, Ok(parse_word(c, chars)))),
        w if w.is_whitespace() => scan_token(chars),
        _ => Some((i, Err(anyhow!(ParseError::InvalidToken(c))))),
    })
}

fn parse_number(
    start: usize,
    first: char,
    chars: &mut Peekable<CharIndices>,
) -> (usize, Result<Token>) {
    let mut chars_cl = chars.clone();
    let number = chars_cl
        .peeking_take_while(|(_, c)| c.is_ascii_digit() || *c == '.')
        .map(|(i, c)| (i - start, c));
    let string: String = if let Some((mut i, c)) = number.last() {
        // This only makes sure we are not including a trailing '.' with no digits following
        // This could technically return an arbitrary chaining of digits and '.'
        // but that can be handled by the int parsing, since it can't represent any other valid Lox syntax
        // if the last char is '.' we take everything up to but excluding it, as it is either a function call or an error
        if c == '.' {
            i = i - 1;
        }
        std::iter::once(first)
            .chain(chars.take(i).map(|(_, c)| c))
            .collect()
    } else {
        // Otherwise all we have is a single digit
        String::from(first)
    };

    if let Ok(n) = string.parse::<f64>() {
        (start, Ok(Token::Number(n)))
    } else {
        (start, Err(anyhow!(ParseError::ParseNumError(string))))
    }
}

/// Parses a string of characters that can begin with a letter or '_', and determines whether it is an keyword or an identifier
/// Returns a `Token` as this will always be valid, since we start with one valid character (which is a valid identifier alone) and stop at the first invalid character
/// validating the identifier itself comes in the next pass.
fn parse_word(first: char, chars: &mut Peekable<CharIndices>) -> Token {
    let string: String = std::iter::once(first)
        .chain(
            chars
                .peeking_take_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
                .map(|(_, c)| c),
        )
        .collect();

    if let Some(&k) = KEYWORDS.get(string.as_str()) {
        Token::Keyword(k)
    } else {
        Token::Identifier(string)
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
