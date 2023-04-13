use super::syntax::{Expr, Keyword, LoxVal, Precedence, Statement, Token};
use anyhow::{anyhow, Result};
use once_cell::sync::Lazy;

use hash_chain::ChainMap;
use itertools::{Itertools, MultiPeek, PeekingNext};
use std::{collections::HashMap, str::CharIndices};

pub struct Parser<'a> {
    names: ChainMap<String, LoxVal>,
    source: &'a str,
}

impl<'a> Parser<'a> {
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

pub fn parse(source: &str) -> MultiPeek<impl Iterator<Item = Parsed<Statement>>> {
    let mut tokens = scan_tokens(source);
    use super::syntax::Keyword::*;
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
                while let Some(Parsed(_, Ok(op))) = tokens.peeking_next(|r| {
                    if let Ok(t) = &r.1 {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_precedence() {
        assert!(Precedence::Factor > Token::OneChar('+').highest_valid_precedence())
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

fn scan_tokens(source: &str) -> MultiPeek<impl Iterator<Item = Parsed<Token>> + '_> {
    source
        .lines()
        .enumerate()
        .flat_map(|(i, l)| scan_line(l).map(move |(j, r)| Parsed((i, j), r)))
        .multipeek()
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
            a if a == '_' || a.is_ascii_alphabetic() => Some(parse_word(i, c, chars)),
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
    let prefix = chars
        .peeking_take_while(|(_, c)| c.is_ascii_digit())
        .map(|(_, c)| c);
    let mut string: String = std::iter::once(first).chain(prefix).collect();
    chars.reset_peek();

    if let Some((_, '.')) = chars.peek()
    && let Some((_, n)) = chars.peek()
    && n.is_ascii_digit() {
        chars.next();
        string
            .extend(
                std::iter::once('.')
                .chain(chars.peeking_take_while(|(_, c)| c.is_ascii_digit()).map(|(_, c)| c))
            );

    }

    if let Ok(n) = string.parse::<f64>() {
        (start, Ok(Token::Number(n)))
    } else {
        (start, Err(anyhow!(ParseError::ParseNumError(string))))
    }
}

fn parse_word(
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
