use super::syntax::{Declaration, Expr, Keyword, Precedence, Statement, Token};
use anyhow::{anyhow, Result};

use itertools::{Itertools, PeekingNext};
use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, Clone)]
pub enum ParseError {
    UnclosedString,
    ParseNumError(String),
    InvalidToken(char),
    ClosingParen,
    UnexpectedToken(Token),
    MissingSemicolon,
    StatementMissingExpr,
    // UndefinedVariable(String),
    Expected(ExpectedToken, Option<Token>),
}

#[derive(Debug, Clone)]
pub enum ExpectedToken {
    Identifier,
    Delimiter(char),
}

impl std::fmt::Display for ExpectedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedToken::Identifier => write!(f, "identifier"),
            ExpectedToken::Delimiter(c) => write!(f, "{c}"),
        }
    }
}

impl std::error::Error for ParseError {}

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
            // ParseError::UndefinedVariable(s) => write!(f, "undefined variable {s}"),
            ParseError::Expected(exp, found) => {
                if let Some(token) = found {
                    write!(f, "expected {exp}, found {token}")
                } else {
                    write!(f, "expected {exp}, but reached eof")
                }
            }
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

/// This should only get called if we have already peeked, know to advance, and do not need the token
fn get_line_column(tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>) -> (usize, usize) {
    tokens
        .next()
        .expect("should only be called when a token is known to exist")
        .0
}

// TODO: Fix this monstrosity
fn parse_var_dec(tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>) -> Parsed<Statement> {
    // advancing past "var"
    let start = get_line_column(tokens);
    // short circuiting directly to primary since the only valid token is an identifier
    if let Some(Parsed(lc, Ok(Expr::Variable(name)))) = parse_primary_expr(tokens) {
        match tokens.next() {
                Some(Parsed(_, Ok(Token::OneChar(';')))) => {
                    Parsed(start, Ok(Statement::Declaration(Declaration::Variable(name, None))))
                }
                Some(Parsed(_, Ok(Token::OneChar('=')))) => {
                    if let Some(Parsed(_, Ok(expr))) = parse_expr(tokens)
                    && let Some(Parsed(lc, res)) = tokens.next() {
                        res.map_or_else(
                            |err| Parsed(lc, Err(err)),
                            |t| match t {
                                Token::OneChar(';') => Parsed(start, Ok(Statement::Declaration(Declaration::Variable(name, Some(expr))))),

                                _ => Parsed(lc, Err(anyhow!(ParseError::UnexpectedToken(t)))),
                            })
                    } else {
                        Parsed(lc, Err(anyhow!(ParseError::MissingSemicolon)))
                    }
                }
                Some(Parsed(lc, Ok(token))) => {
                    Parsed(lc, Err(anyhow!(ParseError::UnexpectedToken(token))))
                }
                Some(Parsed(lc, Err(err))) => Parsed(lc, Err(err)),
                None => Parsed(lc, Err(anyhow!(ParseError::MissingSemicolon))),
            }
    } else {
        Parsed(
            start,
            Err(anyhow!(ParseError::Expected(
                ExpectedToken::Identifier,
                None
            ))),
        )
    }
}

fn parse_assignment(
    lvalue: Expr,
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Result<Expr> {
    tokens.next();
    if lvalue.is_lvalue() {
        match parse_binary_expr(Precedence::Or, tokens) {
            Some(Parsed(_, Ok(rvalue))) => Ok(Expr::assignment(lvalue, rvalue)),
            Some(Parsed(_, Err(err))) => Err(err),
            None => Err(anyhow!(ParseError::StatementMissingExpr)),
        }
    } else {
        Err(anyhow!("{lvalue}"))
    }
}

fn check_semicolon(
    statement: Statement,
    tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
) -> Result<Statement> {
    tokens
        .peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
        .map(|_| statement)
        .ok_or(anyhow!(ParseError::MissingSemicolon))
}

fn parse_print_stmt(
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Parsed<Statement> {
    // advance past "print"
    let start = get_line_column(tokens);
    if let Some(Parsed(lc, res)) = parse_expr(tokens) {
        match res {
            Ok(expr) => Parsed(start, check_semicolon(Statement::Print(expr), tokens)),
            Err(err) => Parsed(lc, Err(err)),
        }
    } else {
        Parsed(start, Err(anyhow!(ParseError::StatementMissingExpr)))
    }
}

fn parse_expr_statement(
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Parsed<Statement> {
    let Parsed(lc, res) =
        parse_expr(tokens).expect("we're only calling this when we've already peeked a token");

    match res {
        Ok(expr) => Parsed(lc, check_semicolon(Statement::Expression(expr), tokens)),
        Err(err) => Parsed(lc, Err(err)),
    }
}

// Currently I don't check for parentheses around if and while
// it'd be trivial, I just don't see the point when it parses correctly
fn parse_if_statement(
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Parsed<Statement> {
    use Keyword::*;
    let lc = get_line_column(tokens);
    match (parse_expr(tokens), parse_statement(tokens)) {
        (Some(Parsed(_, Ok(cond))), Some(Parsed(_, Ok(if_exec)))) => match tokens
            .peeking_next(|Parsed(_, res)| res.as_ref().is_ok_and(|t| *t == Token::Keyword(Else)))
            .and_then(|_| parse_statement(tokens))
        {
            Some(Parsed(_, Ok(else_exec))) => Parsed(
                lc,
                Ok(Statement::If(
                    cond,
                    Box::new(if_exec),
                    Some(Box::new(else_exec)),
                )),
            ),
            None => Parsed(lc, Ok(Statement::If(cond, Box::new(if_exec), None))),
            Some(err) => err,
        },
        (Some(Parsed(lc, Err(err))), _) | (_, Some(Parsed(lc, Err(err)))) => Parsed(lc, Err(err)),
        // Could make this more robust in future, but currently it'll be some combination of 'Nones' and 'Errors'
        _ => Parsed(lc, Err(anyhow!(ParseError::StatementMissingExpr))),
    }
}

fn parse_while_statement(
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Parsed<Statement> {
    let lc = get_line_column(tokens);
    match (parse_expr(tokens), parse_statement(tokens)) {
        (Some(Parsed(_, Ok(cond))), Some(Parsed(_, Ok(while_exec)))) => {
            Parsed(lc, Ok(Statement::While(cond, Box::new(while_exec))))
        }
        (Some(Parsed(lc, Err(err))), _) | (_, Some(Parsed(lc, Err(err)))) => Parsed(lc, Err(err)),
        // Could make this more robust in future, but currently it'll be some combination of 'Nones' and 'Errors' which is impossible to parse anyway
        _ => Parsed(lc, Err(anyhow!(ParseError::StatementMissingExpr))),
    }
}

fn parse_for_statement(
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Parsed<Statement> {
    let lc = get_line_column(tokens);
    let next = tokens
        .peeking_next(|Parsed(_, res)| res.as_ref().is_ok_and(|t| *t == Token::OneChar('(')))
        .unwrap_or_else(|| {
            Parsed(
                lc,
                Err(anyhow!(ParseError::Expected(
                    ExpectedToken::Delimiter('('),
                    None
                ))),
            )
        });

    if let Parsed(lc, Err(err)) = next {
        return Parsed(lc, Err(err));
    }

    let init = if tokens
        .peeking_next(|Parsed(_, res)| res.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
        .is_none()
    {
        let parsed =
            parse_statement(tokens).expect("just checked that it wasn't last token or ';'");
        match parsed {
            Parsed(_, Err(_)) => return parsed,
            Parsed(
                _,
                Ok(
                    stmt @ (Statement::Declaration(Declaration::Variable(_, _))
                    | Statement::Expression(_)),
                ),
            ) => Some(stmt),
            Parsed(lc, Ok(s)) => return Parsed(lc, Err(anyhow!("{s} is not a valid initializer"))),
        }
    } else {
        None
    };

    let cond = if tokens
        .peeking_next(|Parsed(_, res)| res.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
        .is_none()
    {
        let parsed =
            parse_statement(tokens).expect("just checked that it wasn't last token or ';'");
        match parsed {
            Parsed(_, Err(_)) => return parsed,
            Parsed(_, Ok(Statement::Expression(expr))) => Some(expr),
            Parsed(lc, Ok(_stmt)) => return Parsed(lc, Err(anyhow!("invalid condition"))),
        }
    } else {
        None
    };
    let inc = if tokens
        .peeking_next(|Parsed(_, res)| res.as_ref().is_ok_and(|t| *t == Token::OneChar(';')))
        .is_none()
    {
        match parse_expr(tokens).expect("just checked that there was a token") {
            Parsed(lc, Err(err)) => return Parsed(lc, Err(err)),
            Parsed(_, Ok(expr)) => Some(Statement::Expression(expr)),
        }
    } else {
        None
    };
    if inc.is_some()
        && tokens
            .peeking_next(|Parsed(_, res)| res.as_ref().is_ok_and(|t| *t == Token::OneChar(')')))
            .is_none()
    {
        return Parsed(
            lc,
            Err(anyhow!(ParseError::Expected(
                ExpectedToken::Delimiter(')'),
                None
            ))),
        );
    }

    let for_exec = if let Some(parsed) = parse_statement(tokens) {
        if let Parsed(_, Ok(stmt)) = parsed {
            match inc {
                None => Some(stmt),
                Some(inc) => Some(Statement::Block(vec![stmt, inc])),
            }
        } else {
            return parsed;
        }
    } else {
        None
    };

    let while_stmt = if let Some(cond) = cond
    && let Some(while_exec) = for_exec {
        Some(Statement::While(cond, Box::new(while_exec)))
    } else {
        None
    };

    Parsed(
        lc,
        Ok(Statement::Block(
            [init, while_stmt].into_iter().flatten().collect(),
        )),
    )
}

fn parse_block(tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>) -> Parsed<Statement> {
    // consume the '{'
    let lc = get_line_column(tokens);

    let mut vec = vec![];
    // loop to consume the whole block whether or not there is a parse error, for sync purposes
    while let Some(Parsed(_, Ok(token))) = tokens.peek()
    && *token != Token::OneChar('}') {
        vec.push(parse_statement(tokens).unwrap_or(Parsed(lc, Err(anyhow!(ParseError::Expected(ExpectedToken::Delimiter('}'), None))))))
    }

    match tokens.peeking_next(|Parsed(_, res)| {
        (res.as_ref().is_ok_and(|t| *t == Token::OneChar('}'))) || res.is_err()
    }) {
        Some(Parsed(_, Ok(_))) => {
            if vec.iter().any(|parsed| parsed.1.is_err()) {
                // we return only the error but consume the whole block
                vec.into_iter().find(|parsed| parsed.1.is_err()).unwrap()
            } else {
                let vec = vec.into_iter().map(|Parsed(_, res)| res.unwrap()).collect();
                Parsed(lc, Ok(Statement::Block(vec)))
            }
        }
        Some(Parsed(lc, Err(err))) => Parsed(lc, Err(err)),
        None => Parsed(
            lc,
            Err(anyhow!(ParseError::Expected(
                ExpectedToken::Delimiter('}'),
                None
            ))),
        ),
    }
}

fn parse_statement(
    tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>,
) -> Option<Parsed<Statement>> {
    use super::syntax::Keyword::*;
    // Peeking because in the case of expression statements we don't want to consume a token
    // peeking_next().and_then() might be able to get rid of the else, but we'll wait on that until it's more complete
    if let Some(Parsed(_, res)) = tokens.peek() {
        //println!("{res:?}");
        match res {
            Ok(token) => match token {
                Token::OneChar('{') => Some(parse_block(tokens)),
                Token::Keyword(Var) => Some(parse_var_dec(tokens)),
                Token::Keyword(Print) => Some(parse_print_stmt(tokens)),
                Token::Keyword(If) => Some(parse_if_statement(tokens)),
                Token::Keyword(While) => Some(parse_while_statement(tokens)),
                Token::Keyword(For) => Some(parse_for_statement(tokens)),
                _ => Some(parse_expr_statement(tokens)),
            },
            _ => {
                let Parsed(lc, res) = tokens.next().unwrap();
                Some(Parsed(lc, Err(res.unwrap_err())))
            }
        }
    } else {
        None
    }
}

pub fn parse(source: &str) -> impl PeekingNext<Item = Parsed<Statement>> + '_ {
    scan_tokens(source).batching(parse_statement).peekable()
}

fn parse_expr(tokens: &mut Peekable<impl Iterator<Item = Parsed<Token>>>) -> Option<Parsed<Expr>> {
    let expr = parse_binary_expr(Precedence::Or, tokens)?;
    match expr {
        Parsed(lc, Ok(e)) => match tokens.peek() {
            Some(Parsed(_, Ok(Token::OneChar('=')))) => {
                Some(Parsed(lc, parse_assignment(e, tokens)))
            }
            Some(Parsed(_, Ok(Token::OneChar(';') | Token::OneChar('{')))) => {
                Some(Parsed(lc, Ok(e)))
            }
            Some(Parsed(tlc, Ok(t))) => Some(Parsed(
                *tlc,
                Err(anyhow!(ParseError::UnexpectedToken(t.clone()))),
            )),
            Some(Parsed(tlc, Err(_))) => {
                Some(Parsed(*tlc, Err(anyhow!(ParseError::MissingSemicolon))))
            }
            None => Some(Parsed(lc, Err(anyhow!(ParseError::MissingSemicolon)))),
        },
        Parsed(lc, Err(err)) => Some(Parsed(lc, Err(err))),
    }
}

fn parse_binary_expr(
    precedence: Precedence,
    tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
) -> Option<Parsed<Expr>> {
    if precedence < Precedence::Unary {
        parse_binary_expr(precedence.next_highest(), tokens).map(|res| {
            if let Parsed(lc, Ok(mut expr)) = res {
                while let Some(Parsed(_, Ok(op))) = tokens.peeking_next(|Parsed(_, r)| {
                    r.as_ref()
                        .is_ok_and(|t| t.is_binary_operator() && t.matches_precedence(precedence))
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

fn parse_unary_expr(tokens: &mut impl PeekingNext<Item = Parsed<Token>>) -> Option<Parsed<Expr>> {
    // I really want there to be a way to consolidate the branches, but the different indices make it really difficult
    // let op = tokens.peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| t.is_unary_operator() && t.matches_precedence(Precedence::Unary)));
    if let Some(Parsed(lc, Ok(op))) = tokens
            .peeking_next(|Parsed(_, r)| r.as_ref().is_ok_and(|t| t.is_unary_operator() && t.matches_precedence(Precedence::Unary)))
        && let Some(Parsed(_, Ok(right))) = parse_unary_expr(tokens) {
            Some(Parsed(lc, Ok(Expr::unary(op, right))))
        } else {
            parse_primary_expr(tokens)
        }
}

fn parse_primary_expr(tokens: &mut impl PeekingNext<Item = Parsed<Token>>) -> Option<Parsed<Expr>> {
    use itertools::Either::*;
    tokens
        .peeking_next(|Parsed(_, r)| {
            r.as_ref().is_ok_and(|t| {
                *t != Token::OneChar(';') && *t != Token::OneChar('=') && *t != Token::OneChar('{')
            })
        })
        .and_then(|Parsed(lc, r)| match r {
            Ok(Token::OneChar('(')) => parse_grouping(lc, tokens),
            Ok(Token::Identifier(name)) => Some(Parsed(lc, Ok(Expr::Variable(name)))),
            Ok(t) => Some(Parsed(
                lc,
                match t.try_convert_literal() {
                    Left(val) => Ok(Expr::Literal(val)),
                    Right(token) => Err(anyhow!(ParseError::UnexpectedToken(token))),
                },
            )),
            Err(e) => Some(Parsed(lc, Err(e))),
        })
}

fn parse_grouping(
    lc: (usize, usize),
    tokens: &mut impl PeekingNext<Item = Parsed<Token>>,
) -> Option<Parsed<Expr>> {
    parse_binary_expr(Precedence::Or, tokens).map(|Parsed(_, res)| {
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

fn scan_tokens(source: &str) -> Peekable<impl Iterator<Item = Parsed<Token>> + '_> {
    source
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.char_indices()
                .peekable()
                .batching(scan_token)
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
            } else {
                // if the next char is not '"' as we specified it means we've reached the end of the line
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
            i -= 1;
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

    if let Some(k) = Keyword::try_from_str(&string) {
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
