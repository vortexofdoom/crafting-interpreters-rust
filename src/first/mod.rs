mod parser;
mod syntax;

use self::parser::parse;

use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use anyhow::{anyhow, Result};
use clap::Parser;
use hash_chain::ChainMap;
use syntax::{Expr, LoxVal, Statement, Token};

#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    InvalidLValue(Expr),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::UndefinedVariable(_) => todo!(),
            RuntimeError::InvalidLValue(expr) => write!(f, "{expr} is not a valid lvalue"),
        }
    }
}

pub struct Interpreter {
    names: ChainMap<String, LoxVal>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            names: ChainMap::new(HashMap::new()),
        }
    }

    fn add_identifier(&mut self, declaration: Statement) {
        use syntax::Declaration::*;
        if let Statement::Declaration(Variable(name, initialize)) = declaration {
            let value = match initialize {
                Some(expr) => self.evaluate(expr).unwrap_or(LoxVal::Nil),
                None => LoxVal::Nil,
            };

            self.names.insert(name, value);
        }
    }

    fn get_identifier_value(&self, name: &str) -> Result<LoxVal> {
        if let Some(val) = self.names.get(name) {
            Ok(val.clone())
        } else {
            Err(anyhow!(RuntimeError::UndefinedVariable(String::from(name))))
        }
    }

    fn assign_variable(&mut self, lvalue: Expr, rvalue: Expr) -> Result<()> {
        if let Expr::Variable(name) = lvalue {
            let rvalue = self.evaluate(rvalue);
            match (self.names.get_mut(&name), rvalue) {
                (Some(entry), Ok(value)) => {
                    *entry = value;
                    Ok(())
                }
                (None, _) => Err(anyhow!(RuntimeError::UndefinedVariable(name))),
                (_, Err(err)) => Err(err),
            }
        } else {
            Err(anyhow!("invalid lvalue expression"))
        }
    }

    fn evaluate(&mut self, expr: Expr) -> Result<LoxVal> {
        match expr {
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::Binary(l, op, r) => self.eval_binary(*l, op, *r),
            Expr::Unary(op, r) => self.eval_unary(op, *r),
            Expr::Assignment(lval, rval) => {
                if let Expr::Variable(name) = *lval {
                    let rval = self.evaluate(*rval)?;
                    if let Some(entry) = self.names.get_mut(&name) {
                        *entry = rval.clone();
                        Ok(rval)
                    } else {
                        Err(anyhow!(RuntimeError::UndefinedVariable(name)))
                    }
                } else {
                    Err(anyhow!(RuntimeError::InvalidLValue(*lval)))
                }
            }
            Expr::Variable(_) => todo!(),
            Expr::Literal(_) => todo!(),
        }
    }

    fn eval_binary(&mut self, l: Expr, op: Token, r: Expr) -> Result<LoxVal> {
        use syntax::Keyword::*;
        match op {
            // Simple Arithmetic operations
            Token::OneChar('*') => self.evaluate(l)? * self.evaluate(r)?,
            Token::OneChar('/') => self.evaluate(l)? / self.evaluate(r)?,
            Token::OneChar('+') => self.evaluate(l)? + self.evaluate(r)?,
            Token::OneChar('-') => self.evaluate(l)? - self.evaluate(r)?,

            // Comparison operators can only evaluate two numbers
            Token::OneChar('>') => match (self.evaluate(l)?, self.evaluate(r)?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x > y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::OneChar('<') => match (self.evaluate(l)?, self.evaluate(r)?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x < y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('>') => match (self.evaluate(l)?, self.evaluate(r)?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x >= y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('<') => match (self.evaluate(l)?, self.evaluate(r)?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x <= y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },

            // Equality operators are more permissive
            Token::CharThenEqual('=') => {
                Ok(LoxVal::Boolean(self.evaluate(l)? == self.evaluate(r)?))
            }
            Token::CharThenEqual('!') => {
                Ok(LoxVal::Boolean(self.evaluate(l)? != self.evaluate(r)?))
            }

            // The Lox Logical `and` and `or` operators work on truthiness and return the operands by value, rather than a boolean
            // Both short circuit if the left operand determines the value of the expression, returning the left object
            // This 'Just Works' for boolean values, but is a good thing to know if you're trying to compare other objects
            // For instance, an `and` statement with the left operand being truthy will always return the right operand whether it's truthy or not
            Token::Keyword(And) => {
                let left = self.evaluate(l)?;
                if !left.truthy() {
                    Ok(left)
                } else {
                    self.evaluate(r)
                }
            }
            Token::Keyword(Or) => {
                let left = self.evaluate(l)?;
                if left.truthy() {
                    Ok(left)
                } else {
                    self.evaluate(r)
                }
            }
            _ => Err(anyhow!("{} is not a valid binary operator", op)),
        }
    }

    fn eval_unary(&mut self, op: Option<Token>, right: Expr) -> Result<LoxVal> {
        if let Some(t) = op {
            match (t, self.evaluate(right)?) {
                (Token::OneChar('!'), o) => Ok(LoxVal::Boolean(!o.truthy())),
                (Token::OneChar('-'), LoxVal::Number(n)) => Ok(LoxVal::Number(-n)),
                (Token::OneChar('-'), o) => Err(anyhow!("cannot negate {o}")),
                (t, _) => Err(anyhow!("{t} is not a valid unary operator")),
            }
        } else {
            self.evaluate(right)
        }
    }

    fn interpret(&mut self, stmt: Statement) -> Result<()> {
        use syntax::Declaration::*;
        match stmt {
            Statement::Declaration(Variable(name, assignment)) => todo!(),
            Statement::Print(expr) => todo!(),
            Statement::Expression(expr) => match expr {
                Expr::Grouping(e) => todo!(),
                Expr::Binary(_, _, _) => todo!(),
                Expr::Unary(_, _) => todo!(),
                Expr::Assignment(name, expr) => todo!(),
                Expr::Variable(_) => todo!(),
                Expr::Literal(_) => todo!(),
            },
        }

        Ok(())
    }
}

#[derive(Debug, Parser)]
pub struct LoxArgs {
    /// Path to complete lox file.
    /// if empty opens a lox prompt.
    pub path: Option<String>,
}

pub fn run_file(path: &str) -> Result<()> {
    let mut file = File::open(Path::new(path)).expect("valid files only");
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    run(&source)
}

pub fn run_prompt() -> Result<()> {
    let mut input = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().expect("new prompt failed");
        if std::io::stdin().read_line(&mut input).is_ok() {
            if input.is_empty() {
                break;
            } else {
                run(&input).unwrap();
            }
        }
    }

    Ok(())
}

fn run(source: &str) -> Result<()> {
    let mut interpreter = Interpreter::new();
    // for r in tokens.iter() {
    //     match r {
    //         // just printing for now
    //         ((row, col), Ok(t)) => println!("{row}, {col}: {t}"),
    //         ((row, col), Err(e)) => println!("{row}, {col}: {e}"),
    //     }
    // }

    let parsed = parse(source);

    for s in parsed {
        println!("{s}");
    }
    // let vals: Vec<_> = exprs
    //     .into_iter()
    //     .map(|Parsed(i, e)| (i, e.and_then(|e| e.evaluate())))
    //     .collect();
    // for val in vals {
    //     println!("{}", val.1.unwrap());
    // }
    Ok(())
}
