mod parser;
mod syntax;

use parser::parse;
use syntax::{Expr, Function, LoxVal, Statement, Token};

use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{anyhow, Result};
use clap::Parser;
use hash_chain::ChainMap;

#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    InvalidLValue(String),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::UndefinedVariable(name) => write!(f, "undefined variable '{name}'"),
            RuntimeError::InvalidLValue(expr) => write!(f, "{expr} is not a valid lvalue"),
        }
    }
}

pub struct Interpreter {
    names: ChainMap<String, LoxVal>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { names: ChainMap::new(HashMap::new()) }
    }

    fn get_cloned(&self, name: &str) -> Option<LoxVal> {
        self.names.get(name).cloned()
    }

    #[inline]
    fn get_mut(&mut self, name: &str) -> Option<&mut LoxVal> {
        self.names.get_mut(name)
    }

    #[inline]
    fn open_scope(&mut self) {
        self.names.new_child();
    }

    #[inline]
    fn close_scope(&mut self) {
        self.names.remove_child();
    }

    // Probably want to return references to LoxObj
    fn evaluate(&mut self, expr: &Expr) -> Result<LoxVal> {
        match expr {
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Binary(l, op, r) => self.eval_binary(l, op, r),
            Expr::Unary(op, r) => self.eval_unary(op, r),
            // Assignments evaluate to the right hand side for the purposes of print
            Expr::Assignment(_, rval) => self.evaluate(rval),
            Expr::Variable(name) => self
                .get_cloned(name)
                .ok_or(anyhow!(RuntimeError::UndefinedVariable(name.clone()))),
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Call(callee, args) => self.eval_call(callee, args),
        }
    }

    // TODO: Get rid of cloning
    fn eval_call(&mut self, callee: &Box<Expr>, args: &[Expr]) -> Result<LoxVal> {
        if let Expr::Variable(s) = callee.as_ref()
        && s == "clock" {
            return Ok(LoxVal::Number(SystemTime::elapsed(&UNIX_EPOCH).unwrap().as_millis() as f64 / 1000.0));
        }
        if let LoxVal::Function(function) = self.evaluate(callee)? {
            self.open_scope();
            for (param, arg) in std::iter::zip(&function.params, args) {
                // The only way this will error is if the supplied argument is a syntactically valid expr that doesn't evaluate successfully
                let val = self.evaluate(arg)?;
                if let Err(e) = self.interpret(&Statement::VarDec(
                    String::from(param),
                    Some(Expr::Literal(val)),
                )) {
                    self.close_scope();
                    return Err(e);
                }
            }
            let result = self.interpret(&function.body);
            self.close_scope();
            match result {
                Ok(o) => Ok(o.into()),
                Err(e) => Err(e),
            }
        } else {
            Err(anyhow!("{callee} is not a Lox callable!"))
        }
    }

    fn eval_binary(&mut self, l: &Expr, op: &Token, r: &Expr) -> Result<LoxVal> {
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
                if !left.is_truthy() {
                    Ok(left)
                } else {
                    self.evaluate(r)
                }
            }
            Token::Keyword(Or) => {
                let left = self.evaluate(l)?;
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.evaluate(r)
                }
            }
            _ => Err(anyhow!("{} is not a valid binary operator", op)),
        }
    }

    fn eval_block(&mut self, stmts: &[Statement]) -> Result<Option<LoxVal>> {
        self.open_scope();
        for s in stmts {
            if let Some(obj) = self.interpret(s)? {
                self.close_scope();
                return Ok(Some(obj));
            }
        }
        self.close_scope();
        Ok(None)
    }

    fn eval_unary(&mut self, op: &Token, right: &Expr) -> Result<LoxVal> {
        match (op, self.evaluate(right)?) {
            (Token::OneChar('!'), o) => Ok(LoxVal::Boolean(!o.is_truthy())),
            (Token::OneChar('-'), LoxVal::Number(n)) => Ok(LoxVal::Number(-n)),
            (Token::OneChar('-'), o) => Err(anyhow!("cannot negate {o}")),
            (t, _) => Err(anyhow!("{t} is not a valid unary operator")),
        }
    }

    // TODO: Remove cloning
    fn interpret(&mut self, stmt: &Statement) -> Result<Option<LoxVal>> {
        match stmt {
            Statement::VarDec(name, assignment) => {
                let value =
                    self.evaluate(assignment.as_ref().unwrap_or(&Expr::Literal(LoxVal::Nil)))?;
                self.names.insert(name.clone(), value);
            }
            Statement::Print(expr) => println!("{}", self.evaluate(expr)?),
            Statement::Expression(expr) => match expr {
                Expr::Assignment(lval, rval) => {
                    if let Expr::Variable(ref name) = **lval {
                        let rval = self.evaluate(rval)?;
                        if let Some(entry) = self.get_mut(name) {
                            *entry = rval;
                        } else {
                            return Err(anyhow!(RuntimeError::UndefinedVariable(name.clone())));
                        }
                    } else {
                        return Err(anyhow!(RuntimeError::InvalidLValue(lval.to_string())));
                    }
                }
                // Statements like `False;` or `5 * 23 or True;` do not do anything. They are not errors, just no-ops
                _ => {
                    self.evaluate(expr)?;
                }
            },
            Statement::Block(stmts) => return self.eval_block(stmts),
            Statement::If(cond, if_exec, else_exec) => {
                if self.evaluate(cond)?.is_truthy()
                && let Some(o) = self.interpret(if_exec)? {
                    return Ok(Some(o));
                } else if let Some(stmt) = else_exec
                && let Some(o) = self.interpret(stmt)? {
                    return Ok(Some(o));
                }
            }
            Statement::While(cond, exec) => {
                while self.evaluate(cond)?.is_truthy() {
                    if let Some(o) = self.interpret(exec)? {
                        return Ok(Some(o));
                    }
                }
            }
            Statement::FunDec(name, params, body) => {
                self.names.insert(
                    name.clone(),
                    LoxVal::Function(Box::new(Function {
                        params: params.clone(),
                        body: body.clone(),
                    })),
                );
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    return Ok(Some(self.evaluate(expr)?));
                }
            }
        }
        // Unless we return early with a value, interpreting a statement should not end by returning anything
        Ok(None)
    }

    pub fn run(&mut self, source: &str) -> Result<()> {
        for stmt in parse(source) {
            match stmt.1 {
                Ok(stmt) => {
                    self.interpret(&stmt)?;
                }
                Err(err) => println!("{err}"),
            }
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
    let mut interpreter = Interpreter::new();
    file.read_to_string(&mut source)?;
    interpreter.run(&source)
}

pub fn run_prompt() -> Result<()> {
    let mut interpreter = Interpreter::new();
    let mut input = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().expect("new prompt failed");
        if std::io::stdin().read_line(&mut input).is_ok() {
            if input == "\n" {
                break;
            } else {
                interpreter.run(&input)?;
                input.clear();
            }
        }
    }

    Ok(())
}
