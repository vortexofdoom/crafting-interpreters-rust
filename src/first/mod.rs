mod parser;
mod syntax;

use parser::parse;
use syntax::{Expr, LoxVal, Statement, Token};

use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::ops::Deref;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{anyhow, Result};
use by_address::ByAddress;
use clap::Parser;

use self::syntax::Function;

#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    AlreadyDeclared(String),
    InvalidLValue(String),
}

enum FunctionType {
    Function,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::UndefinedVariable(name) => write!(f, "undefined variable '{name}'"),
            RuntimeError::AlreadyDeclared(name) => {
                write!(f, "Already a variable named {name} in this scope.")
            }
            RuntimeError::InvalidLValue(expr) => write!(f, "{expr} is not a valid lvalue"),
        }
    }
}

struct Resolver<'a> {
    scopes: Vec<(HashMap<String, (bool, usize)>, usize)>,
    env: &'a mut Scope,
    curr_fun: Option<FunctionType>,
}

impl<'a> Resolver<'a> {
    fn resolve(env: &'a mut Scope, stmts: &[Statement]) -> Result<()> {
        let mut resolver = Self {
            scopes: vec![(HashMap::new(), 0)],
            env,
            curr_fun: None,
        };

        for stmt in stmts {
            //println!("{stmt}");
            resolver.resolve_stmt(stmt)?;
        }
        // println!("{:#?}", resolver.env.locals);
        // println!("{}", resolver.env.scopes[0][0]);
        Ok(())
    }

    fn open(&mut self) {
        self.scopes.push((HashMap::new(), 0));
    }

    fn close(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) -> Result<()> {
        // If there is no local scope, this is a global variable.
        //println!("{name}");
        if let Some(scope) = self.scopes.last_mut() {
            println!("declaring {name} in local scope");
            if scope
                .0
                .insert(String::from(name), (false, scope.1))
                .is_some()
            {
                return Err(anyhow!(RuntimeError::AlreadyDeclared(String::from(name))));
            }
            scope.1 += 1;
        }

        println!("{:?}", self.scopes.last());
        Ok(())
    }

    fn define(&mut self, name: &str) -> Result<()> {
        // println!("{name}");
        if let Some(v) = self.scopes.last_mut().and_then(|s| s.0.get_mut(name)) {
            v.0 = true;
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &str, expr: &'a Expr) -> Result<()> {
        println!("local: {expr}");
        if !self.scopes.is_empty() {
            let len = self.scopes.len() - 1;
            for i in (0..len).rev() {
                if let Some((_, n)) = self.scopes[i].0.get(name) {
                    println!("hello! {name}");
                    self.env.locals.insert(expr as *const Expr, (i, *n));
                    break;
                }
            }
        }
        println!("local {name} {:?}", self.env.locals.get(&(expr as *const Expr)));
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &'a Statement) -> Result<()> {
        match stmt {
            Statement::VarDec(n, v) => {
                self.declare(n)?;
                if let Some(e) = v {
                    self.resolve_expr(e)?;
                }
                self.define(n)
            }
            Statement::FunDec(n, f) => {
                if let Some(n) = n {
                    println!("fun {n}");
                    self.declare(n)?;
                    self.define(n)?;
                }

                self.resolve_function(f)
            }
            Statement::Expression(e) => self.resolve_expr(e),
            Statement::Print(e) => self.resolve_expr(e),
            Statement::Block(stmts) => {
                self.open();
                for stmt in stmts {
                    self.resolve_stmt(stmt)?;
                }
                self.close();
                Ok(())
            }
            Statement::If(cond, if_exec, else_exec) => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(if_exec)?;
                if let Some(s) = else_exec {
                    self.resolve_stmt(s)?;
                }
                Ok(())
            }
            Statement::While(cond, exec) => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(exec)
            }
            Statement::Return(e) => match e {
                Some(e) => self.resolve_expr(e),
                None => Ok(()),
            },
        }
    }

    fn resolve_expr(&mut self, expr: &'a Expr) -> Result<()> {
        match expr {
            Expr::Grouping(e) => self.resolve_expr(e),
            Expr::Binary(l, _, r) => {
                self.resolve_expr(l)?;
                self.resolve_expr(r)
            }
            Expr::Unary(_, r) => self.resolve_expr(r),
            Expr::Assignment(n, e) => {
                self.resolve_expr(e)?;
                self.resolve_expr(n)
            }
            Expr::Call(callee, args) => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::Variable(name) => {
                println!("variable: {name}");
                match self.scopes.last().and_then(|(m, _)| m.get(name)) {
                    Some((false, _)) => Err(anyhow!(
                        "Can't read local variable {name} in its own initializer."
                    )),
                    _ => self.resolve_local(name, expr),
                }
            }
            Expr::Literal(_) => Ok(()),
        }
    }

    fn resolve_function(&mut self, fun: &'a Function) -> Result<()> {
        self.open();
        for param in fun.params.iter() {
            self.declare(param)?;
            self.define(param)?;
        }
        self.resolve_stmt(&fun.body)?;
        self.close();
        Ok(())
    }
}

struct Scope {
    globals: HashMap<String, LoxVal>,
    locals: HashMap<*const Expr, (usize, usize)>,
    scopes: Vec<Vec<LoxVal>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            globals: HashMap::new(),
            locals: HashMap::new(),
            scopes: vec![],
        }
    }

    fn get(&self, name: &str, expr: &Expr) -> Option<&LoxVal> {
        println!("get: {}", LoxVal::from(self.globals.get(name).cloned()));
        match self.locals.get(&(expr as *const Expr)) {
            Some((depth, idx)) => Some(&self.scopes[self.scopes.len() - 1 - depth][*idx]),
            None => self.globals.get(name),
        }
    }

    fn get_mut(&mut self, name: &str, expr: &Expr) -> Option<&mut LoxVal> {
        match self.locals.get(&(expr as *const Expr)) {
            Some((depth, idx)) => {
                let depth = self.scopes.len() - 1 - depth;
                Some(&mut self.scopes[depth][*idx])
            }
            None => self.globals.get_mut(name),
        }
    }

    fn insert(&mut self, name: &str, val: LoxVal) {
        println!("inserting {name} in layer {}", self.scopes.len().saturating_sub(1));
        if self.scopes.is_empty() {
            self.globals.insert(String::from(name), val);
        } else {
            self.scopes
                .last_mut()
                .expect("just checked if globals was empty?")
                .push(val);
            println!("{:?}", self.scopes.last().unwrap());
        }
    }

    #[inline]
    fn open(&mut self) {
        self.scopes.push(vec![]);
    }

    #[inline]
    fn close(&mut self) {
        self.scopes.pop();
    }
}

pub struct Interpreter {
    scope: Scope,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scope: Scope::new(),
        }
    }

    // Probably want to return references to LoxObj
    fn evaluate(&mut self, expr: &Expr) -> Result<LoxVal> {
        match expr {
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Binary(l, op, r) => self.eval_binary(l, op, r),
            Expr::Unary(op, r) => self.eval_unary(op, r),
            // Assignments evaluate to the right hand side for the purposes of print
            Expr::Assignment(_, rval) => self.evaluate(rval),
            Expr::Variable(name) => Ok(self.scope.get(name, expr).cloned().unwrap_or(LoxVal::Nil)),
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Call(callee, args) => self.eval_call(callee, args),
        }
    }

    fn eval_call(&mut self, callee: &Box<Expr>, args: &[Expr]) -> Result<LoxVal> {
        // TODO: This is a hack
        if let Expr::Variable(s) = callee.as_ref()
        && s == "clock" {
            return Ok(LoxVal::Number(SystemTime::elapsed(&UNIX_EPOCH).unwrap().as_millis() as f64 / 1000.0));
        }
        if let LoxVal::Function(function) = self.evaluate(callee)? {
            self.scope.open();
            for (param, arg) in std::iter::zip(&function.params, args) {
                // The only way this will error is if the supplied argument is a syntactically valid expr that doesn't evaluate successfully
                let val = self.evaluate(arg)?;
                if let Err(e) = self.interpret(&Statement::VarDec(
                    String::from(param),
                    Some(Expr::Literal(val)),
                )) {
                    self.scope.close();
                    return Err(e);
                }
            }
            let result = match self.interpret(&function.body) {
                Ok(o) => Ok(LoxVal::from(o)),
                Err(e) => Err(e),
            };
            self.scope.close();
            result
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
        self.scope.open();
        for s in stmts {
            if let Some(obj) = self.interpret(s)? {
                self.scope.close();
                return Ok(Some(obj.clone()));
            }
        }
        self.scope.close();
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
                self.scope.insert(name, value);
            }
            Statement::Print(expr) => println!("{}", self.evaluate(expr)?),
            Statement::Expression(expr) => match expr {
                Expr::Assignment(lval, rval) => {
                    if let Expr::Variable(ref name) = **lval {
                        let rval = self.evaluate(rval)?;
                        if let Some(entry) = self.scope.get_mut(name, &lval) {
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
            Statement::FunDec(name, fun) => {
                if let Some(name) = name {
                    self.scope.insert(name, LoxVal::Function(fun.clone()));
                } else {
                    return Ok(Some(LoxVal::Function(fun.clone())));
                }
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
        match parse(source) {
            Ok(stmts) => {
                Resolver::resolve(&mut self.scope, &stmts)?;
                for s in &stmts {
                    self.interpret(s)?;
                }
            }
            Err(errs) => errs.iter().for_each(|e| println!("{e}")),
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
