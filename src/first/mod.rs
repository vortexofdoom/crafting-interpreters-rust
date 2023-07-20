mod parser;
mod syntax;

use parser::parse;
use syntax::{Expr, LoxVal, Statement, Token};

use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

struct Resolver {
    scopes: Vec<(HashMap<String, (bool, usize)>, usize)>,
    curr_fun: Option<FunctionType>,
}

impl Resolver {
    fn resolve(stmts: &[Statement], env: &mut Scope) -> Result<()> {
        let mut resolver = Self {
            scopes: vec![],
            curr_fun: None,
        };

        for stmt in stmts {
            resolver.resolve_stmt(stmt, env)?;
        }
        Ok(())
    }

    fn open(&mut self) {
        self.scopes.push((HashMap::new(), 0));
    }

    fn close(&mut self) {
        self.scopes.pop();
    }

    fn resolve_declare(&mut self, name: &str) -> Result<()> {
        // If there is no local scope, this is a global variable.
        if let Some(scope) = self.scopes.last_mut() {
            if scope
                .0
                .insert(String::from(name), (false, scope.1))
                .is_some()
            {
                return Err(anyhow!(RuntimeError::AlreadyDeclared(String::from(name))));
            }
            scope.1 += 1;
        }

        Ok(())
    }

    fn resolve_define(&mut self, name: &str) -> Result<()> {
        if let Some(v) = self.scopes.last_mut().and_then(|s| s.0.get_mut(name)) {
            v.0 = true;
        }
        Ok(())
    }

    fn get_name(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Variable(name) => Some(name),
            _ => None,
        }
    }

    fn resolve_local(&mut self, expr: &Expr, env: &mut Scope) -> Result<()> {
        if !self.scopes.is_empty() {
            let name = Self::get_name(expr).ok_or(anyhow!("not a valid name expression"))?;
            self.resolve_name(name)?;
            for i in (0..self.scopes.len()).rev() {
                if let Some((_, n)) = self.scopes[i].0.get(name) {
                    env.locals.insert(expr as *const Expr as usize, (i, *n));
                    break;
                }
            }
        }
        Ok(())
    }

    fn resolve_name(&mut self, name: &str) -> Result<()> {
        match self.scopes.last().and_then(|(m, _)| m.get(name)) {
            Some((false, _)) => Err(anyhow!(
                "Can't read local variable {name} in its own initializer."
            )),
            _ => Ok(()),
        }
    }

    fn resolve_stmt(&mut self, stmt: &Statement, env: &mut Scope) -> Result<()> {
        match stmt {
            Statement::VarDec(name, val) => {
                self.resolve_declare(name)?;
                if let Some(expr) = val {
                    self.resolve_expr(expr, env)?;
                }
                self.resolve_define(name)
            }
            Statement::FunDec(n, f) => {
                if let Some(name) = n {
                    self.resolve_declare(name)?;
                    self.resolve_define(name)?;
                }

                self.resolve_function(FunctionType::Function, f, env)
            }
            Statement::Expression(e) => self.resolve_expr(e, env),
            Statement::Print(e) => self.resolve_expr(e, env),
            Statement::Block(stmts) => {
                self.open();
                for stmt in stmts {
                    self.resolve_stmt(stmt, env)?;
                }
                self.close();
                Ok(())
            }
            Statement::If(cond, if_exec, else_exec) => {
                self.resolve_expr(cond, env)?;
                self.resolve_stmt(if_exec, env)?;
                if let Some(s) = else_exec {
                    self.resolve_stmt(s, env)?;
                }
                Ok(())
            }
            Statement::While(cond, exec) => {
                self.resolve_expr(cond, env)?;
                self.resolve_stmt(exec, env)
            }
            Statement::Return(e) => match e {
                Some(e) => match self.curr_fun {
                    None => Err(anyhow!("can only return from within functions")),
                    _ => self.resolve_expr(e, env),
                },
                None => Ok(()),
            },
        }
    }

    fn resolve_expr(&mut self, expr: &Expr, env: &mut Scope) -> Result<()> {
        match expr {
            Expr::Grouping(e) => self.resolve_expr(&e, env),
            Expr::Binary(l, _, r) => {
                self.resolve_expr(&l, env)?;
                self.resolve_expr(&r, env)
            }
            Expr::Unary(_, r) => self.resolve_expr(&r, env),
            Expr::Assignment(n, e) => {
                self.resolve_expr(&e, env)?;
                self.resolve_local(n, env)
            }
            Expr::Call(callee, args) => {
                self.resolve_expr(&callee, env)?;
                for arg in args {
                    self.resolve_expr(&arg, env)?;
                }
                Ok(())
            }
            Expr::Variable(name) => match self.scopes.last().and_then(|(m, _)| m.get(name)) {
                Some((false, _)) => Err(anyhow!(
                    "Can't read local variable {name} in its own initializer."
                )),
                _ => self.resolve_local(expr, env),
            },
            Expr::Literal(_) => Ok(()),
        }
    }

    fn resolve_function(
        &mut self,
        fun_type: FunctionType,
        fun: &Function,
        env: &mut Scope,
    ) -> Result<()> {
        let enclosing_fun = self.curr_fun;
        self.curr_fun = Some(fun_type);
        self.open();
        for param in &fun.params {
            self.resolve_declare(param)?;
            self.resolve_define(param)?;
        }
        self.resolve_stmt(&fun.body, env)?;
        self.close();
        self.curr_fun = enclosing_fun;
        Ok(())
    }
}

struct Scope {
    globals: HashMap<String, LoxVal>,
    locals: HashMap<usize, (usize, usize)>,
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

    fn get(&self, name: &String, expr: &Expr) -> Option<&LoxVal> {
        match self.locals.get(&(expr as *const Expr as usize)) {
            Some((depth, idx)) => {
                Some(&self.scopes[*depth][*idx])
            },
            None => self.globals.get(name),
        }
    }

    fn get_mut(&mut self, name: &str, expr: &Expr) -> Option<&mut LoxVal> {
        match self.locals.get(&(expr as *const Expr as usize)) {
            Some((depth, idx)) => {
                let depth = self.scopes.len() - 1 - depth;
                Some(&mut self.scopes[depth][*idx])
            }
            None => self.globals.get_mut(name),
        }
    }

    fn insert(&mut self, name: &str, val: LoxVal) {
        if self.scopes.is_empty() {
            self.globals.insert(String::from(name), val);
        } else {
            self.scopes
                .last_mut()
                .expect("just checked if globals was empty?")
                .push(val);
        }
    }

    fn resolve(&mut self, stmts: &[Statement]) -> Result<()> {
        Resolver::resolve(stmts, self)
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
            Expr::Grouping(e) => self.evaluate(&e),
            Expr::Binary(l, op, r) => self.eval_binary(l, op, r),
            Expr::Unary(op, r) => self.eval_unary(&op, r),
            // Assignments evaluate to the right hand side for the purposes of print
            Expr::Assignment(_, rval) => self.evaluate(rval),
            Expr::Variable(name) => Ok(self.scope.get(name, expr).cloned().unwrap_or(LoxVal::Nil)),
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Call(callee, args) => self.eval_call(callee, args),
        }
    }

    fn eval_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<LoxVal> {
        // TODO: This is a hack
        if let Expr::Variable(s) = callee
        && s == "clock" {
            return Ok(LoxVal::Number(SystemTime::elapsed(&UNIX_EPOCH).unwrap().as_millis() as f64 / 1000.0));
        }
        if let LoxVal::Function(function) = self.evaluate(callee)? {
            self.scope.open();
            for (param, arg) in std::iter::zip(&function.params, args.as_ref()) {
                // The only way this will error is if the supplied argument is a syntactically valid expr that doesn't evaluate successfully
                let val = self.evaluate(arg)?;
                self.scope.insert(&param, val);
            }
            let result = LoxVal::from(self.interpret(&function.body)?);
            self.scope.close();
            Ok(result)
        } else {
            Err(anyhow!("{} is not a Lox callable!", *callee))
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
                    self.evaluate(assignment.as_ref().unwrap_or(&ByAddress(Box::new(Expr::Literal(LoxVal::Nil)))))?;
                self.scope.insert(name, value);
            }
            Statement::Print(expr) => println!("{}", self.evaluate(expr)?),
            Statement::Expression(expr) => match expr {
                Expr::Assignment(lval, rval) => {
                    if let Expr::Variable(name) = &**lval {
                        let rval = self.evaluate(&rval)?;
                        if let Some(entry) = self.scope.get_mut(&name, &lval) {
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
                self.scope.resolve(&stmts)?;
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
