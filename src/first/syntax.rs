use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{anyhow, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    OneChar(char),
    CharThenEqual(char),
    Identifier(String),
    String(String),
    Number(f64),
    Keyword(Keyword),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::OneChar(c) => write!(f, "{c}"),
            Token::CharThenEqual(c) => write!(f, "{c}="),
            Token::Identifier(s) => write!(f, "{s}"),
            Token::String(s) => write!(f, "{s}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::Keyword(k) => write!(f, "{k}"),
        }
    }
}

impl Token {
    pub fn is_binary_operator(&self) -> bool {
        use Keyword::*;
        matches!(
            self,
            Token::OneChar('-' | '!' | '+' | '*' | '/' | '>' | '<')
                | Token::CharThenEqual(_)
                | Token::Keyword(Or | And)
        )
    }

    pub fn is_unary_operator(&self) -> bool {
        matches!(self, Token::OneChar('-' | '!'))
    }

    pub fn try_convert_literal(self) -> Result<LoxVal, Self> {
        use Keyword::*;
        match self {
            Token::String(s) => Ok(LoxVal::String(s)),
            Token::Number(n) => Ok(LoxVal::Number(n)),
            Token::Keyword(True) => Ok(LoxVal::Boolean(true)),
            Token::Keyword(False) => Ok(LoxVal::Boolean(false)),
            Token::Keyword(Nil) => Ok(LoxVal::Nil),
            _ => Err(self),
        }
    }

    pub fn highest_valid_precedence(&self) -> Precedence {
        match self {
            Token::Keyword(Keyword::Or) => Precedence::Or,
            Token::Keyword(Keyword::And) => Precedence::Or,
            Token::CharThenEqual('=' | '!') => Precedence::Equality,
            Token::CharThenEqual('<' | '>') | Token::OneChar('<' | '>') => Precedence::Comparison,
            Token::OneChar('+') => Precedence::Term,
            Token::OneChar('*' | '/') => Precedence::Factor,
            Token::OneChar('-' | '!') => Precedence::Unary,
            Token::Identifier(_) | Token::String(_) | Token::Number(_) => Precedence::Primary,
            _ => todo!(),
        }
    }

    #[inline]
    pub fn matches_precedence(&self, precedence: Precedence) -> bool {
        self.highest_valid_precedence() >= precedence
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Keyword::And => "and",
            Keyword::Class => "class",
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::Fun => "fun",
            Keyword::For => "for",
            Keyword::If => "if",
            Keyword::Nil => "nil",
            Keyword::Or => "or",
            Keyword::Print => "print",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::This => "this",
            Keyword::True => "true",
            Keyword::Var => "var",
            Keyword::While => "while",
        };
        write!(f, "{s}")
    }
}

impl Keyword {
    pub fn get_from_str(s: &str) -> Option<Self> {
        use Keyword::*;
        match s {
            "and" => Some(And),
            "class" => Some(Class),
            "else" => Some(Else),
            "False" => Some(False),
            "fun" => Some(Fun),
            "for" => Some(For),
            "if" => Some(If),
            "nil" => Some(Nil),
            "or" => Some(Or),
            "print" => Some(Print),
            "return" => Some(Return),
            "super" => Some(Super),
            "this" => Some(This),
            "True" => Some(True),
            "var" => Some(Var),
            "while" => Some(While),
            _ => None,
        }
    }
}

/// Simple wrapper type for Lox values
/// so that we can take advantage of Rust's type system and built in behavior when possible,
/// but have the freedom to diverge and customize where necessary
#[derive(Debug, Clone)]
pub enum LoxVal {
    String(String),
    Number(f64),
    Boolean(bool),
    Function(Rc<Function>, Option<Rc<RefCell<Instance>>>), // Encompasses both methods and functions with an optional attached instance
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Nil,
}

// This just makes None semantically equivalent to Nil
impl From<Option<LoxVal>> for LoxVal {
    fn from(value: Option<LoxVal>) -> Self {
        match value {
            Some(o) => o,
            None => Self::Nil,
        }
    }
}

impl LoxVal {
    /// Lox's rules for conversion into boolean values are simple: False and Nil are "falsey", any other value (even 0) is "truthy"
    pub fn is_truthy(&self) -> bool {
        // might want to make this return a LoxVal::Boolean
        match self {
            Self::Boolean(b) => *b,
            Self::Nil => false,
            _ => true,
        }
    }

    pub fn new_class(
        name: String,
        super_class: Option<Rc<Class>>,
        methods: HashMap<String, Rc<Function>>,
    ) -> Self {
        Self::Class(Rc::new(Class {
            name,
            super_class,
            methods,
        }))
    }
}

impl std::fmt::Display for LoxVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Boolean(true) => write!(f, "True"),
            Self::Boolean(false) => write!(f, "False"),
            Self::Function(_, i) => match i {
                Some(i) => write!(f, "<method> on {}", i.borrow().class),
                None => write!(f, "<fun>"),
            },
            Self::Class(c) => write!(f, "{c}"),
            Self::Instance(i) => write!(f, "{}", i.borrow()),
            Self::Nil => write!(f, "nil"),
        }
    }
}

/// Addition
/// If either operand is a string, the remaining operand (if not already a string) is concatenated along with it from left to right as a string
/// Otherwise, both operands must be numbers.
impl std::ops::Add<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn add(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (Self::String(_), _) | (_, Self::String(_)) => Ok(Self::String(format!("{self}{rhs}"))),
            (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x + y)),
            _ => Err(anyhow!("cannot add {self} and {rhs}")),
        }
    }
}

// Subtraction
impl std::ops::Sub<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn sub(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x - y)),
            _ => Err(anyhow!("cannot subtract {rhs} from {self}")),
        }
    }
}

// Multiplication
impl std::ops::Mul<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn mul(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x * y)),
            _ => Err(anyhow!("cannot multiply {self} and {rhs}")),
        }
    }
}

// Division
impl std::ops::Div<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn div(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x / y)),
            _ => Err(anyhow!("cannot divide {self} and {rhs}")),
        }
    }
}

/// Equality operators can be called freely on Lox values
/// But they can only possibly be deemed equal if they are of the same type
impl PartialEq for LoxVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for LoxVal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(x), Self::Number(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub fun_type: FunctionType,
    pub params: Vec<String>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ClassType {
    Class,
    SubClass,
}

#[derive(Debug, PartialEq)]
pub struct Class {
    name: String,
    super_class: Option<Rc<Class>>,
    methods: HashMap<String, Rc<Function>>,
}

impl Class {
    pub fn get_method(&self, name: &str) -> Option<Rc<Function>> {
        match self.methods.get(name) {
            Some(method) => Some(method.clone()),
            None => self.super_class.as_ref().and_then(|sc| sc.get_method(name)),
        }
    }

    #[inline]
    pub fn get_super(&self) -> Option<Rc<Class>> {
        self.super_class.clone()
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, LoxVal>,
}

impl Instance {
    #[inline]
    pub fn new(class: Rc<Class>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            class: class.clone(),
            fields: HashMap::new(),
        }))
    }

    #[inline]
    pub fn get_field(&self, name: &str) -> Option<LoxVal> {
        self.fields.get(name).cloned()
    }

    #[inline]
    pub fn get_method(&self, name: &str) -> Option<Rc<Function>> {
        self.class.get_method(name)
    }

    pub fn get_super(&self) -> Option<Rc<Class>> {
        self.class.get_super()
    }

    pub fn insert(&mut self, name: &str, value: LoxVal) {
        self.fields.insert(String::from(name), value);
    }
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
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
    pub fn next_highest(&self) -> Self {
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Variable(String),
    Literal(LoxVal),
    This,
    Super,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(t) => write!(f, "{t}"),
            Expr::Grouping(e) => write!(f, "{e}"),
            Expr::Binary(l, op, r) => write!(f, "({l} {op} {r})"),
            Expr::Variable(s) => write!(f, "{s}"),
            Expr::Assignment(l, r) => write!(f, "{l} = {r}"),
            Expr::Call(l, args) => write!(f, "{l}({})", itertools::join(args, ", ")),
            Expr::Get(from, name) => write!(f, "{from}.{name}"),
            Expr::Unary(op, r) => write!(f, "({op}{r})"),
            Expr::This => write!(f, "this"),
            Expr::Super => write!(f, "super"),
        }
    }
}

impl Expr {
    // Constructors to avoid Box::new() for every constructed expression
    pub fn assignment(lvalue: Expr, rvalue: Expr) -> Self {
        Self::Assignment(Box::new(lvalue), Box::new(rvalue))
    }

    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }

    pub fn unary(operator: Token, right: Expr) -> Self {
        Self::Unary(operator, Box::new(right))
    }

    pub fn group(self) -> Self {
        Self::Grouping(Box::new(self))
    }

    pub fn fun_call(expr: Expr, args: Vec<Expr>) -> Self {
        Self::Call(Box::new(expr), args)
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Variable(_) | Expr::Get(_, _))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VarDec(String, Option<Expr>),
    FunDec(Option<String>, Rc<Function>),
    ClassDec(String, Option<Expr>, Vec<Statement>),
    Expression(Expr),
    Print(Expr),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    Return(Option<Expr>),
}

impl Statement {
    pub fn fun_dec(name: Option<String>, fun: Function) -> Self {
        Self::FunDec(name, Rc::new(fun))
    }

    pub fn if_stmt(cond: Expr, exec: Statement, else_exec: Option<Statement>) -> Self {
        Self::If(cond, Box::new(exec), else_exec.map(|s| Box::new(s)))
    }

    pub fn while_stmt(cond: Expr, exec: Statement) -> Self {
        Self::While(cond, Box::new(exec))
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VarDec(name, assign) => {
                if let Some(expr) = assign {
                    write!(f, "var {name} = {expr}")
                } else {
                    write!(f, "var {name}")
                }
            }
            Self::ClassDec(n, s, _) => {
                if let Some(s) = s {
                    write!(f, "class {n} < {s}")
                } else {
                    write!(f, "class {n}")
                }
            }
            Self::Expression(e) => write!(f, "Expr: {e}"),
            Self::Print(e) => write!(f, "Print: {e}"),
            Self::FunDec(name, fun) => {
                if let Some(name) = name {
                    write!(f, "fun {name}(")?;
                } else {
                    write!(f, "fun (")?;
                }
                for p in fun.params.iter() {
                    write!(f, "{p}, ")?;
                }
                write!(f, "): {}", fun.body)
            }
            Self::Block(stmts) => write!(f, "Block:\n{stmts:?}\nEnd Block"),
            Self::If(cond, if_exec, else_exec) => {
                if let Some(stmt) = else_exec {
                    write!(f, "if {cond}: {if_exec} else: {stmt}")
                } else {
                    write!(f, "if {cond}: {if_exec}")
                }
            }
            Self::While(cond, exec) => write!(f, "while {cond}: {exec}"),
            Self::Return(expr) => {
                if let Some(e) = expr {
                    write!(f, "return {e}")
                } else {
                    write!(f, "return nil")
                }
            }
        }
    }
}
