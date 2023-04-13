use anyhow::{anyhow, Result};

#[derive(Debug, PartialEq)]
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

    pub fn is_literal(&self) -> bool {
        use Keyword::*;
        matches!(
            self,
            Token::String(_)
                | Token::Number(_)
                | Token::Keyword(True)
                | Token::Keyword(False)
                | Token::Keyword(Nil)
        )
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

/// Simple wrapper type for Lox values
/// So that we can take advantage of Rust's type system and built in behavior when possible
/// But have the freedom to diverge and customize where necessary
#[derive(Debug)]
pub enum LoxVal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl LoxVal {
    /// Lox's rules for conversion into boolean values are simple: False and Nil are falsey, any other value (even 0) is truthy
    fn truthy(&self) -> bool {
        match self {
            LoxVal::String(_) => true,
            LoxVal::Number(_) => true,
            LoxVal::Boolean(b) => *b,
            LoxVal::Nil => false,
        }
    }
}

impl std::fmt::Display for LoxVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxVal::String(s) => write!(f, "{s}"),
            LoxVal::Number(n) => write!(f, "{n}"),
            LoxVal::Boolean(_) => todo!(),
            LoxVal::Nil => todo!(),
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
            (LoxVal::String(_), _) | (_, LoxVal::String(_)) => {
                Ok(LoxVal::String(format!("{self}{rhs}")))
            }
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x + y)),
            _ => Err(anyhow!("cannot add {self} and {rhs}")),
        }
    }
}

// Subtraction
impl std::ops::Sub<LoxVal> for LoxVal {
    type Output = Result<LoxVal>;
    fn sub(self, rhs: LoxVal) -> Self::Output {
        match (&self, &rhs) {
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x - y)),
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
            (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Number(x / y)),
            _ => Err(anyhow!("cannot divide {self} and {rhs}")),
        }
    }
}

/// Equality operators can be called freely on Lox values
/// But they can only possibly be deemed equal if they are of the same type
impl PartialEq for LoxVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxVal::Boolean(l), LoxVal::Boolean(r)) => l == r,
            (LoxVal::Number(l), LoxVal::Number(r)) => l == r,
            (LoxVal::String(l), LoxVal::String(r)) => l == r,
            (LoxVal::Nil, LoxVal::Nil) => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for LoxVal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LoxVal::Number(x), LoxVal::Number(y)) => x.partial_cmp(y),
            _ => None,
        }
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

#[derive(Debug)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Token(Token),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Token(t) => write!(f, "{t}"),
            Expr::Grouping(e) => write!(f, "{e}"),
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

impl BinaryExpr {
    pub fn evaluate(self) -> Result<LoxVal> {
        use super::syntax::Keyword::*;
        match self.operator {
            // Simple Arithmetic operations
            Token::OneChar('*') => self.left.evaluate()? * self.right.evaluate()?,
            Token::OneChar('/') => self.left.evaluate()? / self.right.evaluate()?,
            Token::OneChar('+') => self.left.evaluate()? + self.right.evaluate()?,
            Token::OneChar('-') => self.left.evaluate()? - self.right.evaluate()?,

            // Comparison operators can only evaluate two numbers
            Token::OneChar('>') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x > y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::OneChar('<') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x < y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('>') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x >= y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },
            Token::CharThenEqual('<') => match (self.left.evaluate()?, self.right.evaluate()?) {
                (LoxVal::Number(x), LoxVal::Number(y)) => Ok(LoxVal::Boolean(x <= y)),
                (LoxVal::Number(_), val) | (val, LoxVal::Number(_)) => {
                    Err(anyhow!("{val} is not a valid number for comparison"))
                }
                (val1, val2) => Err(anyhow!("cannot compare {val1} with {val2}")),
            },

            // Equality operators are more permissive
            Token::CharThenEqual('=') => Ok(LoxVal::Boolean(
                self.left.evaluate()? == self.right.evaluate()?,
            )),
            Token::CharThenEqual('!') => Ok(LoxVal::Boolean(
                self.left.evaluate()? != self.right.evaluate()?,
            )),

            // The Lox Logical `and` and `or` operators work on truthiness and return the operands by value, rather than a boolean
            // Both short circuit if the left operand determines the value of the expression, returning the left object
            // This 'Just Works' for boolean values, but is a good thing to know if you're trying to compare other objects
            // For instance, an `and` statement with the left operand being truthy will always return the right operand whether it's truthy or not
            Token::Keyword(And) => {
                let left = self.left.evaluate()?;
                if !left.truthy() {
                    Ok(left)
                } else {
                    self.right.evaluate()
                }
            }
            Token::Keyword(Or) => {
                let left = self.left.evaluate()?;
                if left.truthy() {
                    Ok(left)
                } else {
                    self.right.evaluate()
                }
            }
            _ => Err(anyhow!("{} is not a valid binary operator", self.operator)),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    operator: Option<Token>,
    right: Expr,
}

impl UnaryExpr {
    pub fn evaluate(self) -> Result<LoxVal> {
        if let Some(t) = self.operator {
            match (t, self.right.evaluate()?) {
                (Token::OneChar('!'), o) => Ok(LoxVal::Boolean(!o.truthy())), // Might want truthy() to return a LoxVal instead of a native bool
                (Token::OneChar('-'), LoxVal::Number(n)) => Ok(LoxVal::Number(-n)),
                (Token::OneChar('-'), o) => Err(anyhow!("cannot negate {o}")),
                (t, _) => Err(anyhow!("{t} is not a valid unary operator")),
            }
        } else {
            self.right.evaluate()
        }
    }
}

impl Expr {
    // Constructors to avoid Box::new() for every constructed expression
    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Binary(Box::new(BinaryExpr {
            left,
            operator,
            right,
        }))
    }
    pub fn unary(operator: Option<Token>, right: Expr) -> Self {
        Self::Unary(Box::new(UnaryExpr { operator, right }))
    }
    pub fn group(self) -> Self {
        Self::Grouping(Box::new(self))
    }

    // All valid Lox expressions evaluate to a single value
    pub fn evaluate(self) -> Result<LoxVal> {
        use super::syntax::Keyword::*;
        match self {
            Expr::Grouping(e) => e.evaluate(),
            Expr::Binary(b) => b.evaluate(),
            Expr::Unary(u) => u.evaluate(),
            Expr::Token(o) => match o {
                Token::String(s) => Ok(LoxVal::String(s)),
                Token::Number(n) => Ok(LoxVal::Number(n)),
                Token::Identifier(s) => todo!(),
                Token::Keyword(True) => Ok(LoxVal::Boolean(true)),
                Token::Keyword(False) => Ok(LoxVal::Boolean(false)),
                Token::Keyword(Nil) => Ok(LoxVal::Nil),
                Token::Keyword(This) => todo!(),
                Token::Keyword(Class) => todo!(),
                Token::Keyword(_) => todo!(),
                _ => todo!(),
            },
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Declaration(String, Expr),
    Expression(Expr),
    Print(Expr),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(e) | Self::Print(e) => write!(f, "{e}"),
            _ => todo!(),
        }
    }
}
