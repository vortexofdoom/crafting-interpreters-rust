use std::{iter::Peekable, ops::Index, any};

use anyhow::{anyhow, Result};
use enum_map::{Enum, EnumMap};

use super::{
    chunk::{Chunk, OpCode},
    scanner::{scan, Parsed, Token, TokenType},
    value::Value,
    InterpretError,
};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn next_highest(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::Primary,
        }
    }
}

type ParseFn<'a, T> = Option<fn(&mut Parser<'a, T>, bool) -> Result<()>>;

#[derive(Debug, Clone, Copy)]
struct ParseRule<'a, T: Iterator<Item = Parsed<Token<'a>>>>(
    ParseFn<'a, T>,
    ParseFn<'a, T>,
    Precedence,
);

#[rustfmt::skip]
impl<'a, T: Iterator<Item = Parsed<Token<'a>>>> ParseRule<'a, T> {
    const LEFT_PAREN: Self      = Self(Some(Parser::grouping), None, Precedence::None);
    const RIGHT_PAREN: Self     = Self(None, None, Precedence::None);
    const LEFT_BRACE: Self      = Self(None, None, Precedence::None);
    const RIGHT_BRACE: Self     = Self(None, None, Precedence::None);
    const COMMA: Self           = Self(None, None, Precedence::None);
    const DOT: Self             = Self(None, None, Precedence::None);
    const MINUS: Self           = Self(Some(Parser::unary), Some(Parser::binary), Precedence::Term);
    const PLUS: Self            = Self(None, Some(Parser::binary), Precedence::Term);
    const SEMICOLON: Self       = Self(None, None, Precedence::None);
    const SLASH: Self           = Self(None, Some(Parser::binary), Precedence::Factor);
    const STAR: Self            = Self(None, Some(Parser::binary), Precedence::Factor);
    const BANG: Self            = Self(Some(Parser::unary), None, Precedence::None);
    const BANG_EQUAL: Self      = Self(None, Some(Parser::binary), Precedence::Comparison);
    const EQUAL: Self           = Self(None, None, Precedence::None);
    const EQUAL_EQUAL: Self     = Self(None, Some(Parser::binary), Precedence::Comparison);
    const GREATER: Self         = Self(None, Some(Parser::binary), Precedence::Comparison);
    const GREATER_EQUAL: Self   = Self(None, Some(Parser::binary), Precedence::Comparison);
    const LESS: Self            = Self(None, Some(Parser::binary), Precedence::Comparison);
    const LESS_EQUAL: Self      = Self(None, Some(Parser::binary), Precedence::Comparison);
    const IDENTIFIER: Self      = Self(Some(Parser::variable), None, Precedence::None);
    const STRING: Self          = Self(Some(Parser::string), None, Precedence::None);
    const NUMBER: Self          = Self(Some(Parser::number), None, Precedence::None);
    const AND: Self             = Self(None, None, Precedence::None);
    const CLASS: Self           = Self(None, None, Precedence::None);
    const ELSE: Self            = Self(None, None, Precedence::None);
    const FALSE: Self           = Self(Some(Parser::literal), None, Precedence::None);
    const FOR: Self             = Self(None, None, Precedence::None);
    const FUN: Self             = Self(None, None, Precedence::None);
    const IF: Self              = Self(None, None, Precedence::None);
    const NIL: Self             = Self(Some(Parser::literal), None, Precedence::None);
    const OR: Self              = Self(None, None, Precedence::None);
    const PRINT: Self           = Self(None, None, Precedence::None);
    const RETURN: Self          = Self(None, None, Precedence::None);
    const SUPER: Self           = Self(None, None, Precedence::None);
    const THIS: Self            = Self(None, None, Precedence::None);
    const TRUE: Self            = Self(Some(Parser::literal), None, Precedence::None);
    const VAR: Self             = Self(None, None, Precedence::None);
    const WHILE: Self           = Self(None, None, Precedence::None);

    const RULES: [Self; 38] = [
        ParseRule::LEFT_PAREN,
        ParseRule::RIGHT_PAREN,
        ParseRule::LEFT_BRACE,
        ParseRule::RIGHT_BRACE,
        ParseRule::COMMA,
        ParseRule::DOT,
        ParseRule::MINUS,
        ParseRule::PLUS,
        ParseRule::SEMICOLON,
        ParseRule::SLASH,
        ParseRule::STAR,
        ParseRule::BANG,
        ParseRule::BANG_EQUAL,
        ParseRule::EQUAL,
        ParseRule::EQUAL_EQUAL,
        ParseRule::GREATER,
        ParseRule::GREATER_EQUAL,
        ParseRule::LESS,
        ParseRule::LESS_EQUAL,
        ParseRule::IDENTIFIER,
        ParseRule::STRING,
        ParseRule::NUMBER,
        ParseRule::AND,
        ParseRule::CLASS,
        ParseRule::ELSE,
        ParseRule::FALSE,
        ParseRule::FOR,
        ParseRule::FUN,
        ParseRule::IF,
        ParseRule::NIL,
        ParseRule::OR,
        ParseRule::PRINT,
        ParseRule::RETURN,
        ParseRule::SUPER,
        ParseRule::THIS,
        ParseRule::TRUE,
        ParseRule::VAR,
        ParseRule::WHILE,
    ];

    #[inline]
    fn prefix(token: Token) -> ParseFn<'a, T> {
        Self::RULES[TokenType::from(token).into_usize()].0
    }

    #[inline]
    fn infix(token: Token) -> ParseFn<'a, T> {
        Self::RULES[TokenType::from(token).into_usize()].1
    }

    #[inline]
    fn precedence(token: Token) -> Precedence {
        Self::RULES[TokenType::from(token).into_usize()].2
    }
}

// struct ErrReport {
//     line: usize,
//     column: usize,
//     error: &'static str,
// }

// impl std::fmt::Display for ErrReport {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "Error at {}, {}: {}", self.line, self.column, self.error)
//     }
// }

#[derive(Debug, Clone, Copy)]
struct Local<'a> {
    name: &'a str,
    depth: isize,
}

impl Default for Local<'_> {
    fn default() -> Self {
        Self { name: Default::default(), depth: -1 }
    }
}

#[derive(Debug)]
struct Compiler<'a> {
    enclosing: Option<&'a mut Self>,
    locals: [Local<'a>; 256],
    local_count: usize,
    scope_depth: usize,
}

impl<'a> Compiler<'a> {
    const MAX_LOCALS: usize = 256;

    fn new() -> Self {
        Self {
            enclosing: None,
            locals: [Local::default(); 256],
            local_count: 0,
            scope_depth: 0,
        }
    }

    fn new_enclosed(enclosing: &'a mut Self) -> Self {
        Self {
            enclosing: Some(enclosing),
            locals: [Local::default(); 256],
            local_count: 0,
            scope_depth: 0,
        }
    }
}

struct Parser<'a, T: Iterator<Item = Parsed<Token<'a>>>> {
    chunk: Chunk,
    prev: Option<(usize, Token<'a>)>,
    current_compiler: Option<&'a mut Compiler<'a>>,
    tokens: Peekable<T>,
}

// Can probably do everything with peek() instead of keeping the previous token, but we'll find out
impl<'a, T: Iterator<Item = Parsed<Token<'a>>>> Parser<'a, T> {
    #[inline]
    fn begin_scope(&mut self) {
        if let Some(ref mut c) = self.current_compiler {
            c.scope_depth += 1;
        }
    }

    #[inline]
    fn end_scope(&mut self) -> Result<()> {
        let compiler = self.current_compiler.as_mut().unwrap();
        compiler.scope_depth -= 1;

        while compiler.local_count > 0
            && compiler.locals[compiler.local_count - 1].depth > compiler.scope_depth as isize
        {
            self.chunk.write(OpCode::Pop, self.prev.unwrap().0);
            compiler.local_count -= 1;
        }
        Ok(())
    }

    fn block(&mut self) -> Result<()> {
        while self.tokens.peek().is_some_and(|p| *p != Token::RightBrace) {
            self.declaration()?;
        }
        self.consume_token(TokenType::RightBrace)
    }

    fn consume_token(&mut self, token: TokenType) -> Result<()> {
        match self.tokens.peek() {
            Some(Parsed(_, Ok(t))) if *t == token => self.advance(),
            _ => Err(anyhow!("expected {token}")),
        }
    }

    fn match_token(&mut self, token: TokenType) -> Result<bool> {
        if self.tokens.peek().is_some_and(|t| *t == token) {
            self.advance()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn current(&mut self) -> Option<Result<(usize, Token)>> {
        match self.tokens.peek() {
            Some(Parsed((l, _), Ok(t))) => Some(Ok((*l, *t))),
            Some(Parsed((l, c), Err(e))) => Some(Err(anyhow!("Error at {l}, {c}: {e}"))),
            None => None,
        }
    }

    fn previous(&self) -> Token {
        self.prev
            .expect("should only call previous when there's a token")
            .1
    }

    // fn emit_byte(&mut self, byte: impl Into<u8>, line) {
    //     self.chunk
    //         .write(byte, self.prev.expect("only emit bytes from a token").0);
    // }

    fn advance(&mut self) -> Result<()> {
        let parsed = self.tokens.next();
        match parsed {
            Some(Parsed((l, c), Err(e))) => {
                while let Some(Parsed(_, Err(_))) = self.tokens.peek() {
                    self.tokens.next();
                }
                Err(anyhow!("Error at {l}, {c}: {e}"))
            }
            _ => {
                self.prev = parsed.map(|Parsed((l, _), res)| (l, res.unwrap()));
                Ok(())
            }
        }
    }

    fn synchronize(&mut self) {
        while let Some(Parsed(_, Ok(t))) = self.tokens.peek() {
            if self.prev.unwrap().1 == Token::Semicolon {
                return;
            }
            match t {
                Token::Class
                | Token::Fun
                | Token::For
                | Token::If
                | Token::Print
                | Token::Return
                | Token::Var
                | Token::While => return,
                _ => {
                    let _ = self.advance();
                }
            }
        }
    }

    fn declaration(&mut self) -> Result<()> {
        if self.match_token(TokenType::Var)? {
            self.var_dec()
        } else {
            self.statement()
        }
    }

    fn var_dec(&mut self) -> Result<()> {
        let (global, line) = self.parse_variable()?;
        if self.match_token(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.chunk.write(OpCode::Nil, line);
        }
        self.consume_token(TokenType::Semicolon)?;

        // Define variable
        if self
            .current_compiler
            .as_ref()
            .is_some_and(|c| c.scope_depth > 0)
        {
            return Ok(());
        }
        self.chunk.write(OpCode::DefineGlobal, line);
        self.chunk.write(global, line);
        Ok(())
    }

    fn parse_variable(&mut self) -> Result<(u8, usize)> {
        self.consume_token(TokenType::Identifier)?;
        // this is infalliable since if it wasn't an identifier we'd return with an error after the previous line
        let Some((l, Token::Identifier(s))) = self.prev else {
            unreachable!()
        };

        self.declare_variable()?;
        
        Ok((self.chunk.add_constant(Value::new_string(s.to_string())), l))
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self
            .current_compiler
            .as_ref()
            .is_some_and(|c| c.scope_depth == 0)
        {
            return Ok(());
        }

        match self.prev {
            Some((_, Token::Identifier(name))) => {
                let compiler = self.current_compiler.as_ref().unwrap();
                for i in (0..compiler.local_count).rev() {
                    let local = compiler.locals[i];
                    if local.depth != -1 && local.depth < compiler.scope_depth as isize {
                        break;
                    }

                    if local.name == name {
                        return Err(anyhow!("already a variable named {name} in this scope."));
                    }
                }
                self.add_local(name)
            }
            _ => Err(anyhow!("expect identifier")),
        }
    }

    fn add_local(&mut self, name: &'a str) -> Result<()> {
        let mut compiler = self.current_compiler.as_mut().unwrap();
        if compiler.local_count == Compiler::MAX_LOCALS {
            return Err(anyhow!("Too many local variables"));
        }
        compiler.locals[compiler.local_count] = Local {
            name,
            depth: compiler.scope_depth as isize,
        };
        compiler.local_count += 1;
        Ok(())
    }

    fn resolve_local(&mut self, name: &str) -> Option<u8> {
        let compiler = self.current_compiler.as_ref().unwrap();
        for i in (0..compiler.local_count).rev() {
            if compiler.locals[i].name == name {
                return Some(i as u8);
            }
        }
        None
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let Some((l, Token::Identifier(name))) = self.prev else {
            unreachable!()
        };

        let (arg, get_op, set_op) =
            if let Some(i) = self.resolve_local(name) {
                (i, OpCode::GetLocal, OpCode::SetLocal)
            } else {
                (
                    self.chunk.add_constant(Value::new_string(name.to_string())),
                    OpCode::GetGlobal,
                    OpCode::SetGlobal,
                )
            };

        
        if can_assign && self.match_token(TokenType::Equal)? {
            self.expression()?;
            self.chunk.write(set_op, l);
        } else {
            self.chunk.write(get_op, l);
        }
        self.chunk.write(arg, l);
        Ok(())
    }

    fn emit_jump(&mut self, op: OpCode, line: usize) -> usize {
        self.chunk.write(op, line);
        self.chunk.write(0xff, line);
        self.chunk.write(0xff, line);
        self.chunk.code().len() - 2
    }

    fn patch_jump(&mut self, offset: usize) -> Result<()> {
        let code = &mut self.chunk.code_mut();
        let jump = code.len() - offset - 2;

        if jump > u16::MAX as usize {
            return Err(anyhow!("Too much code to jump over."));
        }

        let [hi, lo] = (jump as u16).to_be_bytes();
        code[offset] = hi;
        code[offset + 1] = lo;
        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        // TODO: fix error propagation
        let Some(Ok((line, token))) = self.current() else {
            return Ok(());
        };
        match token {
            Token::LeftBrace => {
                self.advance()?;
                self.begin_scope();
                self.block()?;
                self.end_scope();
            }
            Token::Class => todo!(),
            Token::Fun => todo!(),
            Token::For => todo!(),
            Token::If => {
                self.advance()?;
                self.consume_token(TokenType::LeftParen)?;
                self.expression()?;
                self.consume_token(TokenType::RightParen)?;

                let then_jump = self.emit_jump(OpCode::JumpIfFalse, self.prev.unwrap().0);
                self.chunk.write(OpCode::Pop, self.prev.unwrap().0);

                self.statement()?;
                
                let else_jump = self.emit_jump(OpCode::Jump, self.prev.unwrap().0);
                self.patch_jump(then_jump)?;
                self.chunk.write(OpCode::Pop, self.prev.unwrap().0);
                if self.match_token(TokenType::Else)? {
                    // Does this get else-if for free? I think it might
                    self.statement()?;
                }
                self.patch_jump(else_jump)?;
            },
            Token::Print => {
                self.advance()?;
                self.expression()?;
                self.consume_token(TokenType::Semicolon)?;
                self.chunk.write(OpCode::Print, line);
            }
            Token::Return => todo!(),
            Token::While => todo!(),
            _ => {
                self.expression()?;
                self.consume_token(TokenType::Semicolon)?;
                self.chunk.write(OpCode::Pop, line);
            }
        }
        Ok(())
    }

    fn string(&mut self, can_assign: bool) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call string when there is a token");
        match token {
            Token::String(s) => {
                let constant = self.chunk.add_constant(Value::new_string(s.to_string()));
                self.chunk.write(OpCode::Constant, line);
                self.chunk.write(constant, line);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn unary(&mut self, can_assign: bool) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call unary when there is a token");
        self.parse_precedence(Precedence::Unary)?;
        let op = match token {
            Token::Minus => OpCode::Negate,
            Token::Bang => OpCode::Not,
            _ => return Err(anyhow!("{token} is not a valid unary operator!")),
        };
        self.chunk.write(op, line);
        Ok(())
    }

    fn binary(&mut self, can_assign: bool) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call binary when there is a token");
        self.parse_precedence(ParseRule::<T>::precedence(token).next_highest())?;
        match token {
            Token::Minus => self.chunk.write(OpCode::Subtract, line),
            Token::Plus => self.chunk.write(OpCode::Add, line),
            Token::Slash => self.chunk.write(OpCode::Divide, line),
            Token::Star => self.chunk.write(OpCode::Multiply, line),
            Token::BangEqual => {
                self.chunk.write(OpCode::Equal, line);
                self.chunk.write(OpCode::Not, line);
            }
            Token::EqualEqual => self.chunk.write(OpCode::Equal, line),
            Token::Greater => self.chunk.write(OpCode::Greater, line),
            Token::GreaterEqual => self.chunk.write(OpCode::GreaterEqual, line),
            Token::Less => self.chunk.write(OpCode::Less, line),
            Token::LessEqual => self.chunk.write(OpCode::LessEqual, line),
            _ => return Err(anyhow!("{token} is not a valid binary operator!")),
        }
        Ok(())
    }

    fn number(&mut self, can_assign: bool) -> Result<()> {
        let (line, Token::Number(n)) = self.prev.expect("only call on numbers") else {
            unreachable!()
        };
        self.chunk.write(OpCode::Constant, line);
        let constant = self.chunk.add_constant(Value::Number(n));
        self.chunk.write(constant, line);
        Ok(())
    }

    fn literal(&mut self, can_assign: bool) -> Result<()> {
        match self.prev {
            Some((i, Token::False)) => self.chunk.write(OpCode::False, i),
            Some((i, Token::Nil)) => self.chunk.write(OpCode::Nil, i),
            Some((i, Token::True)) => self.chunk.write(OpCode::True, i),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn grouping(&mut self, can_assign: bool) -> Result<()> {
        self.expression()?;
        self.consume_token(TokenType::RightParen)?;
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;
        if let Some((_, token)) = self.prev {
            let can_assign = precedence <= Precedence::Assignment;
            match ParseRule::<T>::prefix(token) {
                Some(prefix_rule) => prefix_rule(self, can_assign)?,
                None => return Err(anyhow!("expect expression.")),
            }

            while let Some(res) = self.current()
            && ParseRule::<T>::precedence(res?.1) >= precedence {
                match self.current() {
                    Some(Ok(_)) => {
                        self.advance()?;
                        ParseRule::infix(self.previous()).expect("should be an infix rule.")(self, can_assign)?;
                        if can_assign && self.match_token(TokenType::Equal)? {
                            return Err(anyhow!("Invalid assignment target"));
                        }
                    }
                    Some(Err(e)) => return Err(e),
                    None => break,
                }
            }
        }
        Ok(())
    }
}

pub fn compile(source: &str) -> Result<Chunk> {
    // for token in scan(source) {
    //     println!("Token: {token}");
    // }

    let mut compiler = Compiler::new();

    let mut parser = Parser {
        chunk: Chunk::new(),
        prev: None,
        current_compiler: Some(&mut compiler),
        tokens: scan(source),
    };

    loop {
        if let Err(e) = parser.declaration() {
            parser.synchronize();
            println!("{e}");
        }
        if parser.tokens.peek().is_none() {
            break;
        }
    }
    Ok(parser.chunk)
}
