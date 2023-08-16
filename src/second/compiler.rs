use std::{iter::Peekable, ops::Index, ptr::NonNull};

use anyhow::{anyhow, Result};
use enum_map::{Enum, EnumMap};

use super::{
    chunk::{Chunk, OpCode},
    memory::Heap,
    object::{FunctionType, Obj, ObjFunction, ObjString},
    scanner::{
        scan, Parsed, Token,
        TokenType::{self, *},
    },
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
    const LEFT_PAREN: Self      = Self(Some(Parser::grouping),  Some(Parser::call),     Precedence::Call);
    const RIGHT_PAREN: Self     = Self(None,                    None,                   Precedence::None);
    const LEFT_BRACE: Self      = Self(None,                    None,                   Precedence::None);
    const RIGHT_BRACE: Self     = Self(None,                    None,                   Precedence::None);
    const COMMA: Self           = Self(None,                    None,                   Precedence::None);
    const DOT: Self             = Self(None,                    None,                   Precedence::None);
    const MINUS: Self           = Self(Some(Parser::unary),     Some(Parser::binary),   Precedence::Term);
    const PLUS: Self            = Self(None,                    Some(Parser::binary),   Precedence::Term);
    const SEMICOLON: Self       = Self(None,                    None,                   Precedence::None);
    const SLASH: Self           = Self(None,                    Some(Parser::binary),   Precedence::Factor);
    const STAR: Self            = Self(None,                    Some(Parser::binary),   Precedence::Factor);
    const BANG: Self            = Self(Some(Parser::unary),     None,                   Precedence::None);
    const BANG_EQUAL: Self      = Self(None,                    Some(Parser::binary),   Precedence::Comparison);
    const EQUAL: Self           = Self(None,                    None,                   Precedence::None);
    const EQUAL_EQUAL: Self     = Self(None,                    Some(Parser::binary),   Precedence::Equality);
    const GREATER: Self         = Self(None,                    Some(Parser::binary),   Precedence::Comparison);
    const GREATER_EQUAL: Self   = Self(None,                    Some(Parser::binary),   Precedence::Comparison);
    const LESS: Self            = Self(None,                    Some(Parser::binary),   Precedence::Comparison);
    const LESS_EQUAL: Self      = Self(None,                    Some(Parser::binary),   Precedence::Comparison);
    const IDENTIFIER: Self      = Self(Some(Parser::variable),  None,                   Precedence::None);
    const STRING: Self          = Self(Some(Parser::string),    None,                   Precedence::None);
    const NUMBER: Self          = Self(Some(Parser::number),    None,                   Precedence::None);
    const AND: Self             = Self(None,                    Some(Parser::and),      Precedence::And);
    const CLASS: Self           = Self(None,                    None,                   Precedence::None);
    const ELSE: Self            = Self(None,                    None,                   Precedence::None);
    const FALSE: Self           = Self(Some(Parser::literal),   None,                   Precedence::None);
    const FOR: Self             = Self(None,                    None,                   Precedence::None);
    const FUN: Self             = Self(None,                    None,                   Precedence::None);
    const IF: Self              = Self(None,                    None,                   Precedence::None);
    const NIL: Self             = Self(Some(Parser::literal),   None,                   Precedence::None);
    const OR: Self              = Self(None,                    Some(Parser::or),       Precedence::Or);
    const PRINT: Self           = Self(None,                    None,                   Precedence::None);
    const RETURN: Self          = Self(None,                    None,                   Precedence::None);
    const SUPER: Self           = Self(None,                    None,                   Precedence::None);
    const THIS: Self            = Self(None,                    None,                   Precedence::None);
    const TRUE: Self            = Self(Some(Parser::literal),   None,                   Precedence::None);
    const VAR: Self             = Self(None,                    None,                   Precedence::None);
    const WHILE: Self           = Self(None,                    None,                   Precedence::None);

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
    is_captured: bool,
}

impl Default for Local<'_> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            depth: -1,
            is_captured: false,
        }
    }
}

impl<'a> Local<'a> {
    fn new(name: &'a str, depth: isize, is_captured: bool) -> Self {
        Self {
            name,
            depth,
            is_captured,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
struct UpValue {
    index: u8,
    is_local: bool,
}

#[derive(Debug)]
pub struct Compiler<'a> {
    pub function: NonNull<ObjFunction>,
    fun_type: FunctionType,
    enclosing: Option<NonNull<Self>>,
    locals: [Local<'a>; 256],
    upvalues: [UpValue; 256],
    scope_depth: usize,
    local_count: u8,
}

impl<'a> Compiler<'a> {
    const MAX_LOCALS: usize = 256;

    fn new(enclosing: NonNull<Self>, fun_type: FunctionType, heap: &mut Heap) -> Self {
        let function = heap.new_obj(ObjFunction::new()).cast();
        let mut compiler = Self {
            function,
            fun_type,
            enclosing: Some(enclosing),
            locals: [Local::default(); 256],
            upvalues: [UpValue::default(); 256],
            local_count: 1,
            scope_depth: 0,
        };

        compiler.locals[0] = Local::new("", 0, false);

        compiler
    }

    fn function(&mut self) -> *mut ObjFunction {
        unsafe { self.function.as_ptr() }
    }

    fn set_function_name(&mut self, name: &str) {
        unsafe {
            (*self.function.as_ptr()).name = Some(String::from(name));
        }
    }

    #[inline]
    fn local_count(&self) -> usize {
        self.local_count as usize
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> Result<u8> {
        unsafe {
            let function = self.function.as_ptr();
            let count = (*function).upvalue_count;

            for i in 0..count {
                let upvalue = self.upvalues[i as usize];
                if upvalue.index == index && upvalue.is_local == is_local {
                    return Ok(i);
                }
            }

            self.upvalues[count as usize].index = index;
            self.upvalues[count as usize].is_local = is_local;
            let overflow;
            ((*function).upvalue_count, overflow) = count.overflowing_add(1);
            if overflow {
                return Err(anyhow!("Too many upvalues"));
            }
            Ok(count)
        }
    }
}

struct Parser<'a, T: Iterator<Item = Parsed<Token<'a>>>> {
    prev: Option<(usize, Token<'a>)>,
    current_compiler: NonNull<Compiler<'a>>,
    tokens: Peekable<T>,
    heap: &'a mut Heap,
}

// Can probably do everything with peek() instead of keeping the previous token, but we'll find out
impl<'a, T: Iterator<Item = Parsed<Token<'a>>>> Parser<'a, T> {
    #[inline]
    fn scope_depth(&mut self) -> usize {
        unsafe { (*self.current_compiler.as_ptr()).scope_depth }
    }

    #[inline]
    fn begin_scope(&mut self) {
        unsafe {
            (*self.current_compiler.as_ptr()).scope_depth += 1;
        }
    }

    #[inline]
    fn end_scope(&mut self) -> Result<()> {
        unsafe {
            let mut compiler = self.current_compiler.as_ptr();

            (*compiler).scope_depth -= 1;

            while (*compiler).local_count > 0
                && (*compiler).locals[(*compiler).local_count() - 1].depth
                    > (*compiler).scope_depth as isize
            {
                if (*compiler).locals[(*compiler).local_count() - 1].is_captured {
                    self.emit_byte(OpCode::CloseUpvalue);
                } else {
                    self.emit_byte(OpCode::Pop);
                }
                // reborrowing to make the borrow checker happy
                (*compiler).local_count = (*compiler).local_count.saturating_sub(1);
            }
            Ok(())
        }
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
    #[inline]
    fn chunk(&mut self) -> &mut Chunk {
        unsafe { &mut (*(*self.current_compiler.as_ptr()).function.as_ptr()).chunk }
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

    pub fn parse(&mut self) {
        while self.tokens.peek().is_some() {
            if let Err(e) = self.declaration() {
                self.synchronize();
                println!("{e}");
            }
        }

        self.emit_return();
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
        if self.match_token(Fun)? {
            self.fun_dec()
        } else if self.match_token(Var)? {
            self.var_dec()
        } else {
            self.statement()
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Nil);
        self.emit_byte(OpCode::Return);
    }

    fn fun_dec(&mut self) -> Result<()> {
        unsafe {
            let (global, line) = self.parse_variable()?;
            let mut compiler = unsafe { self.current_compiler.as_ptr() };
            if (*compiler).scope_depth != 0 {
                (*compiler).locals[(*compiler).local_count() - 1].depth =
                    (*compiler).scope_depth as isize;
            }
            self.function(FunctionType::Function)?;
            self.define_variable(global)?;
            Ok(())
        }
    }

    fn init_compiler(&mut self, fun_type: FunctionType) -> Result<()> {
        Ok(())
    }

    fn inc_arity(&mut self) -> Result<()> {
        unsafe {
            let overflow;
            (
                (*(*self.current_compiler.as_ptr()).function.as_ptr()).arity,
                overflow,
            ) = (*(*self.current_compiler.as_ptr()).function.as_ptr())
                .arity
                .overflowing_add(1);
            if overflow {
                return Err(anyhow!("Can't have more than 255 parameters."));
            }
        }
        return Ok(());
    }

    fn function(&mut self, fun_type: FunctionType) -> Result<()> {
        let enclosing = self.current_compiler;
        let mut compiler = Compiler::new(enclosing, fun_type, &mut self.heap);
        let Some((_, Token::Identifier(name))) = self.prev else {
            unreachable!()
        };
        compiler.set_function_name(name);
        self.current_compiler = NonNull::new(&mut compiler as *mut Compiler).unwrap();
        self.begin_scope();
        self.consume_token(LeftParen)?;
        if self.tokens.peek().is_some_and(|t| *t != RightParen) {
            loop {
                self.inc_arity()?;

                let constant = self.parse_variable()?;
                self.define_variable(constant.0)?;
                if !self.match_token(Comma)? {
                    break;
                }
            }
        }
        self.consume_token(RightParen)?;
        self.consume_token(LeftBrace)?;
        self.block()?;
        self.emit_return();

        self.emit_return();
        unsafe {
            // end compiler
            let function = (*self.current_compiler.as_ptr()).function;
            let compiler = self.current_compiler.as_ptr();
            self.current_compiler = (*self.current_compiler.as_ptr()).enclosing.unwrap();

            let constant = self.chunk().add_constant(Value::Obj(function.cast()));
            self.emit_byte(OpCode::Closure);
            self.emit_byte(constant);
            for i in (0..(*function.as_ptr()).upvalue_count as usize) {
                self.emit_byte((*compiler).upvalues[i].is_local);
                self.emit_byte((*compiler).upvalues[i].index);
            }
        }
        Ok(())
    }

    fn var_dec(&mut self) -> Result<()> {
        let (global, line) = self.parse_variable()?;
        if self.match_token(Equal)? {
            self.expression()?;
        } else {
            self.chunk().write(OpCode::Nil, line);
        }
        self.consume_token(Semicolon)?;

        // Define variable
        self.define_variable(global)
    }

    fn parse_variable(&mut self) -> Result<(u8, usize)> {
        self.consume_token(Identifier)?;
        // this is infalliable since if it wasn't an identifier we'd return with an error after the previous line
        let Some((l, Token::Identifier(s))) = self.prev else {
            unreachable!()
        };

        self.declare_variable()?;
        let string = self.heap.new_obj(ObjString::new(s.to_string())).cast();
        Ok((self.chunk().add_constant(Value::Obj(string)), l))
    }

    fn mark_initialized(&mut self) -> Result<()> {
        let depth = self.scope_depth();
        if depth != 0 {
            unsafe {
                let compiler = self.current_compiler.as_mut();
                compiler.locals[compiler.local_count() - 1].depth = compiler.scope_depth as isize;
            }
        }
        Ok(())
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self.scope_depth() == 0 {
            return Ok(());
        }

        match self.prev {
            Some((_, Token::Identifier(name))) => {
                let compiler = unsafe { self.current_compiler.as_mut() };
                for i in (0..compiler.local_count()).rev() {
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

    fn define_variable(&mut self, global: u8) -> Result<()> {
        if self.scope_depth() > 0 {
            self.mark_initialized()
        } else {
            self.emit_byte(OpCode::DefineGlobal);
            self.emit_byte(global);
            Ok(())
        }
    }

    fn add_local(&mut self, name: &'a str) -> Result<()> {
        let mut compiler = unsafe { self.current_compiler.as_mut() };
        if compiler.local_count() == Compiler::MAX_LOCALS {
            return Err(anyhow!("Too many local variables"));
        }
        compiler.locals[compiler.local_count()] = Local::new(name, -1, false);
        compiler.local_count += 1;
        Ok(())
    }

    fn resolve_local(&mut self, compiler: NonNull<Compiler>, name: &str) -> Result<Option<u8>> {
        let compiler = unsafe { compiler.as_ref() };
        for i in (0..compiler.local_count()).rev() {
            let local = compiler.locals[i];
            if local.name == name {
                if local.depth == -1 {
                    return Err(anyhow!(
                        "Can't read local variable {name} in its own initializer."
                    ));
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }

    fn resolve_upvalue(&mut self, compiler: NonNull<Compiler>, name: &str) -> Result<Option<u8>> {
        unsafe {
            let compiler = compiler.as_ptr();
            let fun_name = (*(*compiler).function())
                .name
                .clone()
                .unwrap_or("".to_owned());
            if let Some(enclosing) = (*compiler).enclosing {
                if let Some(local) = self.resolve_local(enclosing, name)? {
                    let l = &mut (*enclosing.as_ptr()).locals[local as usize];
                    l.is_captured = true;
                    return Ok(Some((*compiler).add_upvalue(local, true)?));
                }

                if let Some(upvalue) = self.resolve_upvalue(enclosing, name)? {
                    return Ok(Some((*compiler).add_upvalue(upvalue, false)?));
                }
            }
            Ok(None)
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let Some((l, Token::Identifier(name))) = self.prev else {
            unreachable!()
        };

        let (arg, get_op, set_op) =
            if let Some(i) = self.resolve_local(self.current_compiler, name)? {
                (i, OpCode::GetLocal, OpCode::SetLocal)
            } else if let Some(i) = self.resolve_upvalue(self.current_compiler, name)? {
                (i, OpCode::GetUpvalue, OpCode::SetUpvalue)
            } else {
                let obj = self.heap.new_obj(ObjString::new(name.to_string()));
                (
                    self.chunk().add_constant(Value::Obj(obj)),
                    OpCode::GetGlobal,
                    OpCode::SetGlobal,
                )
            };

        if can_assign && self.match_token(Equal)? {
            self.expression()?;
            self.chunk().write(set_op, l);
        } else {
            self.chunk().write(get_op, l);
        }
        self.chunk().write(arg, l);
        Ok(())
    }

    fn emit_jump(&mut self, op: OpCode, line: usize) -> isize {
        self.chunk().write(op, line);
        self.chunk().write(0xff, line);
        self.chunk().write(0xff, line);
        self.chunk().code().len() as isize - 2
    }

    fn emit_loop(&mut self, start: usize) -> Result<()> {
        let line = self.prev.unwrap().0;
        self.chunk().write(OpCode::Loop, line);
        let offset = self.chunk().code().len() - start + 2;
        if offset > u16::MAX as usize {
            return Err(anyhow!("loop body too large."));
        }
        //let [lo, hi] = (offset as u16).to_le_bytes();
        let hi = ((offset >> 8) & 0xff) as u8;
        let lo = (offset & 0xff) as u8;
        self.chunk().write(hi, line);
        self.chunk().write(lo, line);
        Ok(())
    }

    fn patch_jump(&mut self, offset: usize) -> Result<()> {
        let code = &mut self.chunk().code_mut();
        let jump = code.len() - offset - 2;

        if jump > u16::MAX as usize {
            return Err(anyhow!("Too much code to jump over."));
        }
        let hi = ((jump >> 8) & 0xff) as u8;
        let lo = (jump & 0xff) as u8;
        code[offset] = hi;
        code[offset + 1] = lo;
        Ok(())
    }

    fn and(&mut self, can_assign: bool) -> Result<()> {
        let line = self.prev.unwrap().0;
        let end_jump = self.emit_jump(OpCode::JumpIfFalse, line);
        self.chunk().write(OpCode::Pop, line);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump as usize)
    }

    fn or(&mut self, can_assign: bool) -> Result<()> {
        let line = self.prev.unwrap().0;
        let else_jump = self.emit_jump(OpCode::JumpIfFalse, line);
        let end_jump = self.emit_jump(OpCode::Jump, line);

        self.patch_jump(else_jump as usize)?;
        self.chunk().write(OpCode::Pop, line);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump as usize)
    }

    fn expression_stmt(&mut self, line: usize) -> Result<()> {
        self.expression()?;
        self.consume_token(TokenType::Semicolon)?;
        self.chunk().write(OpCode::Pop, line);
        Ok(())
    }

    fn emit_byte<U: Into<u8>>(&mut self, byte: U) {
        let line = self.prev.unwrap().0;
        self.chunk().write(byte, line);
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
            Token::For => {
                self.begin_scope();
                self.advance()?;
                self.consume_token(TokenType::LeftParen)?;

                if self.match_token(TokenType::Semicolon)? {
                    // No initializer
                } else if self.match_token(TokenType::Var)? {
                    self.var_dec()?;
                } else {
                    self.expression_stmt(self.prev.unwrap().0)?;
                }

                let mut loop_start = self.chunk().code().len();
                let mut exit_jump = -1;
                if !self.match_token(TokenType::Semicolon)? {
                    self.expression()?;
                    self.consume_token(TokenType::Semicolon)?;

                    let line = self.prev.unwrap().0;
                    exit_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.chunk().write(OpCode::Pop, line);
                }

                if !self.match_token(TokenType::RightParen)? {
                    let body_jump = self.emit_jump(OpCode::Jump, self.prev.unwrap().0);
                    let inc_start = self.chunk().code().len();
                    self.expression()?;
                    self.emit_byte(OpCode::Pop);
                    self.consume_token(TokenType::RightParen)?;

                    self.emit_loop(loop_start)?;
                    loop_start = inc_start;
                    self.patch_jump(body_jump as usize)?;
                }

                self.statement()?;
                self.emit_loop(loop_start)?;

                if exit_jump != -1 {
                    self.patch_jump(exit_jump as usize)?;
                    self.emit_byte(OpCode::Pop);
                }
                self.end_scope();
            }
            Token::If => {
                self.advance()?;
                self.consume_token(TokenType::LeftParen)?;
                self.expression()?;
                self.consume_token(TokenType::RightParen)?;

                let then_jump = self.emit_jump(OpCode::JumpIfFalse, self.prev.unwrap().0);
                self.emit_byte(OpCode::Pop);

                self.statement()?;

                let else_jump = self.emit_jump(OpCode::Jump, self.prev.unwrap().0);
                self.patch_jump(then_jump as usize)?;
                self.emit_byte(OpCode::Pop);
                if self.match_token(TokenType::Else)? {
                    // Does this get else-if for free? I think it might
                    self.statement()?;
                }
                self.patch_jump(else_jump as usize)?;
            }
            Token::Print => {
                self.advance()?;
                self.expression()?;
                self.consume_token(TokenType::Semicolon)?;
                self.chunk().write(OpCode::Print, line);
            }
            Token::Return => {
                self.advance()?;
                if self.match_token(Semicolon)? {
                    self.emit_return();
                } else {
                    self.expression()?;
                    self.consume_token(Semicolon)?;
                    self.emit_byte(OpCode::Return);
                }
            }
            Token::While => {
                self.advance()?;
                let loop_start = self.chunk().code().len();
                self.consume_token(TokenType::LeftParen)?;
                self.expression()?;
                self.consume_token(TokenType::RightParen)?;
                let line = self.prev.unwrap().0;

                let exit_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.chunk().write(OpCode::Pop, line);
                self.statement()?;
                self.emit_loop(loop_start)?;

                self.patch_jump(exit_jump as usize)?;
                self.emit_byte(OpCode::Pop);
            }
            _ => self.expression_stmt(line)?,
        }
        Ok(())
    }

    fn string(&mut self, can_assign: bool) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call string when there is a token");
        match token {
            Token::String(s) => {
                let obj = self.heap.new_obj(ObjString::new(s.to_string())).cast();
                let constant = self.chunk().add_constant(Value::Obj(obj));
                self.chunk().write(OpCode::Constant, line);
                self.chunk().write(constant, line);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn call(&mut self, _can_assign: bool) -> Result<()> {
        let arg_count = self.argument_list()?;
        self.emit_byte(OpCode::Call);
        self.emit_byte(arg_count);
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut arg_count = 0;
        if !self.tokens.peek().is_some_and(|t| *t == RightParen) {
            loop {
                self.expression()?;
                if arg_count == 255 {
                    return Err(anyhow!("can't have more than 255 arguments"));
                }
                arg_count += 1;
                if !self.match_token(Comma)? {
                    break;
                }
            }
        }
        self.consume_token(RightParen)?;
        Ok(arg_count)
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
        self.chunk().write(op, line);
        Ok(())
    }

    fn binary(&mut self, can_assign: bool) -> Result<()> {
        let (line, token) = self
            .prev
            .expect("should only call binary when there is a token");
        self.parse_precedence(ParseRule::<T>::precedence(token).next_highest())?;
        match token {
            Token::Minus => self.chunk().write(OpCode::Subtract, line),
            Token::Plus => self.chunk().write(OpCode::Add, line),
            Token::Slash => self.chunk().write(OpCode::Divide, line),
            Token::Star => self.chunk().write(OpCode::Multiply, line),
            Token::BangEqual => {
                self.chunk().write(OpCode::Equal, line);
                self.chunk().write(OpCode::Not, line);
            }
            Token::EqualEqual => self.chunk().write(OpCode::Equal, line),
            Token::Greater => self.chunk().write(OpCode::Greater, line),
            Token::GreaterEqual => self.chunk().write(OpCode::GreaterEqual, line),
            Token::Less => self.chunk().write(OpCode::Less, line),
            Token::LessEqual => self.chunk().write(OpCode::LessEqual, line),
            _ => return Err(anyhow!("{token} is not a valid binary operator!")),
        }
        Ok(())
    }

    fn number(&mut self, can_assign: bool) -> Result<()> {
        let (line, Token::Number(n)) = self.prev.expect("only call on numbers") else {
            unreachable!()
        };
        self.chunk().write(OpCode::Constant, line);
        let constant = self.chunk().add_constant(Value::Number(n));
        self.chunk().write(constant, line);
        Ok(())
    }

    fn literal(&mut self, can_assign: bool) -> Result<()> {
        match self.prev {
            Some((i, Token::False)) => self.chunk().write(OpCode::False, i),
            Some((i, Token::Nil)) => self.chunk().write(OpCode::Nil, i),
            Some((i, Token::True)) => self.chunk().write(OpCode::True, i),
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
                        let infix = ParseRule::infix(self.previous())
                            .expect("should be an infix rule.");                        
                        infix(self, can_assign)?;
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

pub fn compile(source: &str, heap: &mut Heap) -> Result<NonNull<ObjFunction>> {
    // for token in scan(source) {
    //     println!("Token: {token}");
    // }
    let mut compiler = Compiler {
        function: heap.new_obj(ObjFunction::new()).cast(),
        fun_type: FunctionType::Script,
        enclosing: None,
        locals: [Local::default(); 256],
        upvalues: [UpValue::default(); 256],
        local_count: 1,
        scope_depth: 0,
    };

    compiler.locals[0] = Local::new("", 0, false);
    let mut parser = Parser {
        prev: None,
        current_compiler: NonNull::new(&mut compiler as *mut Compiler).unwrap(),
        tokens: scan(source),
        heap,
    };

    while parser.tokens.peek().is_some() {
        if let Err(e) = parser.declaration() {
            parser.synchronize();
            println!("{e}");
        }
    }

    parser.emit_return();

    return Ok(compiler.function);
}
