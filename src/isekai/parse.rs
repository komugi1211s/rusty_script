#[macro_use]
use bitflags;

use super::token::{Token, TokenType};
use super::types::{Constant, Type, Value};
use std::fmt;

// TODO: REMOVE CLONE
pub trait Visitor<T> {
    type Result;
    fn visit(&mut self, t: &T) -> Self::Result;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Box<Expr>, Token),
    Logical(Box<Expr>, Box<Expr>, Token),
    FunctionCall(Box<Expr>, Token, Vec<Expr>),
    Assign(u16, Box<Expr>),

    Literal(Constant),
    Grouping(Box<Expr>),
    Unary(Box<Expr>, Token),
    Variable(u16),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // DebugPrint
    Print(Expr),
    Return(Expr),
    Expression(Expr),
    // Defer(Expr),
    Decralation(DeclarationData),
    Function(FunctionData),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Box<Statement>, Expr, Expr, Box<Statement>),
    Block(BlockData),
    Break,
    Continue,
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationData {
    pub name: String,
    pub name_u16: u16,
    pub _type: Type,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionData {
    pub it: DeclarationData,
    pub args: Vec<DeclarationData>,
    pub block: BlockData,
}

pub struct DeclarationDataBuilder {
    name: Option<String>,
    name_u16: Option<u16>,
    _type: Option<Type>,
    expr: Option<Expr>,
}

impl DeclarationDataBuilder {
    pub fn new() -> Self {
        Self {
            name: None,
            name_u16: None,
            _type: None,
            expr: None,
        }
    }

    pub fn setname(mut self, name: &str) -> Self {
        self.name = Some(name.to_string());
        self.name_u16 = Some(name.as_bytes().iter().map(|x| *x as u16).sum());
        self
    }

    pub fn settype(mut self, ty: Type) -> Self {
        self._type = Some(ty);
        self
    }

    pub fn setexpr(mut self, expr: Expr) -> Self {
        self.expr = Some(expr);
        self
    }

    pub fn build(mut self) -> DeclarationData {
        DeclarationData {
            name: self.name.unwrap(),
            _type: self._type.unwrap(),
            name_u16: self.name_u16.unwrap(),
            expr: self.expr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockData {
    pub local_count: usize,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ParserNode {
    pub line: usize,
    pub value: Statement,
}

impl ParserNode {
    pub fn new(stmt: Statement, line: usize) -> Self {
        Self { line, value: stmt }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Binary(ref left, ref right, ref token) => {
                write!(f, "{} {} {}", left, right, token)
            }
            Expr::Logical(ref left, ref right, ref token) => {
                write!(f, "{} {} {}", left, right, token)
            }
            Expr::Grouping(ref inside) => write!(f, "({})", inside),
            Expr::Literal(ref lit) => write!(f, "{:?}", lit),
            Expr::Unary(ref item, ref t) => write!(f, "{}{}", t, item),
            Expr::Variable(ref s) => write!(f, "{}", s),
            Expr::Assign(s, ex) => write!(f, "{} = {}", s, ex),
            Expr::FunctionCall(ref name, _, ref items) => write!(f, "{}({:?})", name, items),
            x => write!(f, "unimplemented display expr {:?}", x),
        }
    }
}
