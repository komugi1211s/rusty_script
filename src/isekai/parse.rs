#[macro_use]
use super::token::{Token};
use super::types::{Constant, Type};
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
    Assign(String, Box<Expr>),

    Literal(Constant),
    Grouping(Box<Expr>),
    Unary(Box<Expr>, Token),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // DebugPrint
    Print(Expr),
    Return(Option<Expr>),
    Expression(Expr),
    // Defer(Expr),
    Decralation(DeclarationData),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Box<Statement>, Expr, Expr, Box<Statement>),
    Block(BlockData),
    Function(usize),
    Break,
    Continue,
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationData {
    pub name: String,
    pub _type: Type,
    pub is_argument: bool,
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
    is_argument: bool,
    _type: Option<Type>,
    expr: Option<Expr>,
}

impl DeclarationDataBuilder {
    pub fn new() -> Self {
        Self {
            name: None,
            is_argument: false,
            _type: None,
            expr: None,
        }
    }

    pub fn setname(mut self, name: &str) -> Self {
        self.name = Some(name.to_string());
        self
    }

    pub fn setargmode(mut self, mode: bool) -> Self {
        self.is_argument = true;
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

    pub fn build(self) -> DeclarationData {
        DeclarationData {
            name: self.name.unwrap(),
            _type: self._type.unwrap(),
            is_argument: self.is_argument,
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
pub struct StatementNode {
    pub line: usize,
    pub value: Statement,
}

#[derive(Debug)]
pub struct ParsedResult
{
    pub functions: Vec<FunctionData>,
    pub statements: Vec<StatementNode>,
}

impl StatementNode {
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
