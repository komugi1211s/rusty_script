use std::fmt;
use super::types::{ Value, Type };
use super::token::{ Token, TokenType };

// TODO: REMOVE CLONE 
pub trait Visitor<T>
{
    type Result;
    fn visit(&mut self, t: &T) -> Self::Result;
}


#[derive(Debug, PartialEq, Clone)]
pub struct ParsedData
{
    pub line: usize,
    pub value: Statement,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr
{
    Binary(Box<Expr>, Box<Expr>, Token),
    Logical(Box<Expr>, Box<Expr>, Token),
    FunctionCall(Box<Expr>, Token, Vec<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(Box<Expr>, Token),  
    Variable(u16),
    Assign(u16, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement
{
    // DebugPrint
    Print(Expr),
    Return(Expr),
    Expression(Expr),
    // Defer(Expr),
    Decralation(u16, Type, Expr),
    Function(u16, Type, Vec<Statement>, Box<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Box<Statement>, Expr, Expr, Box<Statement>),
    Block(Vec<Statement>),
    Break,
    Continue,
    Empty,
}

impl ParsedData
{
    pub fn new(stmt: Statement, line: usize) -> Self
    {
        Self
        {
            line,
            value: stmt,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self
        {
            Expr::Binary(ref left, ref right, ref token) => write!(f, "{} {} {}", left, right, token),
            Expr::Logical(ref left, ref right, ref token) => write!(f, "{} {} {}", left, right, token),
            Expr::Grouping(ref inside) => write!(f, "({})", inside),
            Expr::Literal(ref lit) => write!(f, "{}", lit),
            Expr::Unary(ref item, ref t) => write!(f, "{}{}", t, item),
            Expr::Variable(ref s) => write!(f, "{}", s),
            Expr::Assign(s, ex) => write!(f, "{} = {}", s, ex),
            Expr::FunctionCall(ref name, _, ref items) => write!(f, "{}({:?})", name, items),
            x => write!(f, "unimplemented display expr {:?}", x),
        }
    }
}
