use std::fmt;
use super::types::{ Types };
use super::token::{ Token, TokenType };

pub trait Visitor<T>
{
    type Result;
    fn visit(&mut self, t: &T) -> Self::Result;
}

#[derive(Debug, Clone)]
pub enum Expr
{
    Binary(Box<Expr>, Box<Expr>, Token),
    Grouping(Box<Expr>),
    Literal(Types),
    Unary(Box<Expr>, Token),  
    Variable(String),
}

#[derive(Debug, Clone)]
pub enum Statement
{
    // DebugPrint
    Print(Expr),

    Expression(Expr),
    Decralation(String, TokenType, Expr),
    If(Expr),
    Else(Box<Statement>),
    While(Expr),
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self
        {
            Expr::Binary(ref left, ref right, ref token) => write!(f, "{} {} {}", left, right, token),
            Expr::Grouping(ref inside) => write!(f, "({})", inside),
            Expr::Literal(ref lit) => write!(f, "{}", lit),
            Expr::Unary(ref item, ref t) => write!(f, "{}{}", t, item),
            Expr::Variable(ref s) => write!(f, "{}", s),
        }
    }
}
