 

use super::token::{ TokenType };
pub trait Visitor<T>
{
    fn visit(&mut self, t: &T);
}


#[derive(Debug, Clone)]
pub enum Expr
{
    Binary(Box<Expr>, Box<Expr>, TokenType),
    Grouping(Box<Expr>),
    Literal(TokenType),
    Unary(Box<Expr>, TokenType),  
}

struct ExprVisitor;

impl Visitor<Expr> for ExprVisitor
{
    fn visit(&mut self, expr: &Expr)
    {
        match expr
        {
            Expr::Binary(left, right, token) => (),
            Expr::Grouping(group) => (),
            Expr::Literal(token) => (),
            Expr::Unary(token, value) => (),
        }
    }
}

/*
pub trait Expr {
}

pub struct Binary<EX> {
    left: EX,
    right: EX,
    oper: TokenType,
}

impl<EX: Expr> Binary<EX> {
    pub fn new(left: EX, right: EX, oper: TokenType) -> Self {
        Self {
            left,
            right,
            oper,
        }
    }
}

pub struct Grouping<EX> {
    expr: EX,
}

impl<EX: Expr> Grouping<EX> {
    pub fn new(expr: EX) -> Self {
        Self {
            expr,
        }
    }
}

pub struct Literal {
    value: TokenType,
}

impl Literal {
    pub fn new(value: TokenType) -> Self {
        Self {
            value,
        }
    }
}

struct Unary<EX> {
    oper: TokenType,
    right: EX,
}

impl<EX: Expr> Unary<EX> {
    pub fn new(oper: TokenType, right: EX) -> Self {
        Self {
            oper,
            right,
        }
    }
}
*/
