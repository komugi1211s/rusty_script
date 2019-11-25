use super::types::{ Types };
use super::token::{ TokenType, Token };
use super::parse::{ Expr, Visitor, Statement };
use std::collections::HashMap;

pub struct Interpreter
{
    // pub states: Vec<Statement>,
    pub globals: HashMap<String, Types>,
}


impl Interpreter
{
    pub fn new() -> Self
    {
        Self {
            globals: HashMap::new()
        }
    }
    pub fn interpret(&mut self, expr: &Expr) -> Types
    {
        self.visit(expr)
    }
}

impl Visitor<Expr> for Interpreter
{
    type Result = Types;

    fn visit(&mut self, expr: &Expr) -> Types
    {
        match expr
        {
            Expr::Variable(x) => 
            {
                self.globals.get(x).unwrap().clone()
            },
            Expr::Literal(l) =>
            {
                l.clone()
            },
            Expr::Binary(ref l, ref r, ref t) =>
            {
                let left = self.visit(l);
                let right = self.visit(r);

                match t.tokentype
                {
                    TokenType::Plus      => left + right,
                    TokenType::Minus     => left - right,
                    TokenType::Asterisk  => left * right,
                    TokenType::Slash     => left / right,

                    // PartialEq Series
                    TokenType::NotEqual  => Types::Boolean(left != right),
                    TokenType::EqualEqual=> Types::Boolean(left == right),

                    // PartialOrd Series
                    TokenType::LessEqual => Types::Boolean(left <= right),
                    TokenType::MoreEqual => Types::Boolean(left >= right),
                    TokenType::Less      => Types::Boolean(left < right),
                    TokenType::More      => Types::Boolean(left > right),
                    _ => unreachable!(),
                }
            },
            Expr::Unary(item, ref t) =>
            {
                let expr = self.visit(item);
                match t.tokentype 
                {
                    TokenType::Bang  => !expr,
                    TokenType::Minus => -expr, 
                    _ => unreachable!(),
                }
            },
            Expr::Grouping(g) => self.visit(g),
        }
    }

}

