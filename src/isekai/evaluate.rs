use super::types::{ Types };
use super::token::{ TokenType, Token };
use super::parse::{ Expr, Visitor, Statement };
use std::collections::HashMap;

pub struct Environment 
{
    pub values: HashMap<String, Types>,
    pub enclose: Option<Box<Environment>>
}
impl Environment 
{
    pub fn new() -> Self
    {
        Self {
            values: HashMap::new(),
            enclose: None,
        }
    }

    pub fn connect(&mut self, env: Environment)
    {
        self.enclose = Some(Box::new(env));
    }

    pub fn define(&mut self, name: &str, _type: &TokenType, value: Types)
    {
        if !value.match_token(_type)
        {
            panic!("Should not work.: {:?} to {:?}", value, _type);
        }
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &str, value: Types)
    {
        if self.values.contains_key(name)
        {
            self.values.insert(name.to_string(), value);
        }
        else if let Some(ref mut enc) = self.enclose
        {
            enc.assign(name, value);
        }
        else
        {
            unreachable!("Undefined Variable: {}", name);
        }
    }

    pub fn get(&self, k: &str) -> &Types
    {
        if self.values.contains_key(k)
        {
            self.values.get(k).unwrap()
        }
        else if let Some(ref enc) = self.enclose
        {
            enc.get(k)
        }
        else
        {
            unreachable!("Undefined Variable: {}", k);
        }
    }
}

pub struct Interpreter
{
    // pub states: Vec<Statement>,
    pub environment: Environment,
}


impl Interpreter
{
    pub fn new() -> Self
    {
        Self {
            environment: Environment::new()
        }
    }

    pub fn interpret(&mut self, expr: &Statement)
    {
        match expr
        {
            Statement::Expression(e) => { self.visit(e); },
            Statement::Decralation(_str, _type, lit) => {
                let lit = self.visit(lit);
                self.environment.define(_str, _type, lit)
            },

            Statement::Print(_expr) => println!("{}", self.visit(_expr)),
            b => println!("Can't handle that right now!: {:?}", b),
        }
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
                self.environment.get(x).clone()
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
                    _ => unimplemented!("Binary"),
                }
            },
            Expr::Unary(item, ref t) =>
            {
                let expr = self.visit(item);
                match t.tokentype 
                {
                    TokenType::Bang  => !expr,
                    TokenType::Minus => -expr, 
                    _ => unreachable!("Unary with unsupported Tokentype"),
                }
            },
            Expr::Grouping(g) => self.visit(g),
            Expr::Assign(s, exp) => 
            {
                let result = self.visit(exp);
                self.environment.assign(s, result);
                self.visit(exp)
            }
        }
    }
}

