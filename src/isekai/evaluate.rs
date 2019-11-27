use super::types::{ Type, Value };
use super::token::{ TokenType, Token };
use super::parse::{ Expr, Visitor, Statement };
use std::collections::HashMap;
use std::mem;

pub struct Environment 
{
    pub values: HashMap<String, (Type, Value)>,
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

    pub fn define(&mut self, name: &str, _type: Type, value: Value)
    {
        if !_type.is_compatible(&value)
        {
            panic!("Should not work.: {:?} to {:?}", value, _type);
        }
        self.values.insert(name.to_string(), (_type, value));
    }

    pub fn assign(&mut self, name: &str, new_value: Value)
    {
        if self.values.contains_key(name)
        {
            let (c_type, _) = self.values.get(name).unwrap();
            let c_type = c_type.clone();
            if !c_type.is_compatible(&new_value)
            {
                unreachable!("Mismatched Value: {:?}: {} to {:?}", new_value.to_type(), name, c_type);
            }
            
            self.values.insert(name.to_string(), (c_type, new_value));
        }
        else if let Some(ref mut enc) = self.enclose
        {
            enc.assign(name, new_value);
        }
        else
        {
            unreachable!("Undefined Variable: {}", name);
        }
    }

    pub fn get(&self, k: &str) -> &Value
    {
        if self.values.contains_key(k)
        {
            let (_, x) = self.values.get(k).unwrap();
            if x.is_same_type(&Value::Null)
            {
                unreachable!("Use of possibly uninitialized variable: {}", k);
            }
            x
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
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmt: &Statement) -> i32
    {
        match stmt
        {
            Statement::Expression(e) => { self.visit(e); 0 },
            Statement::Decralation(_str, _type, lit) => {
                let lit = self.visit(lit);
                self.environment.define(_str, _type.clone(), lit);
                0
            },
            Statement::Block(ref v) => {
                self.visit_block(v);
            },
            Statement::Print(_expr) => { println!("{}", self.visit(_expr)); 0 },
            Statement::If(_expr, ref _if, ref _else) => {
                if self.visit(_expr).is_truthy()
                {
                    self.interpret(&*_if);
                }
                else
                {
                    if let Some(_el) = _else
                    {
                        self.interpret(&*_el);
                    }
                }
                0
            },
            Statement::While(l, ref v) => {
                while self.visit(l).is_truthy()
                {
                    let x = self.interpret(&*v);
                    if x == 1 {
                        break;
                    }
                    if x == 2 {
                        continue;
                    }
                }
                0
            },
            Statement::Break => {
                return 1;
            },
            Statement::Continue => {
                return 2;
            },
            b => println!("Can't handle that right now!: {:?}", b),
        }
    }

    pub fn visit_block(&mut self, inside: &Vec<Statement>)
    {

        let new_nev = Environment::new();
        let previous = mem::replace(&mut self.environment, new_nev);
        self.environment.connect(previous);

        for i in inside {
            match self.interpret(i);
        }

        let original = mem::replace(&mut self.environment.enclose, None);
        if let Some(e) = original
        {
            self.environment = *e;
        }
    } 
}

impl Visitor<Expr> for Interpreter
{
    type Result = Value;

    fn visit(&mut self, expr: &Expr) -> Value
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
                    TokenType::NotEqual  => Value::Boolean(left != right),
                    TokenType::EqualEqual=> Value::Boolean(left == right),

                    // PartialOrd Series
                    TokenType::LessEqual => Value::Boolean(left <= right),
                    TokenType::MoreEqual => Value::Boolean(left >= right),
                    TokenType::Less      => Value::Boolean(left < right),
                    TokenType::More      => Value::Boolean(left > right),
                    _ => unimplemented!("Binary"),
                }
            },
            Expr::Logical(ref l, ref r, ref t) =>
            {
                let left = self.visit(l);
                let is_left_true = left.is_truthy();

                match t.tokentype
                {
                    TokenType::And => if is_left_true { self.visit(r) } else { left },
                    TokenType::Or  => if is_left_true { left } else { self.visit(r) },
                    _ => unimplemented!("Logical"),
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

