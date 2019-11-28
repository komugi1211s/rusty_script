use super::types::{ Type, Value };
use super::token::{ TokenType, Token };
use super::nativefunc::{ define_native_functions };
use super::parse::{ Expr, Visitor, Statement };
use std::collections::HashMap;
use std::mem;
use std::time::{SystemTime, UNIX_EPOCH};

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

    // pub fn define_function(&mut self, func: Value)
    // {
    //     if Value::Callable(x) = &func
    //     {
    //         if !x.retType.is_compatible(
    //     }
    // }
    //
    pub fn define(&mut self, name: &str, _type: Type, value: Value)
    {
        println!("define {}", name);
        if !_type.is_compatible(&value)
        {
            panic!("Should not work.: {:?} to {:?}", value, _type);
        }
        self.values.insert(name.to_string(), (_type, value));
    }

    pub fn assign(&mut self, name: &str, new_value: Value)
    {
        println!("assign {}", name);
        if self.values.contains_key(name)
        {
            let (c_type, v) = self.values.get(name).unwrap();
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
                unreachable!("Use of an uninitialized variable: {}", k);
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
        let mut z = Self {
            environment: Environment::new(),
        };

        define_native_functions(&mut z.environment);
        z.enter_block(Environment::new());
        // z.environment.define_function(nativefunc);
        z
    }

    fn enter_block(&mut self, new_env: Environment)
    {
        let previous = mem::replace(&mut self.environment, new_env);
        self.environment.connect(previous);
    }

    fn leave_block(&mut self) -> Environment
    {
        let original = mem::replace(&mut self.environment.enclose, None);
        if let Some(e) = original
        {
            return mem::replace(&mut self.environment, *e);
        }
        unreachable!();
    }

    pub fn visit_block(&mut self, inside: &Vec<Statement>) -> i32
    {
        for i in inside {
            match self.visit(i)
            {
                0 => (),
                1 => { return 1; },
                2 => { return 2; },
                _ => ()
            };
        }
        0
    } 
}

impl Visitor<Statement> for Interpreter
{
    type Result = i32;
    
    fn visit(&mut self, stmt: &Statement) -> i32
    {
        match stmt
        {
            Statement::Expression(e) => { self.visit(e); 0 },
            Statement::Decralation(_str, _type, lit) => {
                let lit = self.visit(lit);
                self.environment.define(_str, _type.clone(), lit);
                0
            },
            Statement::Function(name, _type, args, inside) => 
            {
                let func_block = &**inside;
                if let Statement::Block(func_inside) = func_block
                {
                    self.environment.define(name, _type.clone(), Value::Callable(
                            _type.clone(),
                            args.clone(),
                            func_inside.clone()
                    ));
                }
                0
            },
            Statement::Block(ref v) => {
                self.enter_block(Environment::new());
                let x = self.visit_block(v);
                self.leave_block();
                x
            },
            Statement::Print(_expr) => { println!("{}", self.visit(_expr)); 0 },
            Statement::If(_expr, _if, _else) => {
                if self.visit(_expr).is_truthy()
                {
                    self.visit(&**_if)
                }
                else
                {
                    if let Some(_el) = _else
                    {
                        self.visit(&**_el)
                    }
                    else 
                    { 0 }
                }
            },
            Statement::While(l, v) => {
                while self.visit(l).is_truthy()
                {
                    let x = self.visit(&**v);
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
            _ => 0,
        }
    }
}

impl Visitor<Expr> for Interpreter
{
    type Result = Value;

    fn visit(&mut self, expr: &Expr) -> Value
    {
        // Expr系は全部参照->Box->中身 の流れで渡される為、
        // 必ずDerefを2回挟んでからDeref後を参照する事 (&**expr)
        match expr
        {
            Expr::Variable(x) => self.environment.get(x).clone(),
            Expr::Literal(l) => l.clone(),
            Expr::FunctionCall(expr, _, args) => 
            {
                match self.visit(&**expr)
                {
                    Value::Callable(ref _type, ref require_arg, ref statement) => {
                        if require_arg.len() < args.len() {
                            panic!("Too much Argument");
                        }
                        let mut env = Environment::new();
                        self.enter_block(env);
                        for require in require_arg
                        {
                            if let Statement::Decralation(n, t, h) = require {
                                self.visit(require);
                            }
                            else {
                                panic!("how?");
                            }
                        }
                        for (arg, x) in args.iter().zip(require_arg.iter())
                        {
                            if let Statement::Decralation(ref n, _, _) = x {
                                let x = self.visit(arg);
                                if Value::Null != x {
                                    self.environment.assign(n, x);
                                }
                            }
                            else {
                                panic!("how?");
                            }
                        }
                        self.visit_block(statement);
                        self.leave_block();
                        Value::Null
                    },
                    Value::NativeCallable(_type, size, func) => {
                        if size != args.len()
                        {
                            panic!("Different Argument Size provided: needed {}, provided {}", size, args.len());
                        }
                        let mut vector: Vec<Value> = Vec::with_capacity(size);
                        for i in args
                        {
                            vector.push(self.visit(i));
                        }

                        (func)(vector)
                    }
                    _ => unreachable!("{:?}", self.visit(&**expr)),
                }
            },
            Expr::Binary(l, r, ref t) =>
            {
                let left = self.visit(&**l);
                let right = self.visit(&**r);

                match t.tokentype
                {
                    TokenType::Plus      => left + right,
                    TokenType::Minus     => left - right,
                    TokenType::Asterisk  => left * right,
                    TokenType::Slash     => left / right,
                    TokenType::Percent   => left % right,

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
            Expr::Logical(l, r, ref t) =>
            {
                let left = self.visit(&**l);
                let is_left_true = left.is_truthy();

                match t.tokentype
                {
                    TokenType::And => if is_left_true { self.visit(&**r) } else { left },
                    TokenType::Or  => if is_left_true { left } else { self.visit(&**r) },
                    _ => unimplemented!("Logical"),
                }
            },
            Expr::Unary(item, ref t) =>
            {
                let expr = self.visit(&**item);
                match t.tokentype 
                {
                    TokenType::Bang  => !expr,
                    TokenType::Minus => -expr, 
                    _ => unreachable!("Unary with unsupported Tokentype"),
                }
            },
            Expr::Grouping(g) => self.visit(&**g),
            Expr::Assign(s, exp) => 
            {
                let result = self.visit(&**exp);
                self.environment.assign(s, result);
                self.visit(&**exp)
            },
            x => panic!("Could not handle the expression: {:?}", x),
        }
    }
}

