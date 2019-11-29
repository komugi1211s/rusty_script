use super::types::{ Type, Value };
use super::token::{ TokenType, Token };
use super::nativefunc::{ define_native_functions };
use super::parse::{ Expr, Visitor, Statement };
use std::collections::HashMap;
use std::mem;
use std::time::{SystemTime, UNIX_EPOCH};

type EnvHashMap = HashMap<String, (Type, Value)>;
pub struct Environment 
{
    pub values: Vec<EnvHashMap>,
    pub current: usize,
}

pub enum InterpError
{
    Return(Value),
    Break,
    Continue,
    NoOp
}

impl Environment 
{
    pub fn new() -> Self
    {
        Self {
            values: vec![HashMap::new()],
            current: 0,
        }
    }

    pub fn connect(&mut self, env: Environment) -> usize
    {
        for i in env.values
        {
            self.values.push(i);
            self.current += 1;
        }
        self.current
    }

    // pub fn define_function(&mut self, func: Value)
    // {
    //     if Value::Callable(x) = &func
    //     {
    //         if !x.retType.is_compatible(
    //     }
    // }
    //
    pub fn enter_block(&mut self) -> usize
    {
        self.values.push(HashMap::new());
        self.current += 1;
        self.current
    }

    pub fn leave_block(&mut self) -> (usize, EnvHashMap)
    {
        self.current -= 1;
        (self.current, self.values.pop().unwrap())
    }
    pub fn is_toplevel(&self) -> bool
    {
        self.current == 0
    }

    pub fn define(&mut self, name: &str, _type: Type, value: Value)
    {
        if !_type.is_compatible(&value)
        {
            unreachable!("Mismatched Type and Value: {:?}: {} = {:?}", _type, name, value.to_type());
        }
        let mut insert_key = self.current;
        let exist_key = self.exist(name);
        if let Some(key) = exist_key 
        {
            if key == self.current
            {
                let declared = self.values[key].get(name).unwrap();
                if !declared.0.is_compatible(&value)
                {
                    insert_key = key;
                }
                else
                {
                    unreachable!("Variable Declared Twice - Use an assignment instead.");
                }
            }
        }
        self.values[insert_key].insert(name.to_string(), (_type, value));
    }

    pub fn assign(&mut self, name: &str, new_value: Value)
    {
        let exist_key = self.exist(name);
        if exist_key.is_none() {
            panic!("Undefined Variable {}", name);
        }
        let exist_key = exist_key.unwrap();

        let (c_type, v) = self.values[exist_key].get(name).unwrap();
        let c_type = c_type.clone();
        if !c_type.is_compatible(&new_value)
        {
            unreachable!("Mismatched Type and Value: {:?}: {} = {:?}", c_type, name, new_value.to_type());
        }
            
        self.values[exist_key].insert(name.to_string(), (c_type, new_value));
    }

    pub fn exist(&self, name: &str) -> Option<usize>
    {
        let mut counter = self.current;
        while !self.values[counter].contains_key(name)
        {
            if counter <= 0
            {
                return None;
            }
            counter -= 1;
        }
        Some(counter)
    }

    pub fn get(&self, name: &str) -> &Value
    {
        let exist_key = self.exist(name);
        if exist_key.is_none() {
            panic!("Undefined Variable {}", name);
        }

        let (_, x) = self.values[exist_key.unwrap()].get(name).unwrap();
        if x.is_same_type(&Value::Null)
        {
            unreachable!("Use of an uninitialized variable: {}", name);
        }
        x
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
        z.enter_block();
        // z.environment.define_function(nativefunc);
        z
    }

    fn enter_block(&mut self) -> usize
    {
        self.environment.enter_block()
    }

    fn leave_block(&mut self) -> (usize, EnvHashMap)
    {
        self.environment.leave_block()
    }

    pub fn visit_block(&mut self, inside: &Vec<Statement>) -> Result<(), InterpError>
    {
        for i in inside {
            self.visit(i)?;
        }
        Ok(())
    } 
}

impl Visitor<Statement> for Interpreter
{
    type Result = Result<(), InterpError>;
    
    fn visit(&mut self, stmt: &Statement) -> Self::Result
    {
        match stmt
        {
            Statement::Expression(e) => { self.visit(e); Ok(()) },
            Statement::Decralation(_str, _type, lit) => {
                let lit = self.visit(lit);
                self.environment.define(_str, _type.clone(), lit);
                Ok(())
            },
            Statement::Function(name, _type, args, inside) => 
            {
                let func_block = &**inside;
                if let Statement::Block(func_inside) = func_block
                {
                    self.environment.define(name, _type.clone(), 
                        Value::Callable(
                            _type.clone(),
                            args.clone(),
                            func_inside.clone()
                    ));
                }
                Ok(())
            },
            Statement::Block(ref v) => {
                self.enter_block();
                let x = self.visit_block(v);
                self.leave_block();
                x
            },
            Statement::Print(_expr) => { println!("{}", self.visit(_expr)); Ok(()) },
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
                    { Ok(()) }
                }
            },
            Statement::Return(_expr) => { 
                if self.environment.is_toplevel()
                {
                    panic!("Cannot use Return statement from top-level code");
                }

                Err(InterpError::Return(self.visit(_expr)))
            },
            Statement::While(l, v) => {
                while self.visit(l).is_truthy()
                {
                    let x = self.visit(&**v);
                    match x {
                        Ok(_) => (),
                        Err(ref e) => match e {
                            InterpError::Break => break,
                            InterpError::Continue => continue,
                            _ => ()
                        }
                    }
                    x?;
                }

                Ok(())
            },
            Statement::Break => Err(InterpError::Break),
            Statement::Continue => Err(InterpError::Continue),
            _ => Err(InterpError::NoOp),
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
            Expr::FunctionCall(expr, _, provided_args) => 
            {
                match self.visit(&**expr)
                {
                    Value::Callable(ref func_return_type, ref require_args, ref statement) =>
                    {
                        if require_args.len() < provided_args.len()
                        {
                            panic!("Too much Argument");
                        }

                        self.enter_block();
                        for (arg_count, function) in require_args.iter().enumerate()
                        {
                            if let Statement::Decralation(fa_name, fa_type, ref fa_value_expr) = function
                            {
                                let pa_value = match provided_args.get(arg_count)
                                {
                                    Some(pa_value_expr) => self.visit(pa_value_expr),
                                    None => Value::Null,
                                };
                                {
                                    // Different Type Provided.
                                    if !fa_type.is_compatible(&pa_value)
                                    {
                                        panic!("Mismatched Argument Type at argument {} named {}: required {:?}, provided {:?}", arg_count, fa_name, fa_type, pa_value.to_type());
                                    }

                                    // Positional Argument did not provided
                                    if pa_value == Value::Null
                                    {
                                        let fa_value = self.visit(fa_value_expr);
                                        // it was the required Argument. raise an error
                                        if fa_value == Value::Null
                                        {
                                            panic!("Required Argument {} named {} did not provided: require {:?}", arg_count, fa_name, fa_type);
                                        }
                                        else
                                        {
                                            self.environment.define(fa_name, fa_type.clone(), fa_value);
                                        }
                                    }
                                    // Argument Provided.
                                    else
                                    {
                                        self.environment.define(fa_name, fa_type.clone(), pa_value);
                                    }
                                }
                            }
                        }
                        let z = self.visit_block(statement);
                        self.leave_block();
                        match z {
                            Ok(()) => Value::Null,
                            Err(e) => match e
                            {
                                InterpError::Return(val) => 
                                { 
                                    if func_return_type.is_compatible(&val) { val } else { panic!("Different Return type.") }
                                },
                                _ => unreachable!(),
                            }
                        }
                    },
                    Value::NativeCallable(_type, args, func) => {
                        if args.len() != provided_args.len()
                        {
                            panic!("Different Argument Size provided: needed {}, provided {}", args.len(), provided_args.len());
                        }

                        let mut vector: Vec<Value> = Vec::with_capacity(args.len());
                        for (arg_count, (default_arg, provided_arg)) in args.iter().zip(provided_args.iter()).enumerate()
                        {
                            // default_arg = (Type, Value)
                            // default_arg.1 == Value::Null means it's a required argument.
                            let provided_value = self.visit(provided_arg);
                            if !default_arg.0.is_compatible(&provided_value)
                            {
                                panic!("Mismatched Argument Type at argument {}: required {:?}, provided {:?}", arg_count, default_arg.0, provided_value.to_type());
                            }

                            // Different Type Provided.
                            if provided_value == Value::Null
                            {
                                if default_arg.1 == Value::Null
                                {
                                    panic!("Required Argument {} did not provided: require {:?}", arg_count, default_arg.0);
                                }
                                vector.push(default_arg.1.clone());
                            }
                            else
                            {
                                vector.push(provided_value);
                            }
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

