
use super::token::{ TokenType };
use super::parse::{ Statement };
use super::evaluate::{ Environment, Interpreter };
use std::ops;
use std::fmt;
use std::mem;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Type
{
    Int,
    Float,
    Str,
    Boolean,
    Null,
    Any
}

impl Type
{
    pub fn from_tokentype(t: &TokenType) -> Self
    {
        match t
        {
            TokenType::TypeAny   => Self::Any,
            TokenType::TypeBool  => Self::Boolean,
            TokenType::TypeFloat => Self::Float,
            TokenType::TypeInt   => Self::Int,
            TokenType::TypeStr   => Self::Str,
            // This SHould not work.
            _                    => Self::Null,
        }
    }
    pub fn is_compatible(&self, v: &Value) -> bool
    {
        if v == &Value::Null
        {
            true
        }
        else
        {
            match self
            {
                Type::Int     => v.to_type() == Type::Int,
                Type::Boolean => v.to_type() == Type::Boolean,
                Type::Str     => v.to_type() == Type::Str,
                Type::Float   => v.to_type() == Type::Float,
                Type::Any     => true,
                Type::Null    => false,
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value
{
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Callable(Type, Vec<Statement>, Vec<Statement>),
    NativeCallable(Type, usize, fn(Vec<Value>) -> Value),
    Null
}

impl From<i64> for Type 
{
    fn from(_: i64) -> Self
    { Type::Int }
}
impl From<f64> for Type
{
    fn from(_: f64) -> Self
    { Type::Float }
}

impl From<String> for Type
{
    fn from(_: String) -> Self
    { Type::Str }
}

impl From<bool> for Type
{
    fn from(_: bool) -> Self
    { Type::Boolean }
}

impl From<i64> for Value 
{
    fn from(i: i64) -> Self
    { Value::Int(i) }
}
impl From<f64> for Value
{
    fn from(f: f64) -> Self
    { Value::Float(f) }
}

impl From<String> for Value
{
    fn from(s: String) -> Self
    { Value::Str(s) }
}

impl From<bool> for Value
{
    fn from(b: bool) -> Self
    { Value::Boolean(b) }
}

impl Value
{
    pub fn to_type(&self) -> Type
    {
        // Todo: Type Alias support
        match self
        {
            Value::Null        => Type::Null,
            Value::Int(_)      => Type::Int,
            Value::Float(_)    => Type::Float,
            Value::Str(_)      => Type::Str,
            Value::Boolean(_)  => Type::Boolean,
            Value::Callable(t, _, _) => t.clone(),
            Value::NativeCallable(t, _, _) => t.clone(),
            _                  => Type::Any,
        }
    }

    pub fn is_same_type(&self, other: &Value) -> bool
    {
        mem::discriminant(self) == mem::discriminant(&Value::Null)
        || mem::discriminant(self) == mem::discriminant(other)
    }

    pub fn is_truthy(&self) -> bool
    {
        // NOTE: This should work since Value implement ops::Not
        if let Value::Boolean(x) = !!self
        {
            x
        }
        else
        {
            panic!("Interpreter Internal Error");
        }
    }
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match &self
        {
            Value::Int(i)      => write!(f, "{}", i),
            Value::Float(f_)   => write!(f, "{}", f_),
            Value::Str(ref s)  => write!(f, "{}", s),
            Value::Boolean(b)  => write!(f, "{}", b),
            Value::Callable(t, a, _) => write!(f, "<CALLABLE> {:?} {:?}", t, a),
            Value::NativeCallable(t, a, _) => write!(f, "<CALLABLE> {:?} {:?}", t, a),
            Value::Null        => write!(f, "null"),
        }
    }
}

impl ops::Add<Value> for Value
{
    type Output = Value;

    fn add(self, right: Value) -> Value
    {
        match self
        {
            Value::Int(il) => match right 
            {
                Value::Int(ir) => Value::Int(il + ir),
                Value::Float(fr) => Value::Float(il as f64 + fr),
                x@ _ => unreachable!("TypeError: Can't add Int and {} together", x),
            },
            Value::Float(fl) => match right
            {
                Value::Int(ir) => Value::Float(fl + ir as f64),
                Value::Float(fr) => Value::Float(fl + fr),
                x@ _ => unreachable!("TypeError: Can't add Float and {} together", x),
            },
            Value::Str(ref sl) =>
            {
                if let Value::Str(ref sr) = right
                {
                    Value::Str(format!("{}{}", sl, sr))
                } 
                else
                {
                    unreachable!("TypeError: Can't add String and {} together", right)
                }
            },

            // Bool
            x@ _ => unreachable!("TypeError: {} does not support Addition", x),
        }
    }
}

impl ops::Sub<Value> for Value
{
    type Output = Value;

    fn sub(self, right: Value) -> Value
    {
        match self
        {
            Value::Int(il) => match right 
            {
                Value::Int(ir) => Value::Int(il - ir),
                Value::Float(fr) => Value::Float(il as f64 - fr),
                x@ _ => unreachable!("TypeError: Can't subtract {} from Int", x),
            },
            Value::Float(fl) => match right
            {
                Value::Int(ir) => Value::Float(fl - ir as f64),
                Value::Float(fr) => Value::Float(fl - fr),
                x@ _ => unreachable!("TypeError: Can't subtract {} from Float", x),
            },

            x@ _ => unreachable!("TypeError: {} does not support Subtraction", x),
        }
    }
}

impl ops::Mul<Value> for Value
{
    type Output = Value;

    fn mul(self, right: Value) -> Value
    {
        match self
        {
            Value::Int(il) => match right 
            {
                Value::Int(ir) => Value::Int(il * ir),
                Value::Float(fr) => Value::Float(il as f64 * fr),
                x@ _ => unreachable!("TypeError: Can't multiply Int and {}", x),
            },
            Value::Float(fl) => match right
            {
                Value::Int(ir) => Value::Float(fl * ir as f64),
                Value::Float(fr) => Value::Float(fl * fr),
                x@ _ => unreachable!("TypeError: Can't multiply Float and {}", x),
            },


            // Bool & Str cannot be subtracted from others
            x@ _ => unreachable!("TypeError: {} does not support Multiplification", x),
        }
    }
}

impl ops::Div<Value> for Value
{
    type Output = Value;

    fn div(self, right: Value) -> Value
    {
        match self
        {
            Value::Int(il) => match right 
            {
                Value::Int(ir) => Value::Float(il as f64 / ir as f64),
                Value::Float(fr) => Value::Float(il as f64 / fr),
                x@ _ => unreachable!("TypeError: Cannot divide Int with {}", x),
            },
            Value::Float(fl) => match right
            {
                Value::Int(ir) => Value::Float(fl / ir as f64),
                Value::Float(fr) => Value::Float(fl / fr),
                x@ _ => unreachable!("TypeError: Cannot divide Float with {}", x),
            },

            // Bool & Str cannot be subtracted from others
            x@ _ => unreachable!("TypeError: {} does not support Division", x),
        }
    }
}

impl ops::Rem<Value> for Value
{
    type Output = Value;
    fn rem(self, right: Value) -> Value
    {
        match self
        {
            Value::Int(il) => match right
            {
                Value::Int(ir) => Value::Float(il as f64 % ir as f64),
                Value::Float(fr) => Value::Float(il as f64 % fr),
                _ => panic!("Rem operation is not covered for Str / Bool")
            },
            Value::Float(fl) => match right
            {
                Value::Int(ir) => Value::Float(fl % ir as f64),
                Value::Float(fr) => Value::Float(fl % fr),
                _ => panic!("Rem operation is not covered for Str / Bool")
            },
            _ => panic!("Rem operation is not covered for Str / Bool")
        }
    }
}

impl ops::Not for Value
{
    type Output = Value;
    
    fn not(self) -> Value
    {
        match self 
        {
            Value::Boolean(b) => Value::Boolean(!b),
            Value::Int(i)     => Value::Boolean(i == 0),
            Value::Float(f)   => Value::Boolean(f == 0.0),
            Value::Str(s)     => Value::Boolean(s.len() == 0),
            Value::Null       => Value::Boolean(true),
            _                 => panic!("Unimplemented Not operation"),
        }
    }
}

// TODO:
// Cleanup: This feels weird.
impl ops::Not for &Value
{
    type Output = Value;
    
    fn not(self) -> Value
    {
        match self 
        {
            Value::Boolean(b) => Value::Boolean(!b),
            Value::Int(i)     => Value::Boolean(*i == 0),
            Value::Float(f)   => Value::Boolean(*f == 0.0),
            Value::Str(s)     => Value::Boolean(s.len() == 0),
            Value::Null       => Value::Boolean(true),
            _                 => panic!("Unimplemented Not operation"),
        }
    }
}

impl ops::Neg for Value
{
    type Output = Value;

    fn neg(self) -> Value
    {
        match self
        {
            Value::Int(i)     => Value::Int(-i),
            Value::Float(f)   => Value::Float(-f),
            x@ _                 => unreachable!("TypeError:{} does not support negation", x),
        }
    }
}

use std::cmp::Ordering;
impl PartialOrd for Value
{
    fn partial_cmp(&self, other: &Value) -> Option<Ordering>
    {
        match self
        {
            Value::Int(i_left) => match other
            {
                Value::Int(i_right) => i_left.partial_cmp(i_right),
                Value::Float(f_right) => (*i_left as f64).partial_cmp(f_right),
                _ => unreachable!("Comparison with unsupported type"),
            },
            Value::Float(f_left) => match other
            {
                Value::Int(i_right) => f_left.partial_cmp(&(*i_right as f64)),
                Value::Float(f_right) => f_left.partial_cmp(f_right),
                _ => unreachable!("Comparison with unsupported type"),
            },
            _ => unreachable!("Comparison with unsupported type"),
        }
    }
}
