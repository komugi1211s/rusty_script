
use super::token::{ TokenType };
use std::ops;
use std::fmt;
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub enum Types
{
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Null
}

impl Types
{
    pub fn match_token(&self, _type: &TokenType) -> bool
    {
        if _type == &TokenType::TypeVoid
        {
            true
        }
        else 
        {
            match self
            {
                Types::Null       => true,
                Types::Int(_)     => _type == &TokenType::TypeInt,
                Types::Float(_)   => _type == &TokenType::TypeFloat,
                Types::Str(_)     => _type == &TokenType::TypeStr,
                Types::Boolean(_) => _type == &TokenType::TypeBool,
                _                 => _type == &TokenType::TypeVoid,
            }
        }
    }

    pub fn is_same_type(&self, other: &Types) -> bool
    {
        mem::discriminant(self) == mem::discriminant(&Types::Null)
        || mem::discriminant(self) == mem::discriminant(other)
    }
}

impl fmt::Display for Types
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match &self
        {
            Types::Int(i)     => write!(f, "{}", i),
            Types::Float(f_)  => write!(f, "{}", f_),
            Types::Str(ref s) => write!(f, "{}", s),
            Types::Boolean(b) => write!(f, "{}", b),
            Types::Null       => write!(f, "null"),
        }
    }
}

impl ops::Add<Types> for Types
{
    type Output = Types;

    fn add(self, right: Types) -> Types
    {
        match self
        {
            Types::Int(il) => match right 
            {
                Types::Int(ir) => Types::Int(il + ir),
                Types::Float(fr) => Types::Float(il as f64 + fr),
                x@ _ => unreachable!("TypeError: Can't add Int and {} together", x),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl + ir as f64),
                Types::Float(fr) => Types::Float(fl + fr),
                x@ _ => unreachable!("TypeError: Can't add Float and {} together", x),
            },
            Types::Str(ref sl) =>
            {
                if let Types::Str(ref sr) = right
                {
                    Types::Str(format!("{}{}", sl, sr))
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

impl ops::Sub<Types> for Types
{
    type Output = Types;

    fn sub(self, right: Types) -> Types
    {
        match self
        {
            Types::Int(il) => match right 
            {
                Types::Int(ir) => Types::Int(il - ir),
                Types::Float(fr) => Types::Float(il as f64 - fr),
                x@ _ => unreachable!("TypeError: Can't subtract {} from Int", x),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl - ir as f64),
                Types::Float(fr) => Types::Float(fl - fr),
                x@ _ => unreachable!("TypeError: Can't subtract {} from Float", x),
            },

            x@ _ => unreachable!("TypeError: {} does not support Subtraction", x),
        }
    }
}

impl ops::Mul<Types> for Types
{
    type Output = Types;

    fn mul(self, right: Types) -> Types
    {
        match self
        {
            Types::Int(il) => match right 
            {
                Types::Int(ir) => Types::Int(il * ir),
                Types::Float(fr) => Types::Float(il as f64 * fr),
                x@ _ => unreachable!("TypeError: Can't multiply Int and {}", x),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl * ir as f64),
                Types::Float(fr) => Types::Float(fl * fr),
                x@ _ => unreachable!("TypeError: Can't multiply Float and {}", x),
            },


            // Bool & Str cannot be subtracted from others
            x@ _ => unreachable!("TypeError: {} does not support Multiplification", x),
        }
    }
}

impl ops::Div<Types> for Types
{
    type Output = Types;

    fn div(self, right: Types) -> Types
    {
        match self
        {
            Types::Int(il) => match right 
            {
                Types::Int(ir) => Types::Float(il as f64 / ir as f64),
                Types::Float(fr) => Types::Float(il as f64 / fr),
                x@ _ => unreachable!("TypeError: Cannot divide Int with {}", x),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl / ir as f64),
                Types::Float(fr) => Types::Float(fl / fr),
                x@ _ => unreachable!("TypeError: Cannot divide Float with {}", x),
            },

            // Bool & Str cannot be subtracted from others
            x@ _ => unreachable!("TypeError: {} does not support Division", x),
        }
    }
}

impl ops::Not for Types
{
    type Output = Types;
    
    fn not(self) -> Types
    {
        match self 
        {
            Types::Boolean(b) => Types::Boolean(!b),
            Types::Int(i)     => Types::Boolean(i != 0),
            Types::Float(f)   => Types::Boolean(f != 0.0),
            Types::Str(s)     => Types::Boolean(s.len() != 0),
            Types::Null       => Types::Boolean(false),
        }
    }
}

impl ops::Neg for Types
{
    type Output = Types;

    fn neg(self) -> Types
    {
        match self
        {
            Types::Int(i)     => Types::Int(-i),
            Types::Float(f)   => Types::Float(-f),
            x@ _                 => unreachable!("TypeError:{} does not support negation", x),
        }
    }
}

use std::cmp::Ordering;
impl PartialOrd for Types
{
    fn partial_cmp(&self, other: &Types) -> Option<Ordering>
    {
        match self
        {
            Types::Int(i_left) => match other
            {
                Types::Int(i_right) => i_left.partial_cmp(i_right),
                Types::Float(f_right) => (*i_left as f64).partial_cmp(f_right),
                _ => unreachable!("Comparison with unsupported type"),
            },
            Types::Float(f_left) => match other
            {
                Types::Int(i_right) => f_left.partial_cmp(&(*i_right as f64)),
                Types::Float(f_right) => f_left.partial_cmp(f_right),
                _ => unreachable!("Comparison with unsupported type"),
            },
            _ => unreachable!("Comparison with unsupported type"),
        }
    }
}
