
use std::ops;
use std::fmt;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Types
{
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
}

impl fmt::Display for Types
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match &self
        {
            Types::Int(i)     => write!(f, "{}", i),
            Types::Float(f_)  => write!(f, "{}", f_),
            Types::Str(s)     => write!(f, "{}", &s),
            Types::Boolean(b) => write!(f, "{}", b),
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
                _ => unreachable!(),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl + ir as f64),
                Types::Float(fr) => Types::Float(fl + fr),
                _ => unreachable!(),
            },
            Types::Str(ref sl) =>
            {
                if let Types::Str(ref sr) = right
                {
                    Types::Str(format!("{}{}", sl, sr))
                } else { unreachable!() }
            },

            // Bool
            _ => unreachable!(),
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
                _ => unreachable!(),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl - ir as f64),
                Types::Float(fr) => Types::Float(fl - fr),
                _ => unreachable!(),
            },

            // Bool & Str cannot be subtracted from others
            _ => unreachable!(),
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
                _ => unreachable!(),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl * ir as f64),
                Types::Float(fr) => Types::Float(fl * fr),
                _ => unreachable!(),
            },

            // Bool & Str cannot be subtracted from others
            _ => unreachable!(),
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
                _ => unreachable!(),
            },
            Types::Float(fl) => match right
            {
                Types::Int(ir) => Types::Float(fl / ir as f64),
                Types::Float(fr) => Types::Float(fl / fr),
                _ => unreachable!(),
            },

            // Bool & Str cannot be subtracted from others
            _ => unreachable!(),
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
            Types::Str(s)     => unreachable!(),
            Types::Boolean(i) => unreachable!(),
        }
    }
}
