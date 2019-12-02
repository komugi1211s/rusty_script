#[macro_use]
use bitflags;

use super::token::{ TokenType };
use super::parse::{ Statement };
// use super::evaluate::{ Environment, Interpreter };
use std::ops;
use std::fmt;
use std::mem;


#[cfg(target_pointer_width="32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width="64")]
const USIZE_LENGTH: usize = 8;

#[derive(Debug, Clone, Hash, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode
{
    // Const       = 0b01000000,
    Const8      = 0b01000001, // usize分の大きさのオペランドを取る
    Const16     = 0b01000010, // usize分の大きさのオペランドを取る
    Const32     = 0b01000011, // usize分の大きさのオペランドを取る
    Const64     = 0b01000100, // usize分の大きさのオペランドを取る
    ConstDyn    = 0b01000111, // usize分の大きさのオペランドを取る
    ConstPtr    = 0b01001000, // usize分の大きさのオペランドを取る

    // Operational = 0b00010000,
    Return      = 0b00010000,
    Define      = 0b00010001, // Type 名前のu16(1) 名前のu16(2) の3つのオペランドを取る
    Push        = 0b00010010,
    Pop         = 0b00010011,
    Read        = 0b00010100, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    Write       = 0b00010101, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る

    // Arithmitic = 0b00100000,
    Add        = 0b00100001,
    Sub        = 0b00100010,
    Mul        = 0b00100011,
    Div        = 0b00100100,
    Mod        = 0b00100101,
    Not        = 0b00100111,
    Neg        = 0b00101000,

    // Logical    = 0b00110000,
    EqEq       = 0b00110001,
    NotEq      = 0b00110010,
    LessEq     = 0b00110011,
    MoreEq     = 0b00110100,
    Less       = 0b00110101,
    More       = 0b00110111,
    And        = 0b00111000,
    Or         = 0b00111001,

    // System     = 0b11110000,
    Interrupt  = 0b11111111,
    DebugPrint = 0b11110001,
}

pub trait toVmByte
{
    fn to_vm_byte(&self) -> Vec<u8>;
    fn sufficient_opcode(&self) -> OpCode;
}

impl toVmByte for u16 
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_ne_bytes().iter().cloned().collect()
    }
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const16
    }
}

impl toVmByte for i64 
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_ne_bytes().iter().cloned().collect()
    }
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const64
    }
}

impl toVmByte for f64
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_bits().to_ne_bytes().iter().cloned().collect()
    }

    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const64
    }
}

impl toVmByte for String
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.clone().into_bytes()
    }
    
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::ConstDyn
    }
}

impl toVmByte for bool
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        vec![*self as u8]
    }
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const8
    }
}

impl toVmByte for usize
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_ne_bytes().iter().cloned().collect()
    }

    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Interrupt
    }
}


bitflags! {
    pub struct Type: u8 {
        const Null    = 0b10000000;
        const Int     = 0b00000001;
        const Float   = 0b00000010;
        const Str     = 0b00000100;
        const Boolean = 0b00001000;
        const Type    = 0b00010000;
        const Any     = 0b00100000;
    }
}

impl Type
{
    pub fn set_nullable(&mut self)
    {
        self.insert(Type::Null);
    }

    pub fn is_nullable(&self) -> bool
    {
        self.contains(Type::Null)
    }

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
    pub fn is_compatible(self, v: &Value) -> bool
    {
        if v == &Value::Null
        {
            true
        }
        else
        {
            if self.contains(Self::Any) {
                true
            }
            else {
                self.contains(v.to_type())
            }
            // match self
            // {
            //     Type::Int     => v.to_type().contains(Type::Int),
            //     Type::Boolean => v.to_type().contains(Type::Boolean),
            //     Type::Str     => v.to_type().contains(Type::Str),
            //     Type::Float   => v.to_type().contains(Type::Float),
            //     Type::Type    => if let Value::Type(_) = v { true } else { false },
            //     Type::Any     => true,
            //     Type::Null    => {
            //         self.remove(Type::Null);
            //         self.is_compatible(v)
            //     },
            //     _ => false,
            // }
        }
    }
}

pub struct TestValue<T>
{
    datatype: Type,
    value: T,
}

impl TestValue<String>
{
    fn new(data: &str) -> Self
    {
        Self
        {
            datatype: Type::Str,
            value: data.to_string()
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
    Type(Type),
  //  Struct(String, Vec<(Type, usize, Value)>),
    Callable(Type, Vec<Statement>, Vec<Statement>),
    NativeCallable(Type, Vec<(Type, Value)>, fn(Vec<Value>) -> Value),
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
            Value::Type(x)      => x.clone(),
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
            Value::Type(x)      => write!(f, "{:?}", x),
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
            Value::Boolean(b) => Value::Boolean(!*b),
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
