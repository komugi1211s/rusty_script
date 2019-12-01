
use super::token::{ TokenType };
use super::parse::{ Statement };
use super::evaluate::{ Environment, Interpreter };
use std::ops;
use std::fmt;
use std::mem;


#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum OpCode
{
    Return     = 0x00,
    Const8     = 0x01,
    Const16    = 0x02,
    Const32    = 0x03,
    Const64    = 0x04,
    ConstDyn   = 0x05,
    Define     = 0x06,
    Push       = 0x07,
    Pop        = 0x08,

    Add        = 0x10,
    Sub        = 0x11,
    Mul        = 0x12,
    Div        = 0x13,
    Mod        = 0x14,
    Not        = 0x15,
    Neg        = 0x16,

    EqEq       = 0x20,
    NotEq      = 0x21,
    LessEq     = 0x22,
    MoreEq     = 0x23,
    Less       = 0x24,
    More       = 0x25,
    And        = 0x26,
    Or         = 0x27,

    Interrupt  = 0xCC,
    DebugPrint = 0xCF,
}

impl From<u8> for OpCode
{
    /*
        NOTE:
        CライクなEnumを使って u8 <-> OpCode ができるかと思ったけど
        u8 -> OpCode はともかく、その逆が不可能らしい

        一応 mem::transmute (unsafe) を使えばできるらしいが、Enumの範囲外にあるu8を与えた場合に
        何が起こるか全くわからない
        しかもその状況が起こらないという保証がないので危険

        なので現状このまま全マッチングアームを作る感じで行く
        必ずCoreかどこかで毎回テスト用の関数を実行すること
    */
    fn from(item: u8) -> Self
    {
        match item
        {
            0x00 => Self::Return,
            0x01 => Self::Const8,
            0x02 => Self::Const16,
            0x03 => Self::Const32,
            0x04 => Self::Const64,
            0x05 => Self::ConstDyn,
            0x06 => Self::Define,
            0x07 => Self::Push,
            0x08 => Self::Pop,

            0x10 => Self::Add,
            0x11 => Self::Sub,
            0x12 => Self::Mul,
            0x13 => Self::Div,
            0x14 => Self::Mod,
            0x15 => Self::Not,
            0x16 => Self::Neg,

            0x20 => Self::EqEq,
            0x21 => Self::NotEq,
            0x22 => Self::LessEq,
            0x23 => Self::MoreEq,
            0x24 => Self::Less,
            0x25 => Self::More,
            0x26 => Self::And,
            0x27 => Self::Or,

            0xCF => Self::DebugPrint,
            _    => Self::Interrupt,
        }
    }
}

pub fn test_opcode_u8()
{
    /* u8 <-> opcode の相互マップが正しいことを確認する */
    for i in 0..255u8
    {
        let opcode = OpCode::from(i);
        if opcode == OpCode::Interrupt { continue; }
        let bytevalue: u8 = opcode.into();
        assert_eq!(i, bytevalue, "Expected {} == {}, found {} == {}({:?})", 
                i,
                i,
                i,
                bytevalue,
                OpCode::from(i)
            );
    }
}

impl From<OpCode> for u8
{
    fn from(item: OpCode) -> u8
    {
        item as u8
    }
}

pub trait toVmByte
{
    fn to_vm_byte(&self) -> Vec<u8>;
    fn sufficient_opcode(&self) -> OpCode;
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

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Type
{
    Int,
    Float,
    Str,
    Boolean,
//     Struct,
    Type,
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
                Type::Type    => if let Value::Type(_) = v { true } else { false },
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
