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
    // Const       = 0b00010000,
    Const8         = 0b00010001,  // usize分の大きさのオペランドを取る
    Const16        = 0b00010010,  // usize分の大きさのオペランドを取る
    Const32        = 0b00010011,  // usize分の大きさのオペランドを取る
    Const64        = 0b00010100,  // usize分の大きさのオペランドを取る
    ConstDyn       = 0b00010111,  // usize分の大きさのオペランドを取る

    // Operational = 0b00100000,
    Return         = 0b00100000,
    Push           = 0b00100010,
    PushPtr        = 0b00011000,  // usize分の大きさのオペランドを取る
    Pop            = 0b00100011,
    BlockIn        = 0b00100110, 
    BlockOut       = 0b00100111,

    // Arithmitic = 0b00110000,
    Add            = 0b00110001,
    Sub            = 0b00110010,
    Mul            = 0b00110011,
    Div            = 0b00110100,
    Mod            = 0b00110101,
    Not            = 0b00110111,
    Neg            = 0b00111000,

    // Logical     = 0b01000000,
    EqEq           = 0b01000001,
    NotEq          = 0b01000010,
    LessEq         = 0b01000011,
    MoreEq         = 0b01000100,
    Less           = 0b01000101,
    More           = 0b01000111,
    And            = 0b01001000,
    Or             = 0b01001001,

    // Branching   = 0b01010000,
    Jump           = 0b01010000,
    JumpIfFalse    = 0b01010010,
    Call           = 0b01010011,

    // Globals = 0b01100000,
    GILoad     = 0b01100001,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GIStore    = 0b01100010,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFLoad     = 0b01100011,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFStore    = 0b01100100,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSLoad     = 0b01100101,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSStore    = 0b01100110,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBLoad     = 0b01100111,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBStore    = 0b01101000,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    
    // Locals      = 0b01110000,
    ILoad          = 0b01110001, // index(u16 - u8 + u8) の2つのオペランドを取る
    IStore         = 0b01110010, // index(u16 - u8 + u8) の2つのオペランドを取る
    FLoad          = 0b01110011, // index(u16 - u8 + u8) の2つのオペランドを取る
    FStore         = 0b01110100, // index(u16 - u8 + u8) の2つのオペランドを取る
    SLoad          = 0b01110101, // index(u16 - u8 + u8) の2つのオペランドを取る
    SStore         = 0b01110110, // index(u16 - u8 + u8) の2つのオペランドを取る
    BLoad          = 0b01110111, // index(u16 - u8 + u8) の2つのオペランドを取る
    BStore         = 0b01111000, // index(u16 - u8 + u8) の2つのオペランドを取る

    // System     = 0b11110000,
    Interrupt     = 0b11111111,
    DebugPrint    = 0b11110001,
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
        const Any     = 0b01000000;
        const Func    = 0b00100000;
        const Int     = 0b00000001;
        const Float   = 0b00000010;
        const Str     = 0b00000100;
        const Boolean = 0b00001000;
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

    pub fn is_any(&self) -> bool
    {
        self.contains(Type::Any)
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
        }
    }

    pub fn type_after_binary(a: &Type, b: &Type, oper: &TokenType) -> Result<Type, ()>
    {
        let a = a.clone();
        let b = b.clone();
        match oper {
            TokenType::EqualEqual
            | TokenType::NotEqual
            | TokenType::More
            | TokenType::MoreEqual
            | TokenType::Less
            | TokenType::LessEqual => Ok(Type::Boolean),

            TokenType::Slash => Ok(Type::Float),
            TokenType::Percent => Ok(Type::Int),
            TokenType::Minus
            | TokenType::Asterisk => {
                if a == Type::Float || b == Type::Float
                {
                    Ok(Type::Float)
                }
                else
                {
                    Ok(Type::Int)
                }
            },
            TokenType::Plus => {
                if a == Type::Str && b == Type::Str
                {
                    Ok(Type::Str)
                }
                else {
                    if a == Type::Float || b == Type::Float
                    {
                        Ok(Type::Float)
                    }
                    else
                    {
                        Ok(Type::Int)
                    }
                }
            },
            _ => Err(())
        }
    }

    pub fn type_after_unary(a: &Type, oper: &TokenType) -> Result<Type, ()>
    {
        let a = a.clone();
        match oper {
            TokenType::Minus => {
                if a == Type::Float || a == Type::Int
                {
                    return Ok(a.clone());
                }
                Err(())
            },
            TokenType::Bang => Ok(Type::Boolean),
            _ => Err(())
        }
    }

}

/*
    TODO:
    ValueをここでEnumとして実装するより、trait Valueを作って各型に実装し、
    Box<dyn Value>でトレイトオブジェクトで取り回した方が良いのかな…と思った

    が、動的ディスパッチになるので可能なら避けたい
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Value
{
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Pointer(usize),
    Type(Type),
    //  Struct(String, Vec<(Type, usize, Value)>),

    // TODO: この辺はCallableDataとかのStructとして纏めたほうが楽かも
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

#[derive(Debug, Clone, PartialEq)]
pub struct Constant
{
    pub ctype: Type,
    pub value: Vec<u8>,
    pub code: OpCode,
}

impl Constant
{
    pub fn null() -> Self
    {
        Self
        {
            ctype: Type::Null,
            value: vec![],
            code: OpCode::PushPtr,
        }
    }
}

impl<T> From<T> for Constant
where T:
    toVmByte + Into<Type> + Clone
{
    fn from(t: T) -> Self
    {
        Self {
            ctype: t.clone().into(),
            value: t.to_vm_byte(),
            code: t.sufficient_opcode()
        }
    }
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

impl From<usize> for Value
{
    fn from(u: usize) -> Self
    { Value::Pointer(u) }
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
            Value::Pointer(x)   => write!(f, "Pointer{}", x),
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
