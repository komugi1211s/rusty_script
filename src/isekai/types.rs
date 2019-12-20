
use super::parse::Statement;
use super::token::TokenType;
// use super::evaluate::{ Environment, Interpreter };
use std::fmt;
use std::mem;
use std::ops;
use std::borrow::Borrow;

#[cfg(target_pointer_width = "32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width = "64")]
const USIZE_LENGTH: usize = 8;

#[derive(Debug, Clone, Hash, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // Const       = 0b00010000,
    Const8 = 0b0001_0001,   // usize分の大きさのオペランドを取る
    Const16 = 0b0001_0010,  // usize分の大きさのオペランドを取る
    Const32 = 0b0001_0011,  // usize分の大きさのオペランドを取る
    Const64 = 0b0001_0100,  // usize分の大きさのオペランドを取る
    ConstDyn = 0b0001_0111, // usize分の大きさのオペランドを取る

    // Operational = 0b00100000,
    Return = 0b0010_0000,
    Push = 0b0010_0010,
    PushPtr = 0b0001_1000, // usize分の大きさのオペランドを取る
    Pop = 0b0010_0011,
    BlockIn = 0b0010_0110,
    BlockOut = 0b0010_0111,

    // Arithmitic = 0b00110000,
    Add = 0b0011_0001,
    Sub = 0b0011_0010,
    Mul = 0b0011_0011,
    Div = 0b0011_0100,
    Mod = 0b0011_0101,
    Not = 0b0011_0111,
    Neg = 0b0011_1000,

    // Logical     = 0b01000000,
    EqEq = 0b0100_0001,
    NotEq = 0b0100_0010,
    LessEq = 0b0100_0011,
    MoreEq = 0b0100_0100,
    Less = 0b0100_0101,
    More = 0b0100_0111,
    And = 0b0100_1000,
    Or = 0b0100_1001,

    // Branching   = 0b01010000,
    Jump = 0b0101_0000,
    JumpIfFalse = 0b0101_0010,
    Call = 0b0101_0011,

    // Globals = 0b01100000,
    GILoad = 0b0110_0001,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GIStore = 0b0110_0010, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFLoad = 0b0110_0011,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFStore = 0b0110_0100, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSLoad = 0b0110_0101,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSStore = 0b0110_0110, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBLoad = 0b0110_0111,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBStore = 0b0110_1000, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る

    // Locals      = 0b01110000,
    ILoad = 0b0111_0001,  // index(u16 - u8 + u8) の2つのオペランドを取る
    IStore = 0b0111_0010, // index(u16 - u8 + u8) の2つのオペランドを取る
    FLoad = 0b0111_0011,  // index(u16 - u8 + u8) の2つのオペランドを取る
    FStore = 0b0111_0100, // index(u16 - u8 + u8) の2つのオペランドを取る
    SLoad = 0b0111_0101,  // index(u16 - u8 + u8) の2つのオペランドを取る
    SStore = 0b0111_0110, // index(u16 - u8 + u8) の2つのオペランドを取る
    BLoad = 0b0111_0111,  // index(u16 - u8 + u8) の2つのオペランドを取る
    BStore = 0b0111_1000, // index(u16 - u8 + u8) の2つのオペランドを取る

    // System     = 0b11110000,
    Interrupt = 0b1111_1111,
    DebugPrint = 0b1111_0001,
}

pub trait toVmByte {
    fn to_vm_byte(&self) -> Vec<u8>;
    fn sufficient_opcode(&self) -> OpCode;
}

impl toVmByte for u16 {
    fn to_vm_byte(&self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn sufficient_opcode(&self) -> OpCode {
        OpCode::Const16
    }
}

impl toVmByte for i64 {
    fn to_vm_byte(&self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn sufficient_opcode(&self) -> OpCode {
        OpCode::Const64
    }
}

impl toVmByte for f64 {
    fn to_vm_byte(&self) -> Vec<u8> {
        self.to_bits().to_ne_bytes().to_vec()
    }

    fn sufficient_opcode(&self) -> OpCode {
        OpCode::Const64
    }
}

impl toVmByte for String {
    fn to_vm_byte(&self) -> Vec<u8> {
        self.clone().into_bytes()
    }

    fn sufficient_opcode(&self) -> OpCode {
        OpCode::ConstDyn
    }
}

impl toVmByte for bool {
    fn to_vm_byte(&self) -> Vec<u8> {
        vec![*self as u8]
    }
    fn sufficient_opcode(&self) -> OpCode {
        OpCode::Const8
    }
}

impl toVmByte for usize {
    fn to_vm_byte(&self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }

    fn sufficient_opcode(&self) -> OpCode {
        OpCode::Interrupt
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum TypeKind {
    Int     = 0b0000_0001,
    Float   = 0b0000_0010,
    Str     = 0b0000_0100,
    Boolean = 0b0000_1000,
    Null    = 0b0001_0000,
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Null
    }
}

bitflags! {
    #[derive(Default)]
    pub struct TypeOption: u8 {
        const Func    = 0b0000_0001;
        const Array   = 0b0000_0010;
    }
}

// NOTE: since this struct's size is only about 16 bits ( 2 bytes! ),
// I just implement Copy + Clone and forget about it
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub option: TypeOption,
}

impl Type {
    pub fn new(kind: TypeKind, opt: TypeOption) -> Self {
        Self {
            kind,
            option: opt
        }
    }

    pub fn from_tokentype(t: &TokenType) -> Self {
        match t {
            TokenType::TypeAny => Self::default(),
            TokenType::TypeBool => Self::boolean(),
            TokenType::TypeFloat => Self::float(),
            TokenType::TypeInt => Self::int(),
            TokenType::TypeStr => Self::string(),
            // This SHould not work.
            _ => Self::default(),
        }
    }

    pub fn boolean() -> Self {
        Self {
            kind: TypeKind::Boolean,
            option: TypeOption::default()
        }
    }

    pub fn float() -> Self {
        Self {
            kind: TypeKind::Float,
            option: TypeOption::default()
        }
    }

    pub fn int() -> Self {
        Self {
            kind: TypeKind::Int,
            option: TypeOption::default()
        }
    }

    pub fn string() -> Self {
        Self {
            kind: TypeKind::Str,
            option: TypeOption::default()
        }
    }

    pub fn is_compatible(self, v: &Value) -> bool {
        if v == &Value::Null {
            true
        } else if self.kind == TypeKind::Null {
            true
        } else {
            self.kind == v.to_type().kind
        }
    }

    pub fn type_after_binary(a: &Type, b: &Type, oper: &TokenType) -> Result<Type, ()> {
        let a = a.kind;
        let b = b.kind;
        match oper {
            TokenType::EqualEqual
            | TokenType::NotEqual
            | TokenType::More
            | TokenType::MoreEqual
            | TokenType::Less
            | TokenType::LessEqual => Ok(Type::boolean()),

            TokenType::Slash => Ok(Type::float()),
            TokenType::Percent => Ok(Type::int()),
            TokenType::Minus | TokenType::Asterisk => {
                if a == TypeKind::Float || b == TypeKind::Float {
                    Ok(Type::float())
                } else {
                    Ok(Type::int())
                }
            }
            TokenType::Plus => {
                if a == TypeKind::Str && b == TypeKind::Str {
                    Ok(Type::string())
                } else if a == TypeKind::Float || b == TypeKind::Float {
                    Ok(Type::float())
                } else {
                    Ok(Type::int())
                }
            }
            _ => Err(()),
        }
    }

    pub fn type_after_unary(a: &Type, oper: &TokenType) -> Result<Type, ()> {
        let a_type = a.kind;
        match oper {
            TokenType::Minus => {
                if a_type == TypeKind::Float || a_type == TypeKind::Int {
                    return Ok(a.clone());
                }
                Err(())
            }
            TokenType::Bang => Ok(Type::boolean()),
            _ => Err(()),
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
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Pointer(usize),
    Type(Type),
    //  Struct(String, Vec<(Type, usize, Value)>),
    Null,
}

impl From<&i64> for Type {
    fn from(_: &i64) -> Self {
        Type::int()
    }
}

impl From<&f64> for Type {
    fn from(_: &f64) -> Self {
        Type::float()
    }
}

impl From<&String> for Type {
    fn from(_: &String) -> Self {
        Type::string()
    }
}

impl From<&bool> for Type {
    fn from(_: &bool) -> Self {
        Type::boolean()
    }
}



// TODO @DumbCode - Isn't this Constant Redundant?
#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
    pub ctype: Type,
    pub value: Vec<u8>,
    pub code: OpCode,
}

impl Constant {
    pub fn null() -> Self {
        Self {
            ctype: Type::default(),
            value: vec![],
            code: OpCode::PushPtr,
        }
    }
}

impl<'a, T> From<&'a T> for Constant
where
    T: toVmByte,
    Type: From<&'a T>
{
    fn from(t: &'a T) -> Self {
        Self {
            ctype: t.into(),
            value: t.to_vm_byte(),
            code: t.sufficient_opcode(),
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}
impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Str(s)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl From<usize> for Value {
    fn from(u: usize) -> Self {
        Value::Pointer(u)
    }
}

impl Value {
    pub fn to_type(&self) -> Type {
        // Todo: Type Alias support
        match self {
            Value::Null => Type::default(),
            Value::Int(_) => Type::int(),
            Value::Type(x) => x.clone(),
            Value::Float(_) => Type::float(),
            Value::Str(_) => Type::string(),
            Value::Boolean(_) => Type::boolean(),
            _ => Type::default(),
        }
    }

    pub fn is_same_type(&self, other: &Value) -> bool {
        mem::discriminant(self) == mem::discriminant(&Value::Null)
            || mem::discriminant(self) == mem::discriminant(other)
    }

    pub fn is_truthy(&self) -> bool {
        // NOTE: This should work since Value implement ops::Not
        if let Value::Boolean(x) = !!self {
            x
        } else {
            panic!("Interpreter Internal Error");
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(f_) => write!(f, "{}", f_),
            Value::Str(ref s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Null => write!(f, "<null>"),
            Value::Type(x) => write!(f, "{:?}", x),
            Value::Pointer(x) => write!(f, "Pointer{}", x),
        }
    }
}

impl ops::Add<Value> for Value {
    type Output = Value;

    fn add(self, right: Value) -> Value {
        match self {
            Value::Int(il) => match right {
                Value::Int(ir) => Value::Int(il + ir),
                Value::Float(fr) => Value::Float(il as f64 + fr),
                x => unreachable!("TypeError: Can't add Int and {} together", x),
            },
            Value::Float(fl) => match right {
                Value::Int(ir) => Value::Float(fl + ir as f64),
                Value::Float(fr) => Value::Float(fl + fr),
                x => unreachable!("TypeError: Can't add Float and {} together", x),
            },
            Value::Str(ref sl) => {
                if let Value::Str(ref sr) = right {
                    Value::Str(format!("{}{}", sl, sr))
                } else {
                    unreachable!("TypeError: Can't add String and {} together", right)
                }
            }

            // Bool
            x => unreachable!("TypeError: {} does not support Addition", x),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    fn sub(self, right: Value) -> Value {
        match self {
            Value::Int(il) => match right {
                Value::Int(ir) => Value::Int(il - ir),
                Value::Float(fr) => Value::Float(il as f64 - fr),
                x => unreachable!("TypeError: Can't subtract {} from Int", x),
            },
            Value::Float(fl) => match right {
                Value::Int(ir) => Value::Float(fl - ir as f64),
                Value::Float(fr) => Value::Float(fl - fr),
                x => unreachable!("TypeError: Can't subtract {} from Float", x),
            },

            x => unreachable!("TypeError: {} does not support Subtraction", x),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = Value;

    fn mul(self, right: Value) -> Value {
        match self {
            Value::Int(il) => match right {
                Value::Int(ir) => Value::Int(il * ir),
                Value::Float(fr) => Value::Float(il as f64 * fr),
                x => unreachable!("TypeError: Can't multiply Int and {}", x),
            },
            Value::Float(fl) => match right {
                Value::Int(ir) => Value::Float(fl * ir as f64),
                Value::Float(fr) => Value::Float(fl * fr),
                x => unreachable!("TypeError: Can't multiply Float and {}", x),
            },

            // Bool & Str cannot be subtracted from others
            x => unreachable!("TypeError: {} does not support Multiplification", x),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = Value;

    fn div(self, right: Value) -> Value {
        match self {
            Value::Int(il) => match right {
                Value::Int(ir) => Value::Float(il as f64 / ir as f64),
                Value::Float(fr) => Value::Float(il as f64 / fr),
                x => unreachable!("TypeError: Cannot divide Int with {}", x),
            },
            Value::Float(fl) => match right {
                Value::Int(ir) => Value::Float(fl / ir as f64),
                Value::Float(fr) => Value::Float(fl / fr),
                x => unreachable!("TypeError: Cannot divide Float with {}", x),
            },

            // Bool & Str cannot be subtracted from others
            x => unreachable!("TypeError: {} does not support Division", x),
        }
    }
}

impl ops::Rem<Value> for Value {
    type Output = Value;
    fn rem(self, right: Value) -> Value {
        match self {
            Value::Int(il) => match right {
                Value::Int(ir) => Value::Float(il as f64 % ir as f64),
                Value::Float(fr) => Value::Float(il as f64 % fr),
                _ => panic!("Rem operation is not covered for Str / Bool"),
            },
            Value::Float(fl) => match right {
                Value::Int(ir) => Value::Float(fl % ir as f64),
                Value::Float(fr) => Value::Float(fl % fr),
                _ => panic!("Rem operation is not covered for Str / Bool"),
            },
            _ => panic!("Rem operation is not covered for Str / Bool"),
        }
    }
}

impl ops::Not for Value {
    type Output = Value;

    fn not(self) -> Value {
        match self {
            Value::Boolean(b) => Value::Boolean(!b),
            Value::Int(i) => Value::Boolean(i == 0),
            Value::Float(f) => Value::Boolean(f == 0.0),
            Value::Str(s) => Value::Boolean(s.is_empty()),
            Value::Null => Value::Boolean(true),
            _ => panic!("Unimplemented Not operation"),
        }
    }
}

// TODO:
// Cleanup: This feels weird.
impl ops::Not for &Value {
    type Output = Value;

    fn not(self) -> Value {
        match self {
            Value::Boolean(b) => Value::Boolean(!*b),
            Value::Int(i) => Value::Boolean(*i == 0),
            Value::Float(f) => Value::Boolean(*f == 0.0),
            Value::Str(s) => Value::Boolean(s.is_empty()),
            Value::Null => Value::Boolean(true),
            _ => panic!("Unimplemented Not operation"),
        }
    }
}

impl ops::Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Int(i) => Value::Int(-i),
            Value::Float(f) => Value::Float(-f),
            x => unreachable!("TypeError:{} does not support negation", x),
        }
    }
}

use std::cmp::Ordering;
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match self {
            Value::Int(i_left) => match other {
                Value::Int(i_right) => i_left.partial_cmp(i_right),
                Value::Float(f_right) => (*i_left as f64).partial_cmp(f_right),
                _ => unreachable!("Comparison with unsupported type"),
            },
            Value::Float(f_left) => match other {
                Value::Int(i_right) => f_left.partial_cmp(&(*i_right as f64)),
                Value::Float(f_right) => f_left.partial_cmp(f_right),
                _ => unreachable!("Comparison with unsupported type"),
            },
            _ => unreachable!("Comparison with unsupported type"),
        }
    }
}
