
use std::fmt;
use std::mem;
use std::ops;
use std::borrow::Borrow;

#[cfg(target_pointer_width = "32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width = "64")]
const USIZE_LENGTH: usize = 8;


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeKind<'ty> {
    Int,
    Float,
    Str,
    Boolean,
    Null,
    Array(&'ty Type<'ty>, ArraySize),
    UserDef,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArraySize {
    Sized(u32),
    Dynamic,
}

impl Default for TypeKind<'_> {
    fn default() -> Self {
        TypeKind::Null
    }
}

bitflags! {
    #[derive(Default)]
    pub struct TypeOption: u8 {
        const Func    = 0b0000_0001;
    }
}


// NOTE: since this struct's size is only about 16 bits ( 2 bytes! ),
// I just implement Copy + Clone and forget about it
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Type<'ty> {
    pub kind: TypeKind<'ty>,
    pub option: TypeOption,
}

impl<'ty> Type<'ty> {
    pub fn new(kind: TypeKind<'ty>, opt: TypeOption) -> Self {
        Self {
            kind,
            option: opt
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

    // TODO - @Cleanup: Obsolete
    pub fn is_compatible(self, v: &Value) -> bool {
        if v == &Value::Null {
            true
        } else if self.kind == TypeKind::Null {
            true
        } else {
            self.kind == v.to_type().kind
        }
    }

    pub fn primitive_from_str(cand: &str) -> Option<Self> {
        match cand {
            "int"    | "INT"    => Some(Self::int()),
            "float"  | "FLOAT"  => Some(Self::float()),
            "string" | "STRING" => Some(Self::string()),
            "bool"   | "BOOL"   => Some(Self::boolean()),
            _ => None
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Pointer(usize),
    //  Struct(String, Vec<(Type, usize, Value)>),
    Null,
}

impl From<&i64> for Type<'_> {
    fn from(_: &i64) -> Self {
        Type::int()
    }
}

impl From<&f64> for Type<'_> {
    fn from(_: &f64) -> Self {
        Type::float()
    }
}

impl From<&String> for Type<'_> {
    fn from(_: &String) -> Self {
        Type::string()
    }
}

impl From<&bool> for Type<'_> {
    fn from(_: &bool) -> Self {
        Type::boolean()
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
