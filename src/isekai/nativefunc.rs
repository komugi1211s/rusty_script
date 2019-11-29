
use super::types::{ Type, Value };
use super::evaluate::{ Interpreter, Environment };
use std::time::{ SystemTime, UNIX_EPOCH };

fn clock(_: Vec<Value>) -> Value
{
    SystemTime::now().duration_since(UNIX_EPOCH)
        .unwrap().as_secs_f64().into()
}

fn istype(mut x: Vec<Value>) -> Value
{
    let value = x.pop().unwrap();
    let target_type = x.pop().unwrap();
    if value == Value::Null
    {
        return false.into();
    }

    if let Value::Type(_type) = target_type
    {
        _type.is_compatible(&value).into()
    }
    else
    {
        target_type.is_same_type(&value).into()
    }
}

fn cast(mut x: Vec<Value>) -> Value
{
    let current_value = x.pop().unwrap();
    let new_type = x.pop().unwrap();
    
    if let Value::Type(t) = new_type
    {
        match t {
            Type::Str => format!("{}", current_value).into(),
            Type::Float => match current_value {
                Value::Int(i) => (i as f64).into(),
                Value::Str(s) => s.parse::<f64>().unwrap().into(),
                Value::Float(x) => x.into(),
                Value::Null => 0.0f64.into(),
                _ => unreachable!(),
            },
            Type::Int => match current_value {
                Value::Int(i) => i.into(),
                Value::Str(s) => s.parse::<i64>().unwrap().into(),
                Value::Float(x) => (x.trunc() as i64).into(),
                Value::Null => 0.into(),
                _ => unreachable!(),
            }
            Type::Boolean => current_value.is_truthy().into(),
            Type::Any => current_value.clone(),
            _ => unreachable!(),
        }
    }
    else
    {
        unreachable!()
    }
}

pub fn define_native_functions(x: &mut Environment)
{
    x.define("clock", Type::Float, Value::NativeCallable(
            Type::Float,
            vec![],
            clock
    ));

    x.define("istype", Type::Boolean, Value::NativeCallable(
            Type::Boolean,
            vec![(Type::Type, Value::Null), (Type::Any, Value::Null)],
            istype
    ));

    x.define("cast", Type::Any, Value::NativeCallable(
            Type::Any,
            vec![(Type::Type, Value::Null), (Type::Any, Value::Null)],
            cast
    ));
}
