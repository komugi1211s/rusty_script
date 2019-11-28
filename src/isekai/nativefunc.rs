
use super::types::{ Type, Value };
use super::evaluate::{ Interpreter, Environment };
use std::time::{ SystemTime, UNIX_EPOCH };

fn clock(_: Vec<Value>) -> Value
{
    SystemTime::now().duration_since(UNIX_EPOCH)
        .unwrap().as_secs_f64().into()
}

pub fn define_native_functions(x: &mut Environment)
{
    x.define("clock", Type::Float, Value::NativeCallable(
            Type::Float,
            0,
            clock
    ));
}
