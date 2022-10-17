use crate::{
    ast::*,
    ir::IRCode,
    types::{ Value },
    trace::prelude::*
};

use super::{ Compiler };

pub fn traverse_expression(compiler: &mut Compiler, ast: &ASTree, expr: &Expression<'_>) -> KaiResult<()>
{
    use ExprKind::*;
    match expr.kind
    {
        Binary =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*lhs)?;
            traverse_expression(compiler, ast, &*rhs)?;
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Logical =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*lhs)?;
            traverse_expression(compiler, ast, &*rhs)?;
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Assign =>
        {
            let rhs = expr.rhs.as_ref().unwrap();
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*rhs)?;
            emit_store(compiler, &*lhs);
        }

        Unary =>
        {
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*rhs)?;
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Grouping => 
        {
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*lhs)?;
        }

        Literal => emit_constants(compiler, &expr.literal.as_ref().unwrap()),

        FunctionCall => emit_function_call(compiler, ast, &expr)?,
        Variable =>
        {
            let var_name = expr.variable_name.as_ref().unwrap();
            if let Some(idx) = expr.local_idx
            {
                compiler.emit_op(IRCode::Load(idx));
            }
            else
            {
                if let Some(global_symbol) = compiler.table.get(var_name)
                {
                    compiler.emit_op(IRCode::GLoad(global_symbol.idx as u32));
                }
                else
                {
                    unreachable!()
                }
            }
        }
        _ => {
            let msg = format!("{:?} は実装前です。", expr.kind);
            return expr.report("Unimplemented!", &msg);
        }
    };

    Ok(())
}

fn emit_function_call(compiler: &mut Compiler, ast: &ASTree, expr: &Expression<'_>) -> KaiResult<()>
{
    assert!(expr.lhs.is_some(), "Function Call: lhs is empty, we don't know what it called.");

    let variable = expect_opt!(expr.lhs.as_ref(), "関数の呼び出し対象が解決できていません。");
    let name = expect_opt!(variable.variable_name.as_ref(), "呼び出す関数の名前が分かりませんでした。");

    for arg_expr in expr.arg_expr.iter().rev()
    {
        traverse_expression(compiler, ast, arg_expr)?;
    }

    let (func_index, arg_length) = *expect_opt!(compiler.function_idx.get(name), "関数 {} の定義に失敗しました。", name);
    compiler.emit_op(IRCode::Call(func_index, arg_length));
    Ok(())
}

fn emit_store(compiler: &mut Compiler, target: &Expression<'_>)
{
    let var_name = target.variable_name.as_ref().unwrap();
    if let Some(local_idx) = target.local_idx
    {
        compiler.emit_op(IRCode::Store(local_idx));
    }
    else if let Some(symbol) = compiler.table.get(var_name)
    {
        compiler.emit_op(IRCode::GStore(symbol.idx as u32));
    }
    else
    {
        unreachable!();
    }
}


fn emit_from_oper(compiler: &mut Compiler, oper: Operator)
{
    match oper
    {
        Operator::Add => compiler.emit_op(IRCode::Add),
        Operator::Sub => compiler.emit_op(IRCode::Sub),
        Operator::Mul => compiler.emit_op(IRCode::Mul),
        Operator::Div => compiler.emit_op(IRCode::Div),
        Operator::Mod => compiler.emit_op(IRCode::Mod),

        Operator::EqEq => compiler.emit_op(IRCode::EqEq),
        Operator::NotEq => compiler.emit_op(IRCode::NotEq),
        Operator::LessEq => compiler.emit_op(IRCode::LessEq),
        Operator::MoreEq => compiler.emit_op(IRCode::MoreEq),
        Operator::Less => compiler.emit_op(IRCode::Less),
        Operator::More => compiler.emit_op(IRCode::More),

        Operator::Not => compiler.emit_op(IRCode::Not),
        Operator::Neg => compiler.emit_op(IRCode::Neg),

        Operator::And => compiler.emit_op(IRCode::And),
        Operator::Or => compiler.emit_op(IRCode::Or),
        _ => panic!(), 
    }
}

fn emit_constants(compiler: &mut Compiler, value: &Value)
{

    match value
    {
        Value::Str(_) =>
        {
            let index = compiler.add_const(value.clone());
            compiler.emit_op(IRCode::Const(index as u32));
        }
        Value::Boolean(given_b) =>
        {
            if *given_b {
                compiler.emit_op(IRCode::True);
            } else {
                compiler.emit_op(IRCode::False);
            }
        }
        Value::Int(intv) =>
        {
            let index = compiler.add_const(Value::Int(*intv));
            compiler.emit_op(IRCode::Const(index as u32));
        }
        Value::Float(f) =>
        {
            let index = compiler.add_const(Value::Float(*f));
            compiler.emit_op(IRCode::Const(index as u32));
        }

        Value::Null =>
            compiler.emit_op(IRCode::Null),
    }
}
