use crate::{
    ast::*,
    trace::prelude::*,
    ir::IRCode,
    types::{Type, Value},
};

use super::Compiler;

pub fn traverse_expression(compiler: &mut Compiler, expr: &Expression<'_>)
{
    use ExprKind::*;
    match expr.kind
    {
        Binary =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, &*lhs);
            traverse_expression(compiler, &*rhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Logical =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, &*lhs);
            traverse_expression(compiler, &*rhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Assign =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, &*rhs);
            emit_store(compiler, &*lhs);
            panic!();
        }

        Unary =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, &*lhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Grouping => 
        {
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, &*lhs);
        }

        Literal => emit_constants(compiler, &expr.literal.as_ref().unwrap()),

        FunctionCall => emit_function_call(compiler, &expr),
        _ =>
        {
            let msg = format!("{:?} は実装前です。", expr.kind);
            expr.report("Unimplemented!", &msg);
            panic!();
        }
    };
}

fn emit_function_call(compiler: &mut Compiler, expr: &Expression<'_>)
{
    // at this point, we know that:
    // Function exists, and properly declared as a function.
    // Function will return an intended type.
    // Receiver variable(if exists) and function holds the same type.
    // argument has the same type.
    // Functions are already emitted, we know which index we should jump.
    assert!(expr.lhs.is_some(), "Function Call: lhs is empty, we don't know what it called.");
    assert!(expr.lhs.as_ref().unwrap().kind == ExprKind::Variable,
            "Function Call: Called something that's not a Variable.");
    
    let func_name = expr.lhs.as_ref().unwrap().variable_name.as_ref().unwrap().clone();
    // let func_index = compiler.get_func_ep(func_name);
    
    for arg_expr in expr.arg_expr.iter().rev()
    {
        traverse_expression(compiler, arg_expr);
    }

    compiler.emit_op(IRCode::Interrupt);
}

fn emit_store(compiler: &mut Compiler, target: &Expression<'_>) 
{
    let var_name = target.variable_name.as_ref().unwrap();
    if let Some(index) = compiler.search_local(var_name)
    {
        compiler.emit_op(IRCode::Store(index as u32));
    }
    else
    {
        let message = format!("変数 {} を探しましたが、見つかりませんでした。", var_name);
        target.report("Undefined Local Variable", &message);
        panic!()
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
        _ => (),
    }
}

fn emit_constants(compiler: &mut Compiler, literal: &Literal)
{
    let lexeme: &str = match literal.tok.lexeme.as_ref()
    {
        Some(x) => x,
        None =>
        {
            literal.tok.report("internal", "リテラルなのにトークンが空");
            panic!();
        }
    };

    match literal.kind
    {
        LiteralKind::Bool =>
        {
            match lexeme
            {
                // TODO - @DumbCode: Hardcoded Index.
                "true" => compiler.emit_op(IRCode::True),
                "false" => compiler.emit_op(IRCode::False),
                _ => unreachable!(),
            };
        }

        LiteralKind::Int =>
        {
            let value: i64 = match lexeme.parse()
            {
                Ok(n) => n,
                Err(_) =>
                {
                    literal.tok.report(
                        "Broken Integer Literal",
                        "整数を期待しましたがパースに失敗しました。",
                    );

                    return;
                }
            };

            let index = compiler.add_const(Value::Int(value));
            compiler.emit_op(IRCode::Const64(index as u32));
        }

        LiteralKind::Float =>
        {
            let value: f64 = match lexeme.parse()
            {
                Ok(n) => n,
                Err(_) =>
                {
                    literal.tok.report(
                        "Broken Float Literal",
                        "実数を期待しましたがパースに失敗しました。",
                    );
                    return;
                }
            };

            let index = compiler.add_const(Value::Float(value));
            compiler.emit_op(IRCode::Const64(index as u32));
        }

        LiteralKind::Str =>
        {
            let index = compiler.add_const(Value::Str(lexeme.to_string()));
            compiler.emit_op(IRCode::ConstDyn(index as u32));
        }
        _ => unimplemented!(),
    }
}
