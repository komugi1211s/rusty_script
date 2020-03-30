use crate::{
    ast::*,
    ir::IRCode,
    types::{ Value },
    trace::prelude::*
};

use super::{ Compiler };

pub fn traverse_expression(compiler: &mut Compiler, ast: &ASTree, expr: &Expression<'_>)
{
    use ExprKind::*;
    match expr.kind
    {
        Binary =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*lhs);
            traverse_expression(compiler, ast, &*rhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Logical =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*lhs);
            traverse_expression(compiler, ast, &*rhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Assign =>
        {
            let rhs = expr.rhs.as_ref().unwrap();
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*rhs);
            emit_store(compiler, &*lhs);
        }

        Unary =>
        {
            let rhs = expr.rhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*rhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Grouping => 
        {
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, ast, &*lhs);
        }

        Literal => emit_constants(compiler, &expr.literal.as_ref().unwrap()),

        FunctionCall => emit_function_call(compiler, ast, &expr),
        Variable =>
        {
            let var_name = expr.variable_name.as_ref().unwrap();
            if let Some(idx) = expr.local_idx
            {
                compiler.emit_op(IRCode::Load(idx));
            }
            else
            {
                if let Some(global_symbol) = compiler.table.symbol.get(var_name)
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
            expr.report("Unimplemented!", &msg);
            panic!();
        }
    };
}

fn emit_function_call(compiler: &mut Compiler, ast: &ASTree, expr: &Expression<'_>)
{
    // at this point, we know that:
    // Function exists, and properly declared as a function.
    // Function will return an intended type.
    // Receiver variable(if exists) and function holds the same type.
    // argument has the same type.
    
    assert!(expr.lhs.is_some(), "Function Call: lhs is empty, we don't know what it called.");
    
    let variable = expect_opt!(expr.lhs.as_ref(), "関数の呼び出し対象が解決できていません。");
    let name = expect_opt!(variable.variable_name.as_ref(), "呼び出す関数の名前が分かりませんでした。");

    for arg_expr in expr.arg_expr.iter().rev()
    {
        traverse_expression(compiler, ast, arg_expr);
    }

    let index = *expect_opt!(compiler.function_idx.get(name), "関数 {} の定義に失敗しました。", name);
    compiler.emit_op(IRCode::Call(index));
}

fn emit_store(compiler: &mut Compiler, target: &Expression<'_>) 
{
    let var_name = target.variable_name.as_ref().unwrap();
    if let Some(local_idx) = target.local_idx 
    {
        compiler.emit_op(IRCode::Store(local_idx));
    }
    else if let Some(symbol) = compiler.table.symbol.get(var_name)
    {
        compiler.emit_op(IRCode::GStore(symbol.idx as u32));
    }
    else
    {
        let message = format!("変数 {} を探しましたが、見つかりませんでした。\nこの変数はセマンティクス解析の時点で発見されているべきです。コンパイラーのエラーです。", var_name);
        target.report("internal", &message);
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
