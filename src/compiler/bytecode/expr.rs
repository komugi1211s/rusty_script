use syntax_ast::ast::*;
use trace::prelude::*;

use super::Compiler;
use crate::ir::IRCode;
use types::{Type, Value};

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
            expr.report("Unimplemented!", "代入は実装前です。");
            panic!();
        }

        Unary =>
        {
            let lhs = expr.lhs.as_ref().unwrap();
            traverse_expression(compiler, &*lhs);
            emit_from_oper(compiler, expr.oper.unwrap());
        }

        Literal => emit_constants(compiler, &expr.literal.as_ref().unwrap()),
        _ =>
        {
            let msg = format!("{:?} は実装前です。", expr.kind);
            expr.report("Unimplemented!", &msg);
            panic!();
        }
    };
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
