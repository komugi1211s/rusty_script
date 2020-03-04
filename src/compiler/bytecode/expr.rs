use trace::{code_line, err_fatal, SourceFile};

use syntax_ast::ast::*;

use super::{Context, Env};
use crate::ir::IRCode;
use types::{Type, Value};

pub fn traverse_expression(env: &mut Env, ctx: &mut Context, ast: &ASTree, expression_id: ExprId) {
    let expression = ast.get_expr(expression_id);

    use Expr::*;
    match expression {
        Binary(lhs, rhs, oper) => {
            traverse_expression(env, ctx, ast, *lhs);
            traverse_expression(env, ctx, ast, *rhs);
            emit_from_oper(ctx, *oper);
        }

        Logical(lhs, rhs, oper) => {
            traverse_expression(env, ctx, ast, *lhs);
            traverse_expression(env, ctx, ast, *rhs);
            emit_from_oper(ctx, *oper);
        }

        Assign(idenid, rhs) => {
            traverse_expression(env, ctx, ast, *rhs);
        }

        Unary(rhs, oper) => {
            traverse_expression(env, ctx, ast, *rhs);
            emit_from_oper(ctx, *oper);
        }

        Literal(ref lit_data) => emit_constants(env, ctx, lit_data),
        _ => unimplemented!(),
    };
}

fn emit_from_oper(ctx: &mut Context, oper: Operator) {
    match oper {
        Operator::Add => ctx.emit_op(IRCode::Add),
        Operator::Sub => ctx.emit_op(IRCode::Sub),
        Operator::Mul => ctx.emit_op(IRCode::Mul),
        Operator::Div => ctx.emit_op(IRCode::Div),
        Operator::Mod => ctx.emit_op(IRCode::Mod),
        _ => (),
    }
}

fn emit_constants(env: &mut Env, ctx: &mut Context, literal: &Literal) {
    let lexeme: &str = literal
        .tok
        .lexeme
        .as_ref()
        .expect("リテラルなのにトークンが空");

    match literal.kind {
        LiteralKind::Bool => {
            match lexeme {
                // TODO - @DumbCode: Hardcoded Index.
                "true"  => ctx.emit_op(IRCode::True),
                "false" => ctx.emit_op(IRCode::False),
                _ => unreachable!(),
            };
        }

        LiteralKind::Int => {
            let value: i64 = match lexeme.parse() {
                Ok(n) => n,
                Err(_) => {
                    err_fatal!(
                    src: env.source,
                    span: literal.tok.span,
                    title: "Broken Integer Literal",
                    msg: "整数を期待しましたがパースに失敗しました。"
                    );

                    code_line!(src: env.source, span: literal.tok.span, pad: 2);
                    return;
                }
            };

            let index = env.consts.add_const(Value::Int(value));
            ctx.emit_op(IRCode::Const64(index as u32));
        }

        LiteralKind::Float => {
            let value: f64 = match lexeme.parse() {
                Ok(n) => n,
                Err(_) => {
                    err_fatal!(
                    src: env.source,
                    span: literal.tok.span,
                    title: "Broken Float Literal",
                    msg: "実数を期待しましたがパースに失敗しました。"
                    );

                    code_line!(src: env.source, span: literal.tok.span, pad: 2);
                    return;
                }
            };

            let index = env.consts.add_const(Value::Float(value));
            ctx.emit_op(IRCode::Const64(index as u32));
        }

        LiteralKind::Str => {
            let index = env.consts.add_const(Value::Str(lexeme.to_string()));
            ctx.emit_op(IRCode::ConstDyn(index as u32));
        }
        _ => unimplemented!(),
    }
}
