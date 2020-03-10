use trace::{code_line, err_fatal, SourceFile};

use syntax_ast::ast::*;

use super::{Compiler};
use crate::ir::IRCode;
use types::{Type, Value};

pub fn traverse_expression(compiler: &mut Compiler, ast: &ASTree, expression_id: ExprId) {
    let expression = ast.get_expr(expression_id);

    use Expr::*;
    match expression {
        Binary(lhs, rhs, oper) => {
            traverse_expression(compiler, ast, *lhs);
            traverse_expression(compiler, ast, *rhs);
            emit_from_oper(compiler, *oper);
        }

        Logical(lhs, rhs, oper) => {
            traverse_expression(compiler, ast, *lhs);
            traverse_expression(compiler, ast, *rhs);
            emit_from_oper(compiler, *oper);
        }

        Assign(idenid, rhs) => {
            traverse_expression(compiler, ast, *rhs);
        }

        Unary(rhs, oper) => {
            traverse_expression(compiler, ast, *rhs);
            emit_from_oper(compiler, *oper);
        }

        Literal(ref lit_data) => emit_constants(compiler, lit_data, ast),
        _ => unimplemented!(),
    };
}

fn emit_from_oper(compiler: &mut Compiler, oper: Operator) {
    match oper {
        Operator::Add => compiler.emit_op(IRCode::Add),
        Operator::Sub => compiler.emit_op(IRCode::Sub),
        Operator::Mul => compiler.emit_op(IRCode::Mul),
        Operator::Div => compiler.emit_op(IRCode::Div),
        Operator::Mod => compiler.emit_op(IRCode::Mod),
        _ => (),
    }
}

fn emit_constants(compiler: &mut Compiler, literal: &Literal, ast: &ASTree) {
    let lexeme: &str = literal
        .tok
        .lexeme
        .as_ref()
        .expect("リテラルなのにトークンが空");

    match literal.kind {
        LiteralKind::Bool => {
            match lexeme {
                // TODO - @DumbCode: Hardcoded Index.
                "true"  => compiler.emit_op(IRCode::True),
                "false" => compiler.emit_op(IRCode::False),
                _ => unreachable!(),
            };
        }

        LiteralKind::Int => {
            let value: i64 = match lexeme.parse() {
                Ok(n) => n,
                Err(_) => {
                    err_fatal!(
                    src: ast.file,
                    span: literal.tok.span,
                    title: "Broken Integer Literal",
                    msg: "整数を期待しましたがパースに失敗しました。"
                    );

                    code_line!(src: ast.file, span: literal.tok.span, pad: 2);
                    return;
                }
            };

            let index = compiler.add_const(Value::Int(value));
            compiler.emit_op(IRCode::Const64(index as u32));
        }

        LiteralKind::Float => {
            let value: f64 = match lexeme.parse() {
                Ok(n) => n,
                Err(_) => {
                    err_fatal!(
                    src: ast.file,
                    span: literal.tok.span,
                    title: "Broken Float Literal",
                    msg: "実数を期待しましたがパースに失敗しました。"
                    );

                    code_line!(src: ast.file, span: literal.tok.span, pad: 2);
                    return;
                }
            };

            let index = compiler.add_const(Value::Float(value));
            compiler.emit_op(IRCode::Const64(index as u32));
        }

        LiteralKind::Str => {
            let index = compiler.add_const(Value::Str(lexeme.to_string()));
            compiler.emit_op(IRCode::ConstDyn(index as u32));
        }
        _ => unimplemented!(),
    }
}
