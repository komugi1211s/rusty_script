use trace::{code_line, err_fatal, SourceFile};

use syntax_ast::ast::*;

use super::expr::traverse_expression;
use super::{Compiler, Patch, PatchKind};
use crate::ir::IRCode;
use types::{Type, Value};

pub fn traverse_statement(compiler: &mut Compiler, ast: &ASTree, statement_id: StmtId) {
    let statement = ast.get_stmt(statement_id);

    use Statement::*;
    match statement {
        Expression(expr_id) => traverse_expression(compiler, ast, *expr_id),

        Print(expr_id) => {
            traverse_expression(compiler, ast, *expr_id);
            compiler.emit_op(IRCode::DebugPrint);
        }

        If(cond_expr, if_id, else_block) => {
            traverse_expression(compiler, ast, *cond_expr);
            let jnt_patch = compiler.reserve_one(); // Conditional Jump Position
            traverse_statement(compiler, ast, *if_id);

            if let Some(else_id) = else_block {
                let jump_patch = compiler.reserve_one();
                let start_of_else = compiler.codes.len();
                traverse_statement(compiler, ast, *else_id);
                let end_of_else = compiler.codes.len();

                compiler.patch(jnt_patch, IRCode::JNT(start_of_else as u32));
                compiler.patch(jump_patch, IRCode::Jump(end_of_else as u32));
            } else {
                let end_if = compiler.codes.len();
                compiler.patch(jnt_patch, IRCode::JNT(end_if as u32));
            }
        }

        While(expr_id, inner_stmt) => {
            let before_expr = compiler.codes.len();
            traverse_expression(compiler, ast, *expr_id);
            let jnt_patch = compiler.reserve_one();

            let captured = capture_patch(compiler, |enclosed| {
                traverse_statement(enclosed, ast, *inner_stmt);
                enclosed.emit_op(IRCode::Jump(before_expr as u32));

                let end_of_while = enclosed.codes.len();
                enclosed.patch(jnt_patch, IRCode::JNT(end_of_while as u32));
            });

            let end_of_while = compiler.codes.len();
            for patch in &captured {
                match patch.kind {
                    PatchKind::Generic => unreachable!(),
                    PatchKind::Break    => compiler.patch(*patch, IRCode::Jump(end_of_while as u32)),
                    PatchKind::Continue => compiler.patch(*patch, IRCode::Jump(before_expr as u32)),
                }
            }
        }

        Block(innerblock) => traverse_block(compiler, ast, innerblock),
        //        Function(func_idx)    => declare_function(compiler, ast, func_idx),
        Declaration(ref decl) => traverse_vardecl(compiler, ast, decl),

        Break => {
            compiler.mark_break();
        }
        Continue => {
            compiler.mark_continue();
        }
        _ => unimplemented!(),
    };
}

fn traverse_block(compiler: &mut Compiler, ast: &ASTree, inner: &BlockData) {
    compiler.deepen_nest();
    for (stmt_id) in &inner.statements {
        traverse_statement(compiler, ast, *stmt_id);
    }
    compiler.shallowen_nest();
}


fn traverse_vardecl(compiler: &mut Compiler, ast: &ASTree, decl: &DeclarationData) {
    let expr_type = if let Some(expr) = decl.expr {
        traverse_expression(compiler, ast, expr)
    } else {
    };

    let declaration_type = if decl.is_annotated() {
        parsedtype_to_type(&decl.dectype);
    } else {

    };
}

fn parsedtype_to_type(ptype: &ParsedType) -> Type {
    use ParsedType::*;
    match ptype {
        pInt     => Type::int(),
        pStr     => Type::string(),
        pFloat   => Type::float(),
        pBoolean => Type::boolean(),
        pArray(of, size) => {
            let type_of = Box::new(parsedtype_to_type(of));
            Type::array(type_of, *size)
        }
        pPointer(of) => {
            let pointer_of = Box::new(parsedtype_to_type(of));
            Type::ptr(pointer_of)
        }
        pOptional(of) => {
            let pointer_of = Box::new(parsedtype_to_type(of));
            Type::ptr(pointer_of)
        }
        pStruct(defs)  => unreachable!(),
        pUserdef(defs) => unreachable!(),
        pUnknown       => unreachable!(),
    }
}

use std::mem;
fn capture_patch(compiler: &mut Compiler, func: impl FnOnce(&mut Compiler)) -> Vec<Patch> {
    let mut old_vec = Vec::with_capacity(255);
    mem::swap(&mut compiler.patch, &mut old_vec);
    func(compiler);
    mem::swap(&mut old_vec, &mut compiler.patch);
    old_vec
}
