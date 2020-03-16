use trace::prelude::*;

use syntax_ast::ast::*;

use super::expr::traverse_expression;
use super::{Compiler, Patch, PatchKind};
use crate::ir::IRCode;
use types::{Type, Value};

pub fn traverse_statement(
    compiler: &mut Compiler,
    ast: &ASTree,
    statement_id: StmtId,
) -> Result<(), ()>
{
    let statement = ast.get_stmt(statement_id);

    use Stmt::*;
    match &statement.data
    {
        Expression(expr_id) =>
        {
            let expr = ast.get_expr(*expr_id);
            traverse_expression(compiler, expr);
            Ok(())
        }

        Print(expr_id) =>
        {
            let expr = ast.get_expr(*expr_id);
            traverse_expression(compiler, expr);
            compiler.emit_op(IRCode::DebugPrint);
            Ok(())
        }

        If(cond_expr, if_id, else_block) =>
        {
            let expr = ast.get_expr(*cond_expr);
            traverse_expression(compiler, expr);

            let jnt_patch = compiler.reserve_one(); // Conditional Jump Position
            traverse_statement(compiler, ast, *if_id);

            if let Some(else_id) = else_block
            {
                let jump_patch = compiler.reserve_one();
                let start_of_else = compiler.codes.len();
                traverse_statement(compiler, ast, *else_id);
                let end_of_else = compiler.codes.len();

                compiler.patch(jnt_patch, IRCode::JNT(start_of_else as u32));
                compiler.patch(jump_patch, IRCode::Jump(end_of_else as u32));
            }
            else
            {
                let end_if = compiler.codes.len();
                compiler.patch(jnt_patch, IRCode::JNT(end_if as u32));
            }
            Ok(())
        }

        While(expr_id, inner_stmt) =>
        {
            let before_expr = compiler.codes.len();
            let expr = ast.get_expr(*expr_id);
            traverse_expression(compiler, expr);
            let jnt_patch = compiler.reserve_one();

            let captured = capture_patch(compiler, |enclosed| {
                traverse_statement(enclosed, ast, *inner_stmt);
                enclosed.emit_op(IRCode::Jump(before_expr as u32));

                let end_of_while = enclosed.codes.len();
                enclosed.patch(jnt_patch, IRCode::JNT(end_of_while as u32));
            });

            let end_of_while = compiler.codes.len();
            for patch in &captured
            {
                match patch.kind
                {
                    PatchKind::Generic => unreachable!(),
                    PatchKind::Break => compiler.patch(*patch, IRCode::Jump(end_of_while as u32)),
                    PatchKind::Continue => compiler.patch(*patch, IRCode::Jump(before_expr as u32)),
                }
            }
            Ok(())
        }

        Block(innerblock) =>
        {
            traverse_block(compiler, ast, innerblock);
            Ok(())
        }
        //        Function(func_idx)    => declare_function(compiler, ast, func_idx),
        Declaration(ref decl) => Ok(traverse_vardecl(compiler, ast, decl)),

        Break =>
        {
            compiler.mark_break();
            Ok(())
        }
        Continue =>
        {
            compiler.mark_continue();
            Ok(())
        }
        _ =>
        {
            statement.report("Unimplemented", "実装前です");
            Err(())
        }
    }
}

fn traverse_block(compiler: &mut Compiler, ast: &ASTree, inner: &BlockData)
{
    compiler.deepen_nest();
    for (stmt_id) in &inner.statements
    {
        traverse_statement(compiler, ast, *stmt_id);
    }
    compiler.shallowen_nest();
}

fn traverse_vardecl(compiler: &mut Compiler, ast: &ASTree, decl: &DeclarationData)
{
    let expr_type = if let Some(expr) = decl.expr
    {
        let expr = ast.get_expr(expr);
        traverse_expression(compiler, expr)
    }
    else
    {
    };

    let declaration_type = if decl.is_annotated()
    {
    }
    else
    {
    };
}

use std::mem;
fn capture_patch(compiler: &mut Compiler, func: impl FnOnce(&mut Compiler)) -> Vec<Patch>
{
    let mut old_vec = Vec::with_capacity(255);
    mem::swap(&mut compiler.patch, &mut old_vec);
    func(compiler);
    mem::swap(&mut old_vec, &mut compiler.patch);
    old_vec
}
