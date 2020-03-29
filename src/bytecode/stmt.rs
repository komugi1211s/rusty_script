use std::mem;
#[allow(unused_imports)]
use crate::{
    trace::prelude::*,
    ast::*,
    ir::IRCode,
};

use super::expr::traverse_expression;
use super::{Compiler, PatchKind};

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

            let jnt_position = compiler.reserve_one();
            traverse_statement(compiler, ast, *if_id)?;

            if let Some(else_id) = else_block
            {
                let jump_position = compiler.reserve_one();

                let start_of_else = compiler.codes.len();
                traverse_statement(compiler, ast, *else_id)?;
                let end_of_else = compiler.codes.len();

                compiler.patch(jnt_position, IRCode::JNT(start_of_else as u32));
                compiler.patch(jump_position, IRCode::Jump(end_of_else as u32));
            }
            else
            {
                let end_if = compiler.codes.len();
                compiler.patch(jnt_position, IRCode::JNT(end_if as u32));
            }
            Ok(())
        }

        While(expr_id, inner_stmt) =>
        {
            let expr = ast.get_expr(*expr_id);

            let conditional_expr = compiler.codes.len();
            traverse_expression(compiler, expr);

            let jnt_position = compiler.reserve_one();
            let mut reserve_patches = Vec::with_capacity(255);
            mem::swap(&mut compiler.patch, &mut reserve_patches);

            traverse_statement(compiler, ast, *inner_stmt)?;
            compiler.emit_op(IRCode::Jump(conditional_expr as u32));

            let end_of_while = compiler.codes.len();
            compiler.patch(jnt_position, IRCode::JNT(end_of_while as u32));

            mem::swap(&mut compiler.patch, &mut reserve_patches);
            for patch in reserve_patches
            {
                match patch.kind
                {
                    PatchKind::Generic => unreachable!(),
                    PatchKind::Break => compiler.patch(patch, IRCode::Jump(end_of_while as u32)),
                    PatchKind::Continue => compiler.patch(patch, IRCode::Jump(conditional_expr as u32)),
                }
            }
            Ok(())
        }

        Block(innerblock) => traverse_block(compiler, ast, innerblock),

        // Function(func_idx) => declare_function(compiler, ast, func_idx),
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
        Function(_) => 
        {
            Ok(())
        }
        _ =>
        {
            statement.report("Unimplemented", "実装前です");
            Err(())
        }
    }
}

fn traverse_block(compiler: &mut Compiler, ast: &ASTree, inner: &BlockData) -> Result<(), ()>
{
    compiler.deepen_nest();
    for stmt_id in &inner.statements
    {
        traverse_statement(compiler, ast, *stmt_id)?;
    }
    compiler.shallowen_nest();
    Ok(())
}

fn traverse_vardecl(compiler: &mut Compiler, ast: &ASTree, decl: &DeclarationData)
{
    if let Some(expr) = decl.expr
    {
        let expr = ast.get_expr(expr);
        traverse_expression(compiler, expr);
    }
    if let Some(symbol) = compiler.table.symbol.get(&decl.name)
    {
        compiler.emit_op(IRCode::GStore(symbol.idx as u32));
    }
}

