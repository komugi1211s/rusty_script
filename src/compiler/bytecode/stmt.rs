
use trace::{
    SourceFile,
    err_fatal,
    code_line,
};

use syntax_ast::ast::*;

use crate::ir::IRCode;
use crate::typecheck::{ TypeArena, TypeContext };
use types::{ Value, Type };
use super::{ Constants, Env, Context, PatchKind, Patch };
use super::expr::traverse_expression;

pub fn traverse_statement(
    env: &mut Env,
    ctx: &mut Context,
    ast: &ASTree,
    statement_id: StmtId,
) {
    let statement = ast.get_stmt(statement_id);

    use Statement::*;
    match statement {
        Expression(expr_id) => traverse_expression(env, ctx, ast, *expr_id),

        Print(expr_id)      => {
            traverse_expression(env, ctx, ast, *expr_id);
            ctx.emit_op(IRCode::DebugPrint);
        }
        
        If(cond_expr, if_id, else_block) => {
            traverse_expression(env, ctx, ast, *cond_expr);
            let jnt_patch = ctx.reserve_one(); // Conditional Jump Position
            traverse_statement(env, ctx, ast, *if_id);
           

            if let Some(else_id) = else_block {
                let jump_patch = ctx.reserve_one();
                let start_of_else = ctx.codes.len();
                traverse_statement(env, ctx, ast, *else_id);
                let end_of_else = ctx.codes.len();

                ctx.patch(jnt_patch,  IRCode::JNT(start_of_else as u32));
                ctx.patch(jump_patch, IRCode::Jump(end_of_else as u32));
            } else {
                let end_if = ctx.codes.len();
                ctx.patch(jnt_patch,  IRCode::JNT(end_if as u32));
            }
        }

        While(expr_id, inner_stmt) => {
            let before_expr = ctx.codes.len();
            traverse_expression(env, ctx, ast, *expr_id);
            let jnt_patch = ctx.reserve_one();
            let captured = capture_patch(ctx, |enclosed| {
                traverse_statement(env, enclosed, ast, *inner_stmt);
                emit_jump_to(enclosed, before_expr);
            
                let end_of_while = enclosed.codes.len();
                enclosed.patch(jnt_patch, IRCode::JNT(end_of_while as u32));
            });
            let end_of_while = enclosed.codes.len();
            for patch in &captured {
                match patch.kind {
                    PatchKind::Break => ctx.patch(*patch, IRCode::Jump(end_of_while as u32)),
                    PatchKind::Continue => ctx.patch(*patch, IRCode::Jump(before_expr as u32)),
                }
            }
        }

        // Block(innerblock) => traverse_block(env, ctx, ast, innerblock),

        Break    => { ctx.mark_break(); },
        Continue => { ctx.mark_continue(); },
        _ => unimplemented!(),
    };
}

use std::mem;
fn capture_patch(ctx: &mut Context, func: impl FnOnce(&mut Context)) -> Vec<Patch> {
    let mut old_vec = Vec::with_capacity(255);
    mem::swap(&mut ctx.patch, &mut old_vec);
    func(ctx);
    mem::swap(&mut old_vec, &mut ctx.patch);
    old_vec
}
