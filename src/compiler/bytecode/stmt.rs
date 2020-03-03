
use trace::{
        SourceFile,
        err_fatal,
        code_line,
};

use syntax_ast::ast::*;

use crate::ir::IRCode;
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
                    enclosed.emit_op(IRCode::Jump(before_expr as u32));
                
                    let end_of_while = enclosed.codes.len();
                    enclosed.patch(jnt_patch, IRCode::JNT(end_of_while as u32));
            });
            
            let end_of_while = ctx.codes.len();
                for patch in &captured {
                    match patch.kind {
                        PatchKind::Generic  => unreachable!(),
                        PatchKind::Break    => ctx.patch(*patch, IRCode::Jump(end_of_while as u32)),
                        PatchKind::Continue => ctx.patch(*patch, IRCode::Jump(before_expr  as u32)),
                }
            }
        }
        
        Block(innerblock)     => traverse_block(env, ctx, ast, innerblock),
            //        Function(func_idx)    => declare_function(env, ctx, ast, func_idx),
            Declaration(ref decl) => traverse_vardecl(env, ctx, decl),
        
            Break    => { ctx.mark_break(); },
            Continue => { ctx.mark_continue(); },
            _ => unimplemented!(),
    };
}

fn traverse_block(
env: &mut Env,
ctx: &mut Context,
ast: &ASTree,
inner: &BlockData
) {
        env.deepen_nest();
        for (stmt_id) in &inner.statements {
            traverse_statement(env, ctx, ast, *stmt_id);
    }
    env.shallowen_nest();
}

/*    
fn declare_function(
    env: &mut Env,
    ctx: &mut Context,
    ast: &ASTree,
    idx: usize
) {
    let func_decl = ast.functions.get(idx).expect("Undefined Function");
    declare_variable(env, ctx, &func_decl.it);
    
    env.deepen_nest();
    for arg in &func_decl.args {
        declare_variable(env, ctx, arg);
    }
    
    env.shallowen_nest();
}
 */

fn traverse_vardecl(
env:  &mut Env,
ctx:  &mut Context,
decl: &DeclarationData
) {
        // 1. solve variables type.
        // 2. notify an existence of variable.
        // 3. emit variable's expr if possible.
        // 4. emit store.
        unimplemented!();
}

use std::mem;
fn capture_patch(ctx: &mut Context, func: impl FnOnce(&mut Context)) -> Vec<Patch> {
        let mut old_vec = Vec::with_capacity(255);
        mem::swap(&mut ctx.patch, &mut old_vec);
        func(ctx);
        mem::swap(&mut old_vec, &mut ctx.patch);
        old_vec
}

