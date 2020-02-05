
use trace::SourceFile;
use syntax_ast::ast::{ ASTree, Statement, Expr, StmtId, ExprId, Literal, LiteralKind };

use super::ir::IRCode;
use super::context::{ CodeGen, BranchMode, ConditionalBranch, Context };

pub struct CompiledCode {
}

struct GeneratorState {
}

pub fn generate_bytecode(module: &SourceFile, ast: &ASTree) -> Result<CompiledCode, ()> {
    Ok(CompiledCode{})
}

fn traverse_statement(
    ctx: &mut impl CodeGen,
    ast: &ASTree,
    statement_id: StmtId,
) {
    let statement = ast.get_stmt(statement_id);

    match statement {
        Statement::Expression(expr_id) => traverse_expression(ctx, ast, *expr_id),
        _ => unimplemented!(),
    };
}

fn traverse_expression(
    ctx: &mut impl CodeGen,
    ast: &ASTree,
    expression_id: ExprId,
) {
    let expression = ast.get_expr(expression_id);

    match expression {
        Expr::Literal(ref lit_data) => emit_literal(ctx, lit_data),
        _ => unimplemented!(),
    };
}

fn emit_literal(
    ctx: &mut impl CodeGen,
    literal: &Literal
) {
}
