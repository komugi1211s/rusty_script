
use trace::SourceFile;
use syntax_ast::ast::{ ASTree, AstNode, Statement, Expr, StmtId, ExprId, Literal, LiteralKind };

use super::ir::IRCode;
use super::context::{ CodeGen, BranchMode, ConditionalBranch, Context };
use super::typecheck::TypeArena;

pub struct CompiledCode {
}

pub fn generate_bytecode(module: &SourceFile, ast: &ASTree) -> Result<CompiledCode, ()> {
    let mut ctx = Context::new();
    let mut defn = TypeArena::new();

    for node in ast.ast.iter() {
        traverse_statement(&mut ctx, &mut defn, ast, node.stmt_id);
    }

    Ok(CompiledCode{})
}


fn traverse_statement(
    ctx: &mut impl CodeGen,
    defn: &mut TypeArena,
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
