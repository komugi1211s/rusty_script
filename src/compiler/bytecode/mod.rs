
use trace::SourceFile;
use syntax_ast::ast::{ ASTree, AstNode, Statement, Expr, StmtId, ExprId, Literal, LiteralKind };

use super::ir::IRCode;
use super::context::{ CodeGen, BranchMode, ConditionalBranch, Context };
use super::typecheck::TypeArena;

pub struct CompiledCode {
    consts: Constants,
}

pub struct Constants {
}

impl Constants {
    fn new() -> Self {
        Self {}
    }
}

// TODO - @Improvement: since Constants moves from Env to
// CompiledCode at the end, I might want to box the entire
// Structure of this
pub struct Env {
    defn:   TypeArena,
    consts: Constants,
}

impl Env {
    fn new() -> Self {
        Env {
            defn: TypeArena::new(),
            consts: Constants::new(),
        }
    }
}

pub fn generate_bytecode(module: &SourceFile, ast: &ASTree) -> Result<CompiledCode, ()> {
    let mut ctx = Context::new();
    let mut env = Env::new();

    for node in ast.ast.iter() {
        traverse_statement(&mut env, &mut ctx, ast, node.stmt_id);
    }

    Ok(CompiledCode {})
}


fn traverse_statement(
    env: &mut Env,
    ctx: &mut impl CodeGen,
    ast: &ASTree,
    statement_id: StmtId,
) {
    let statement = ast.get_stmt(statement_id);

    match statement {
        Statement::Expression(expr_id) => traverse_expression(env, ctx, ast, *expr_id),
        _ => unimplemented!(),
    };
}

fn traverse_expression(
    env: &mut Env,
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
    env: &mut Env,
    ctx: &mut impl CodeGen,
    literal: &Literal
) {
    
}
