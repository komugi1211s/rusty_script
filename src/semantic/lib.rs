
use syntax_ast::ast::*;
use types::Type;

struct SemanticTable {
}

pub fn analysis(ast: &mut ASTree<'_>) {
    for expr in ast.expr.iter_mut() {
        solve_type(expr);
    }
}

fn solve_type(expr: &mut Expression<'_>) {

}
