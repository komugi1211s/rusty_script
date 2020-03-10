
use typed_arena;
use syntax_ast::ast;
use types::Type;

struct Statement<'a> {
    scope: &'a Statement<'a>
}

struct Expression<'a> {
    parent: Option<&'a Expression<'a>>,

    vartype: Type,
    lhs: &'a Expression<'a>,
    rhs: &'a Expression<'a>,
    oper: ast::Operator,

    scope: &'a Statement<'a>,
}

pub fn analysis() {
}
