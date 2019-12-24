
use types::{ Type, TypeKind, TypeOption };
use syntax_ast::ast_data::*;


fn literal_to_type(lit: &Literal) -> Type {
    match lit.kind {
        LiteralKind::Int => Type::int(),
        LiteralKind::Float => Type::float(),
        LiteralKind::Str => Type::string(),
        LiteralKind::Bool => Type::boolean(),
        LiteralKind::Null => Type::default(),
    }
}

fn check_binary_op(lhs: &Expr, rhs: &Expr, oper: Operator) -> Result<Type, ()> {
    // "and" と "or" は確定でbool
    let lhs_type = match lhs {
        Expr::Literal(ref lit ) => literal_to_type(lit),
        Expr::Binary(lhe, rhe, op) => check_binary_op(&**lhe, &**rhe, *op)
    }
    Err(())
}


fn check_arithmetic_binary_op(lhs: &Literal, rhs: &Literal, oper: Operator) -> Result<Type, ()> {
}


fn check_unary_op(lhs: &Literal, oper: Operator) -> Type {
}


fn lookup_elevation(lhs: Type, rhs: Type, oper: Operator) -> Option<Type> {
}


