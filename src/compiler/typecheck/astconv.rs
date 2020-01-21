use syntax_ast::ast::*;
use types::Type;

use super::{TypeArena, TypeContext};

/*
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprId, ExprId, Operator),
    Logical(ExprId, ExprId, Operator),
    FunctionCall(ExprId, Vec<ExprId>),
    Assign(ExprId, ExprId),

    Literal(Literal),
    Grouping(ExprId),
    Unary(ExprId, Operator),
    Variable(String),
}
*/
pub fn annotation_to_type(parsed: &ParsedType) -> Option<Type> {
    return match parsed {
        ParsedType::pInt => Some(Type::int()),
        ParsedType::pFloat => Some(Type::float()),
        ParsedType::pStr => Some(Type::string()),
        ParsedType::pBoolean => Some(Type::boolean()),
        ParsedType::pArray(ref pinner, ref size) => array_annotation_to_type(pinner, size),
        ParsedType::pOptional(ref oinner) => Some(Type::optional(annotation_to_type(&*oinner).expect("Expected to be converted."))),
        _ => None,
    };
}

fn array_annotation_to_type(base: &Box<ParsedType>, size: &Option<u32>) -> Option<Type> {
    let core = annotation_to_type(base.as_ref());
    return if let Some(x) = core.as_ref() {
        Some(Type::array(x, size.clone()))
    } else {
        None
    };
}

pub fn literal_to_type(lit: &Literal) -> Type {
    match lit.kind {
        LiteralKind::Int => Type::int(),
        LiteralKind::Float => Type::float(),
        LiteralKind::Str => Type::string(),
        LiteralKind::Bool => Type::boolean(),
        LiteralKind::Null => Type::null(),
    }
}
