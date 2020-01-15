use syntax_ast::ast::{self, ParsedType};
use types::Type;

pub fn annotation_to_type(parsed: &ParsedType) -> Option<Type> {
    return match parsed {
        ParsedType::pInt => Some(Type::int()),
        ParsedType::pFloat => Some(Type::float()),
        ParsedType::pStr => Some(Type::string()),
        ParsedType::pBoolean => Some(Type::boolean()),
        ParsedType::pArray(ref pinner, ref size) => array_annotation_to_type(pinner, size),
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

pub fn literal_to_type(lit: &ast::Literal) -> Type {
    match lit.kind {
        ast::LiteralKind::Int => Type::int(),
        ast::LiteralKind::Float => Type::float(),
        ast::LiteralKind::Str => Type::string(),
        ast::LiteralKind::Bool => Type::boolean(),
        ast::LiteralKind::Null => Type::default(),
    }
}
