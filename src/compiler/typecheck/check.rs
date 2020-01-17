use syntax_ast::ast::*;
use types::{Type, TypeKind};

use super::{TypeArena, TypeContext};

fn check_against_parsedtype(pat: &ParsedType, t: &Type) -> bool {
    match (pat, &t.kind) {
        (ParsedType::pInt, TypeKind::Int) => true,
        (ParsedType::pStr, TypeKind::Str) => true,
        (ParsedType::pFloat, TypeKind::Float) => true,
        (ParsedType::pBoolean, TypeKind::Boolean) => true,
        (ParsedType::pArray(ref pinner, ref psize), TypeKind::Array(ref tinner, ref tsize)) => {
            check_against_parsedtype(&**pinner, &**tinner) && psize == tsize
        }
        _ => false,
    }
}

// TODO - @DumbCode
pub fn is_str_builtin_type(candidate: &str) -> bool {
    if candidate.starts_with("int") {
        return true;
    };
    if candidate.starts_with("bool") {
        return true;
    };
    if candidate.starts_with("string") {
        return true;
    };
    if candidate.starts_with("float") {
        return true;
    };
    if candidate.starts_with("struct") {
        return true;
    };
    return false;
}


