use syntax_ast::ast::*;
use types::{Type, TypeKind};

use super::{TypeArena, TypeContext, astconv};

fn check_against_parsedtype(pat: &ParsedType, t: &Type) -> bool {
    match (pat, &t.kind) {
        (ParsedType::pInt, TypeKind::Int) => true,
        (ParsedType::pStr, TypeKind::Str) => true,
        (ParsedType::pFloat, TypeKind::Float) => true,
        (ParsedType::pBoolean, TypeKind::Boolean) => true,
        (ParsedType::pArray(ref pinner, ref psize),
            TypeKind::Array(ref tinner, ref tsize)) => {
            check_against_parsedtype(&**pinner, &**tinner) && psize == tsize
        },
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

pub fn synthesize(cont: &mut TypeArena, ast: &ParsedResult, expr: ExprId) -> Result<Type, ()> {
    use Expr::*;
    match ast.get_expr(expr) {
        Literal(ref lit) => Ok(astconv::literal_to_type(lit)),
        FunctionCall(func_name, func_args) => {
            let ret_ty = synthesize(cont, ast, func_name)?;
            // function type should be here.
            match ret_ty.kind {
                TypeKind::Function { ref args, ret } => {
                    for (declared, actual_id) in args.iter().zip(func_args) {
                        let arg_type = synthesize(cont, ast, actual_id)?;
                        if declared != &arg_type {
                            return Err(());
                        }
                    }

                    Ok(*ret)
                }
                _ => Err(())
            }
        }
        _ => None
    }
}

