use syntax_ast::ast::*;
use types::{Type, TypeKind};

use super::{TypeArena, TypeContext, astconv, Solve};

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

// TODO - @DumbCode: Depth thing is stupid
pub fn synthesize(cont: &mut TypeArena, ast: &ParsedResult, expr: ExprId, depth: u16) -> Result<Type, ()> {
    let is_global = depth > 0;
    use Expr::*;
    match ast.get_expr(expr) {
        Literal(ref lit) => Ok(astconv::literal_to_type(lit)),
        FunctionCall(func_name, func_args) => {
            let func_ty = synthesize(cont, ast, *func_name, depth)?;
            // function type should be here.
            match func_ty.kind {
                TypeKind::Function { ref args, ref ret } => {
                    for (declared, actual_id) in args.iter().zip(func_args) {
                        let arg_type = synthesize(cont, ast, *actual_id, depth)?;
                        if declared != &arg_type {
                            return Err(());
                        }
                    }
                    Ok(*ret.clone())
                }
                _ => Err(())
            }
        }
        Assign(var_id, expr_id) => {
            let var_type = synthesize(cont, ast, *expr_id, depth)?;
            if check_against(cont, ast, expr_id, &var_type, depth) {
                Ok(var_type)
            } else {
                Err(())
            }
        }
        Variable(ref name_str) => {
            let (exists, pos) = if is_global {
                cont.find_global(name_str)
            } else {
                cont.find_local(name_str, depth)
            };

            if !exists {
                // TODO
                return Err(());
            }
            
            let _type = if is_global {
                &cont.global[pos].dtype
            } else {
                &cont.local[pos].dtype
            };
            
            if _type.is_solved() {
                Ok(_type.inner_ref().unwrap().clone())
            } else {
                Err(())
            }
        }
        Binary(lhs, rhs, oper) => {
            let lhs_type = synthesize(cont, ast, *lhs, depth)?;

            if check_against(cont, ast, rhs, &lhs_type, depth) {
                Ok(lhs_type)
            } else {
                Err(())
            }
        }
        Unary(item_id, oper) => {
            let item_type = synthesize(cont, ast, *item_id, depth)?;
            Ok(item_type)
        }
        Grouping(inner) => {
            Ok(synthesize(cont, ast, *inner, depth)?)
        }
        Logical(lhs, rhs, oper) => {
            let lhs_type = synthesize(cont, ast, *lhs, depth)?;
            if check_against(cont, ast, rhs, &lhs_type, depth) {
                Ok(Type::boolean())
            } else {
                Err(())
            }
        }
    }
}

pub fn check_against(cont: &mut TypeArena, ast: &ParsedResult, expr: &ExprId, req_type: &Type, depth: u16) -> bool {
    let is_global = depth > 0;
    use Expr::*;

    match ast.get_expr(*expr) {
        Variable(ref name) => {
            let (exists, pos) = if is_global {
                cont.find_global(name)
            } else {
                cont.find_local(name, depth)
            };

            if !exists {
                false
            } else {
                if is_global {
                    &cont.global[pos].dtype
                } else {
                    &cont.local[pos].dtype
                }.inner_ref().unwrap() == req_type
            }
        }
        _ => false,
    }
}

pub fn is_well_formed(cont: &TypeArena, t: &Type, depth: u16) -> bool {
    let is_global = depth > 0;
    use TypeKind::*;
    match t.kind {
        Int | Float | Str | Boolean | Null | Array(_, _) | Function { .. } | Compound { .. } => true,
        Variable(ref name) =>  {
            let (exists, pos) = if is_global {
                cont.find_global(name)
            } else {
                cont.find_local(name, depth)
            };

            if !exists {
                false
            } else {
                let dtype = if is_global {
                    &cont.global[pos].dtype 
                } else {
                    &cont.local[pos].dtype 
                };
                dtype == &TypeContext::Var(name.to_string())
            }
        }
        Existential(ref name) => {
            let (exists, pos) = if is_global {
                cont.find_global(name)
            } else {
                cont.find_local(name, depth)
            };

            if !exists {
                false
            } else {
                let dtype = if is_global {
                    &cont.global[pos].dtype 
                } else {
                    &cont.local[pos].dtype 
                };
                dtype == &TypeContext::Existential(name.to_string())
            }
        }
    }
}




/*

Unknown Length Array: Currently x = Array(Existential);
x.is_solved() == false;

new_item := [];

new_item.push(2);
Currently x = Array(Numeric);
x.is_solved() == false;
x.determine() == Array(Int);

new_item.push(2.5);
Currently x == Array(Float);
x.is_solved() == true;

*/