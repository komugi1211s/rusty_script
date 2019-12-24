
use types::{ Type, TypeKind, TypeOption };
use syntax_ast::ast_data::*;


pub fn literal_to_type(lit: &Literal) -> Type {
    match lit.kind {
        LiteralKind::Int => Type::int(),
        LiteralKind::Float => Type::float(),
        LiteralKind::Str => Type::string(),
        LiteralKind::Bool => Type::boolean(),
        LiteralKind::Null => Type::default(),
    }
}


pub fn type_after_binary(a: &Type, b: &Type, oper: Oper) -> Result<Type, ()> {
    let a = a.kind;
    let b = b.kind;
    match oper {
        Operator::EqEq
        | Operator::NotEq
        | Operator::More
        | Operator::MoreEq
        | Operator::Less
        | Operator::LessEq => Ok(Type::boolean()),

        Operator::Div => Ok(Type::float()),
        Operator::Mod => Ok(Type::int()),
        Operator::Sub | Operator::Mul => {
            if a == TypeKind::Float || b == TypeKind::Float {
                Ok(Type::float())
            } else {
                Ok(Type::int())
            }
        }
        Operator::Add => {
            if a == TypeKind::Str && b == TypeKind::Str {
                Ok(Type::string())
            } else if a == TypeKind::Float || b == TypeKind::Float {
                Ok(Type::float())
            } else {
                Ok(Type::int())
            }
        }
        _ => Err(()),
    }
}

pub fn type_after_unary(a: &Type, oper: Operator) -> Result<Type, ()> {
    let a_type = a.kind;
    match oper {
        Operator::Neg => {
            if a_type == TypeKind::Float || a_type == TypeKind::Int {
                return Ok(a.clone());
            }
            Err(())
        }
        Operator::Not => Ok(Type::boolean()),
        _ => Err(()),
    }
}


