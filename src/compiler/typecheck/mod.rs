pub mod astconv;
pub mod check;

use syntax_ast::ast::*;
use types::{Type, TypeKind};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeArena {
    cont: Vec<TypeContext>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeContext {
    Var(String),
    Exist(String),
    Solved(String, Type),
    Marked(String),
    TypedVariable(String, Type),
}
