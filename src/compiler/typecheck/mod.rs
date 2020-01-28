pub mod astconv;
pub mod check;

// use syntax_ast::ast::*;
use types::{Type, TypeKind};

use std::cell::Cell;

#[derive(Clone, Debug)]
pub struct TypeArena {
    pub global: Vec<GlobalDef>,
    pub local: Vec<LocalDef>,
    _exist_count: Cell<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeContext {
    Variable(String),
    Existential(String),
    Solved(Type),
}

pub trait Solve {
    fn is_solved(&self) -> bool;
    fn is_not_solved(&self) -> bool { !self.is_solved() }
}

impl Solve for TypeContext {
    fn is_solved(&self) -> bool {
        match self {
            Self::Solved(_) => true,
            _ => false
        }
    }
}

impl TypeContext {
    pub fn inner_ref(&self) -> Option<&Type> {
        match self {
            Self::Solved(ref x) => Some(x),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
pub struct GlobalDef {
    pub name: String,
    pub dtype: TypeContext,
}

impl Solve for GlobalDef {
    fn is_solved(&self) -> bool {
        self.dtype.is_solved()
    }
}

#[derive(Clone, Debug)]
pub struct LocalDef {
    pub name: String,
    pub dtype: TypeContext,
    pub depth: u16,
}

impl Solve for LocalDef {
    fn is_solved(&self) -> bool {
        self.dtype.is_solved()
    }
}

impl TypeArena {
    pub fn new() -> Self {
        Self {
            global: vec![],
            local: vec![],
            _exist_count: Cell::new(0),
        }
    }

    #[inline]
    pub fn add_local(&mut self, dtype: TypeContext, name: &str, depth: u16) -> Result<usize, usize> {
        let (exists, pos) = self.find_local(name, depth);
        if exists && self.local[pos].depth == depth {
            return Err(pos);
        }

        let add = LocalDef {
            name: name.to_string(),
            dtype,
            depth,
        };
        self.local.push(add);
        Ok(self.local.len() - 1)
    }

    #[inline]
    pub fn add_global(&mut self, dtype: TypeContext, name: &str) -> Result<usize, usize> {
        let (exists, pos) = self.find_global(name);
        if exists {
            return Err(pos);
        }
        let add = GlobalDef {
            name: name.to_string(),
            dtype,
        };
        self.global.push(add);
        Ok(self.global.len() - 1)
    }

    pub fn new_existential(&self) -> TypeContext {
        let current = self._exist_count.get();
        self._exist_count.set(self._exist_count.get() + 1);
        TypeContext::Existential(format!("Ext{}", current))
    }

    #[inline]
    pub fn find_local(&self, name: &str, current_depth: u16) -> (bool, usize) {
        // We're reversing this, so index would be length - ind
        let is_exist = self.local.iter()
                                 .rev()
                                 .position(|x| &x.name == name && x.depth <= current_depth);
        (is_exist.is_some(), is_exist.unwrap_or(0))
    }

    #[inline]
    pub fn find_global(&self, name: &str) -> (bool, usize) {
        let is_exist = self.global.iter()
                                  .position(|x| &x.name == name);

        (is_exist.is_some(), is_exist.unwrap_or(0))
    }

    pub fn determine_local(&mut self, position: usize, dtype: Type) -> Result<(), ()> {
        if self.local[position].is_solved() {
            if self.local[position].dtype.inner_ref().unwrap() != &dtype {
                return Err(())
            }
        } else {
            self.local[position].dtype = TypeContext::Solved(dtype);
        }
        Ok(())
    }

    pub fn determine_global(&mut self, position: usize, dtype: Type) -> Result<(), ()> {
        if self.global[position].is_solved() {
            if self.global[position].dtype.inner_ref().unwrap() != &dtype {
                return Err(())
            }
        } else {
            self.global[position].dtype = TypeContext::Solved(dtype);
        }
        Ok(())
    }

    pub fn ditch_out_of_scope(&mut self, depth: u16) {
        self.local.retain(|x| x.depth <= depth);
    }
}
