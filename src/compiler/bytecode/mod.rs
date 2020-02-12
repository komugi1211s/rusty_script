
use std::collections::HashMap; 
use trace::{
    SourceFile,
    err_fatal,
    code_line,
};

use syntax_ast::ast::*;

use super::ir::IRCode;
use types::{ Value, Type };

mod stmt;
mod expr;
use stmt::traverse_statement;

#[derive(Debug)]
pub struct CompiledCode {
    pub code: Vec<IRCode>,
    pub consts: Constants,
}

#[derive(Debug)]
pub struct Constants {
    pub values: Vec<Value>,
}

impl Constants {
    fn new() -> Self {
	let mut _self = Self {
	    values: Vec::with_capacity(255) // TODO: change capacity
	};
	
	_self.values.push(Value::Null);
	_self.values.push(Value::Boolean(true));
	_self.values.push(Value::Boolean(false));
	
	_self
    }

    fn add_const(&mut self, value: Value) -> usize {
	let current = self.values.len();
	self.values.push(value);
	current
    }
}

#[derive(Debug)]
struct Local {
    name: String,
    dtype: Type,
    depth: u16
}

#[derive(Debug)]
pub struct Env<'a> {
    source: &'a SourceFile,
    depth:  u16,
    global: HashMap<String, Type>,
    local:  Vec<Local>,
    consts: Constants,
}

impl<'a> Env<'a> {
    fn new(source: &'a SourceFile) -> Self {
        Env {
	    source,
            depth:  0,
            global: HashMap::new(),
            local:  Vec::with_capacity(255),
            consts: Constants::new(),
        }
    }

    fn deepen_nest(&mut self) {
        self.depth += 1;
    }

    fn shallowen_nest(&mut self) {
        self.depth -= 1;
        let depth = self.depth;
        self.local.retain(|x| x.depth <= depth);
    }
}

#[derive(Debug)]
pub struct Context {
    codes: Vec<IRCode>,
    patch: Vec<Patch>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PatchKind {
    Generic,
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct Patch {
    kind: PatchKind,
    position: usize,
}

impl Context {
    fn new() -> Self {
	Self {
	    codes: Vec::with_capacity(5000),
	    patch: Vec::with_capacity(255),
     	}
    }

    #[inline(always)]
    fn emit_op(&mut self, code: IRCode) {
	self.codes.push(code);
    }
    
    #[inline(always)]
    fn add_patch(&mut self, patch: Patch) {
	self.patch.push(patch);
    }
    
    #[inline(always)]
    fn reserve_one(&mut self) -> Patch {
        let patch = Patch { kind: PatchKind::Generic, position: self.codes.len() };
        self.emit_op(IRCode::Interrupt);
        patch
    }

    // TODO: Technically it could fail
    #[inline(always)]
    fn patch(&mut self, patch: Patch, with: IRCode) {
        let position = patch.position;
        self.codes[position] = with;
    }

    #[inline(always)]
    fn mark_break(&mut self) {
        let position = self.codes.len();
        self.add_patch(Patch { kind: PatchKind::Break, position });
        self.codes.push(IRCode::Interrupt);
    }

    #[inline(always)]
    fn mark_continue(&mut self) {
        let position = self.codes.len();
        self.add_patch(Patch { kind: PatchKind::Continue, position });
        self.codes.push(IRCode::Interrupt);
    }
}


pub fn generate_bytecode(module: &SourceFile, ast: &ASTree) -> Result<CompiledCode, ()> {
    let mut ctx = Context::new();
    let mut env = Env::new(module);
    
    for node in ast.ast.iter() {
        traverse_statement(&mut env, &mut ctx, ast, node.stmt_id);
    }

    Ok(CompiledCode {
           code: ctx.codes.clone(),
	   consts: env.consts,
    })
}
