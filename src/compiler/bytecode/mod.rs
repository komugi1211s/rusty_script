use std::collections::HashMap;
use trace::{code_line, err_fatal, SourceFile};

use syntax_ast::ast::*;

use super::ir::IRCode;
use types::{Type, Value};

mod expr;
mod stmt;
use stmt::traverse_statement;

#[derive(Debug)]
pub struct CompiledCode {
    pub code: Vec<IRCode>,
    pub consts: Vec<Value>,
}

#[derive(Debug)]
struct Local {
    name: String,
    dtype: Type,
    depth: u16,
}

#[derive(Debug)]
pub struct Compiler {
    codes: Vec<IRCode>,
    patch: Vec<Patch>,
    depth: u16,
    global: HashMap<String, Type>,
    local: Vec<Local>,
    consts: Vec<Value>, 
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            codes: Vec::with_capacity(5000),
            patch: Vec::with_capacity(255),
            depth: 0,
            global: HashMap::new(),
            local: Vec::with_capacity(255),
            consts: Vec::with_capacity(255),
        }
    }

    fn add_const(&mut self, value: Value) -> usize {
        let current = self.consts.len();
        self.consts.push(value);
        current
    }

    fn deepen_nest(&mut self) {
        self.depth += 1;
    }

    fn shallowen_nest(&mut self) {
        self.depth -= 1;
        let depth = self.depth;
        self.local.retain(|x| x.depth <= depth);
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
        let patch = Patch {
            kind: PatchKind::Generic,
            position: self.codes.len(),
        };
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
        self.add_patch(Patch {
            kind: PatchKind::Break,
            position,
        });
        self.codes.push(IRCode::Interrupt);
    }

    #[inline(always)]
    fn mark_continue(&mut self) {
        let position = self.codes.len();
        self.add_patch(Patch {
            kind: PatchKind::Continue,
            position,
        });
        self.codes.push(IRCode::Interrupt);
    }
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

pub fn generate_bytecode(ast: &ASTree) -> Result<CompiledCode, ()> {
    let mut compiler = Compiler::new();

    for node in ast.ast.iter() {
        traverse_statement(&mut compiler, ast, node.stmt_id);
    }

    Ok(CompiledCode {
        code: compiler.codes,
        consts: compiler.consts,
    })
}
