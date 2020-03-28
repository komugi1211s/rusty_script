use std::collections::HashMap;
use super::{
    global::Global,
    ast::*,
    semantic::{ SymTable },
    ir::IRCode,
    types::{ Type, Value },
};

mod expr;
mod stmt;
use stmt::traverse_statement;

#[derive(Debug)]
pub struct CompiledCode
{
    pub code: Vec<IRCode>,
    pub consts: Vec<Value>,
}

#[derive(Debug)]
struct Defn
{
    name: String,
    dtype: Type,
    depth: u16,
}

#[derive(Debug)]
pub struct Compiler<'s>
{
    table: &'s SymTable,
    codes: Vec<IRCode>,
    patch: Vec<Patch>,
    consts: Vec<Value>,
    depth: u16,

    local: Vec<Defn>,
    function_id: HashMap<String, usize>,
}

impl<'s> Compiler<'s>
{
    fn new(table: &'s SymTable) -> Self
    {
        Compiler {
            table: table,
            codes: Vec::with_capacity(5000),
            patch: Vec::with_capacity(255),
            depth: 0,
            local: Vec::with_capacity(255),
            consts: Vec::with_capacity(255),
            function_id: HashMap::with_capacity(256),
        }
    }

    fn add_const(&mut self, value: Value) -> usize
    {
        let current = self.consts.len();
        self.consts.push(value);
        current
    }

    fn deepen_nest(&mut self)
    {
        self.depth += 1;
    }

    fn shallowen_nest(&mut self)
    {
        self.depth -= 1;
        let depth = self.depth;
        self.local.retain(|x| x.depth <= depth);
    }

    fn search_local(&self, name: &str) -> Option<usize>
    {
        if self.local.is_empty() { return None; }
        
        let last_idx = self.local.len() - 1;
        for (idx, local) in self.local.iter().rev().enumerate()
        {
            let real_idx = last_idx - idx;
            if local.name == name
            {
                return Some(real_idx);
            }
        }
        None
    }

    #[inline(always)]
    fn emit_op(&mut self, code: IRCode)
    {
        self.codes.push(code);
    }

    #[inline(always)]
    fn add_patch(&mut self, patch: Patch)
    {
        self.patch.push(patch);
    }

    #[inline(always)]
    fn reserve_one(&mut self) -> Patch
    {
        let patch = Patch {
            kind: PatchKind::Generic,
            position: self.codes.len(),
        };
        self.emit_op(IRCode::Interrupt);
        patch
    }

    // TODO: Technically it could fail
    #[inline(always)]
    fn patch(&mut self, patch: Patch, with: IRCode)
    {
        let position = patch.position;
        self.codes[position] = with;
    }

    #[inline(always)]
    fn mark_break(&mut self)
    {
        let position = self.codes.len();
        self.add_patch(Patch {
            kind: PatchKind::Break,
            position,
        });
        self.codes.push(IRCode::Interrupt);
    }

    #[inline(always)]
    fn mark_continue(&mut self)
    {
        let position = self.codes.len();
        self.add_patch(Patch {
            kind: PatchKind::Continue,
            position,
        });
        self.codes.push(IRCode::Interrupt);
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PatchKind
{
    Generic,
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct Patch
{
    kind: PatchKind,
    position: usize,
}


pub fn generate_bytecode(global: &Global, ast: &ASTree) -> Result<CompiledCode, ()>
{
    // TODO: This is not how it supposed to work.
    // You cannot make "import" or "use" or any kind of module-type system work.
    let mut compiler = Compiler::new(&global.symtable);

    /*
    for func_node in ast.functions.iter()
    {
        prepare_function(&mut compiler, ast, *func_node)
    }
    */

    for node in ast.root.iter()
    {
        traverse_statement(&mut compiler, ast, *node)?;
    }

    compiler.emit_op(IRCode::Return);
    Ok(CompiledCode {
        code: compiler.codes,
        consts: compiler.consts,
    })
}
