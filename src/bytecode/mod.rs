use std::collections::HashMap;
use super::{
    global::Global,
    ast::*,
    semantic::{ SymTable },
    ir::IRCode,
    types::{ Value },
};

mod expr;
mod stmt;
use stmt::traverse_statement;

#[derive(Debug)]
pub struct CompiledCode
{
    pub ep:     usize,
    pub code:   Vec<IRCode>,
    pub consts: Vec<Value>,
}

pub struct Compiler<'s>
{
    table: &'s SymTable,
    codes: Vec<IRCode>,
    patch: Vec<Patch>,
    consts: Vec<Value>,
    function_idx: HashMap<String, (u32, u32)>,
}

impl<'s> Compiler<'s>
{
    fn new(table: &'s SymTable) -> Self
    {
        Compiler {
            table: table,
            codes: Vec::with_capacity(5000),
            patch: Vec::with_capacity(255),
            consts: Vec::with_capacity(255),
            function_idx: HashMap::with_capacity(256),
        }
    }

    fn add_const(&mut self, value: Value) -> usize
    {
        let current = self.consts.len();
        self.consts.push(value);
        current
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

fn prepare_function(compiler: &mut Compiler, ast: &ASTree, func_node: &FunctionData) -> Result<(), ()>
{
    if compiler.function_idx.contains_key(&func_node.it.name) 
    {
        return Ok(());
    }

    let index = compiler.codes.len();
    let argcount = func_node.args.len();
    compiler.function_idx.insert(func_node.it.name.clone(), (index as u32, argcount as u32));

    for stmt_id in func_node.body.iter()
    {
        traverse_statement(compiler, ast, *stmt_id)?;
    }

    if func_node.implicit_return_required.get() 
    {
        compiler.emit_op(IRCode::Null);
        compiler.emit_op(IRCode::Return);
    }
    Ok(())
}


pub fn generate_bytecode(global: &Global, ast: &ASTree) -> Result<CompiledCode, ()>
{
    let mut compiler = Compiler::new(&global.symtable);
    for func_node in ast.functions.iter()
    {
        prepare_function(&mut compiler, ast, func_node)?;
    }

    let ep = compiler.codes.len();
    for node in ast.root.iter()
    {
        traverse_statement(&mut compiler, ast, *node)?;
    }

    // compiler.emit_op(IRCode::Return);
    Ok(CompiledCode {
        ep,
        code: compiler.codes,
        consts: compiler.consts,
    })
}
