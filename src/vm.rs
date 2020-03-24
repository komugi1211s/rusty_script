use super::{
    bytecode::CompiledCode,
    ir::IRCode,
    trace::prelude::*,
    types::Value,
};

#[derive(Debug)]
pub struct VirtualMachine
{
    // Instruction Pointer :: current instruction.
    inst_idx: usize,
    callst: Vec<usize>,

    stack: Vec<Value>,
}

impl VirtualMachine
{
    pub fn new() -> Self
    {
        Self {
            inst_idx: 0,
            callst: Vec::with_capacity(65535),
            stack: Vec::with_capacity(65535),
        }
    }
}

pub fn start_vm(vm: &mut VirtualMachine, module: &SourceFile, bin: &CompiledCode) -> ()
{
    let code_length = bin.code.len();

    while code_length > vm.inst_idx
    {
        let instruction = &bin.code[vm.inst_idx];

        match instruction
        {
            // TODO: redundant const difference
            IRCode::Const8(idx) =>
            {
                let value = bin.consts[*idx as usize].clone();
                vm.stack.push(value);
            }

            IRCode::Const64(idx) =>
            {
                let value = bin.consts[*idx as usize].clone();
                vm.stack.push(value);
            }

            IRCode::ConstDyn(idx) =>
            {
                let value = bin.consts[*idx as usize].clone();
                vm.stack.push(value);
            }

            IRCode::Add | IRCode::Sub | IRCode::Mul | IRCode::Div =>
            {
                let rhs = vm.stack.pop().unwrap();
                let lhs = vm.stack.pop().unwrap();

                let result = match instruction
                {
                    IRCode::Add => lhs + rhs,
                    IRCode::Sub => lhs - rhs,
                    IRCode::Mul => lhs * rhs,
                    IRCode::Div => lhs / rhs,
                    _ => unreachable!(),
                };
                vm.stack.push(result);
            }

            IRCode::Jump(to) =>
            {
                vm.inst_idx = *to as usize;
                continue;
            }

            IRCode::JT(to) =>
            {
                let cond = vm.stack.pop().unwrap();
                if cond.is_truthy()
                {
                    vm.inst_idx = *to as usize;
                    continue;
                }
            }

            IRCode::JNT(to) =>
            {
                let cond = vm.stack.pop().unwrap();
                if !cond.is_truthy()
                {
                    vm.inst_idx = *to as usize;
                    continue;
                }
            }

            IRCode::DebugPrint =>
            {
                let value = vm.stack.pop().unwrap();
                println!("{}", value);
            }

            _ => unimplemented!(),
        }

        vm.inst_idx += 1;
    }
}