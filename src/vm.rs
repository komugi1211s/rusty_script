use super::{
    bytecode::CompiledCode,
    ir::IRCode,
    trace::prelude::*,
    types::Value,
};
use std::collections::HashMap;
#[derive(Debug)]
pub struct VirtualMachine
{
    // Instruction Pointer :: current instruction.
    inst_idx: usize,
    callst: Vec<usize>,
    stack: Vec<Value>,
    globals: HashMap<u32, Value>,
}

impl VirtualMachine
{
    pub fn new() -> Self
    {
        Self {
            inst_idx: 0,
            callst:  Vec::with_capacity(512),
            stack:   Vec::with_capacity(512),
            globals: HashMap::with_capacity(512),
        }
    }
}

pub fn start_vm(vm: &mut VirtualMachine, _module: &SourceFile, bin: &CompiledCode) -> ()
{
    vm.inst_idx = bin.ep;
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

            IRCode::EqEq | IRCode::NotEq 
            | IRCode::LessEq | IRCode::MoreEq 
            | IRCode::More | IRCode::Less =>
            {
                let rhs = vm.stack.pop().unwrap();
                let lhs = vm.stack.pop().unwrap();

                let result = match instruction
                {
                    IRCode::EqEq => lhs == rhs,
                    IRCode::NotEq => lhs != rhs,
                    IRCode::LessEq => lhs <= rhs,
                    IRCode::MoreEq => lhs >= rhs,
                    IRCode::Less => lhs < rhs,
                    IRCode::More => lhs > rhs,
                    _ => unreachable!(),
                }.into();
                vm.stack.push(result);
            }

            IRCode::Not =>
            {
                let lhs = vm.stack.pop().unwrap();
                let result = !lhs.is_truthy();
                vm.stack.push(result.into());
            }

            IRCode::Neg =>
            {
                let lhs = vm.stack.pop().unwrap();
                let result = -lhs;
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

            IRCode::Call(index) =>
            {
                vm.callst.push(vm.inst_idx);
                vm.inst_idx = *index as usize;
                continue;
            }

            IRCode::Return =>
            {
                match vm.callst.pop()
                {
                    Some(x) =>vm.inst_idx = x,
                    None => break,
                }
            }

            IRCode::DebugPrint =>
            {
                let value = vm.stack.pop().unwrap();
                println!("{}", value);
            }

            IRCode::Load(idx) =>
            {
                let value = vm.stack.get((*idx) as usize).unwrap().clone();
                vm.stack.push(value);
            }

            IRCode::Store(idx) =>
            {
                let top_stack = vm.stack.last().unwrap().clone();
                vm.stack[*idx as usize] = top_stack;
            }
            IRCode::True =>
            {
                vm.stack.push(Value::Boolean(true));
            }
            IRCode::False => 
            {
                vm.stack.push(Value::Boolean(false));
            }  
            IRCode::Null =>
            {
                vm.stack.push(Value::Null);
            }
            IRCode::GLoad(idx) =>
            {
                let value = vm.globals.get(idx).unwrap().clone();
                vm.stack.push(value);
            }

            IRCode::GStore(idx) =>
            {
                let top_stack = vm.stack.pop().unwrap();
                vm.globals.insert(*idx, top_stack);
            }

            IRCode::Interrupt =>
            {
                println!("Interrupt hit.");
                break;
            }

            x => {
                println!("{:?}", x);
                unimplemented!()
            }
        }

        vm.inst_idx += 1;
    }
}
