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
    
    // Stack Pointer :: last stack pointer. 
    stack_idx: usize,

    // Call stack :: Call stack.
    callst: Vec<(usize, usize)>,
    stack: Vec<Value>,
    globals: HashMap<u32, Value>,
}

const CALLSTACK_DEPTH: usize = 1024;

impl VirtualMachine
{
    pub fn new() -> Self
    {
        Self {
            inst_idx:  0,
            stack_idx: 0,
            callst:  Vec::with_capacity(CALLSTACK_DEPTH),
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
                let rhs = expect_opt!(vm.stack.pop(), "ADD/SUB/MUL/DIV RHS");
                let lhs = expect_opt!(vm.stack.pop(), "ADD/SUB/MUL/DIV LHS"); 

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
                let rhs = expect_opt!(vm.stack.pop(), "EQEQ, NOTEQ, LESSEQ... RHS");
                let lhs = expect_opt!(vm.stack.pop(), "EQEQ, NOTEQ, LESSEQ... LHS"); 

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
                let lhs = expect_opt!(vm.stack.pop(), "NOT LHS");
                let result = !lhs.is_truthy();
                vm.stack.push(result.into());
            }

            IRCode::Neg =>
            {
                let lhs = expect_opt!(vm.stack.pop(), "NEG LHS");
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
                let cond = expect_opt!(vm.stack.pop(), "JT COND");
                if cond.is_truthy()
                {
                    vm.inst_idx = *to as usize;
                    continue;
                }
            }

            IRCode::JNT(to) =>
            {
                let cond = expect_opt!(vm.stack.pop(), "JNT COND");
                if !cond.is_truthy()
                {
                    vm.inst_idx = *to as usize;
                    continue;
                }
            }

            IRCode::Call(index, argcount) =>
            {
                if vm.callst.len() > CALLSTACK_DEPTH
                {
                    report("Callstack Overflow", "関数の再帰呼び出しが制限に達しました。");
                    return;
                }
                vm.callst.push((vm.inst_idx, vm.stack_idx));
                vm.stack_idx = vm.stack.len() - (*argcount) as usize;
                vm.inst_idx = *index as usize;
                continue;
            }

            IRCode::Return =>
            {
                if vm.stack.is_empty() { break; }

                match vm.callst.pop()
                {
                    Some((eip, esp)) =>
                    {
                        let value = expect_opt!(vm.stack.pop(), "RETURN VALUE");
                        vm.stack.truncate(vm.stack_idx);
                        vm.inst_idx = eip;
                        vm.stack_idx = esp;

                        vm.stack.push(value);
                    },
                    None => break,
                }
            }

            IRCode::DebugPrint =>
            {
                let value = expect_opt!(vm.stack.pop(), "DEBUGPRINT VALUE");
                println!("{}", value);
            }

            IRCode::Load(idx) =>
            {
                let value = expect_opt!(vm.stack.get(vm.stack_idx + (*idx as usize)), "LOAD VALUE").clone();
                vm.stack.push(value);
            }

            IRCode::Store(idx) =>
            {
                let top_stack = expect_opt!(vm.stack.last(), "STORE TOP_STACK").clone();
                vm.stack[vm.stack_idx + (*idx as usize)] = top_stack;
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
                let value = expect_opt!(vm.globals.get(idx), "GLOAD").clone();
                vm.stack.push(value);
            }
            IRCode::GStore(idx) =>
            {
                let top_stack = expect_opt!(vm.stack.pop(), "GSTORE");
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
