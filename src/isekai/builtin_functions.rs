use super::{ 
    bytecode::{ BytecodeGenerator, FuncInfo },
    vm::{ VirtualMachine },
    types::{ Value, Type, TypeKind, TypeOption },
    utils,
};
use std::time::{ SystemTime, UNIX_EPOCH };

// ********************* INTERNAL STUFF ********************* //
fn applyfunc(bc: &mut BytecodeGenerator, name: &str, args: Vec<Type>, rettype: Type, func: fn(&mut VirtualMachine)) {
    let funcinfo = FuncInfo::native(name, args.len(), args, rettype, func);

    let mut functype = rettype;
    functype.option.insert(TypeOption::Func);
    bc.global_define.push((functype, name.to_string()));
    let idx = bc.global_define.len() - 1;
    bc.function_table.insert(idx, funcinfo);
}

pub fn apply_native_functions(bc: &mut BytecodeGenerator) {
    applyfunc(bc, "clock", vec![], Type::float(), clock_adapter);
    applyfunc(bc, "assert", vec![Type::boolean()], Type::default(), assert_adapter);
}

// ********************* BUILTIN STUFF ********************* //
fn clock() -> f64 {
    return SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as f64;
}

fn clock_adapter(vm: &mut VirtualMachine) {
    vm.stack.push(clock().into());
}

fn assert(is_true: bool) {
    assert_eq!(is_true, true);
}

fn assert_adapter(vm: &mut VirtualMachine) {
    let is_true = vm.stack.pop().unwrap();
    assert(is_true.is_truthy());
}
