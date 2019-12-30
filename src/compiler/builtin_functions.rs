use super::{ 
    bytecode::{ BytecodeGenerator, FuncInfo },
    vm::{ VirtualMachine },
    utils,
};
use types::types::{ Value, Type, TypeKind, TypeOption };
use std::time::{ SystemTime, UNIX_EPOCH };

// ********************* INTERNAL STUFF ********************* //
fn applyfunc(bc: &mut BytecodeGenerator, name: &str, args: Vec<Type>, rettype: Type, func: fn(&mut VirtualMachine)) {
    let funcinfo = FuncInfo::native(name, args.len(), args, rettype, func);

    let mut functype = rettype;
    functype.option.insert(TypeOption::Func);
    bc.add_global(functype, name);
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
    assert!(is_true);
}

fn assert_adapter(vm: &mut VirtualMachine) {
    let is_true = vm.stack.pop().unwrap();
    assert(is_true.is_truthy());
}


/*
    build in func APIs

    let func_builder = Buildin::Function;
    func_builder.set_returntype(Type { kind: Struct("hello"), option: TypeOption::empty() });
    func_builder.add_args("foo", Type::int());
    func_builder.add_second_args("foo", Type::int());

    func_builder.body(create_body
 */
