use super::{
    global::Global,
    ast::*,
    semantic::{ SymTable },
    // trace::prelude::*;
};

use llvm_sys::core;
use llvm_sys::execution_engine;
use llvm_sys::target;
// use llvm_sys::value;


macro_rules! llvmstr {
    ($x:ident) => {
        b"$x\0".as_ptr() as *const _
    }
}

fn preinit()
{
    unsafe
    {
        target::LLVM_InitializeAllTargetInfos();
        target::LLVM_InitializeAllTargets();
        target::LLVM_InitializeAllTargetMCs();
        target::LLVM_InitializeAllAsmPrinters();
        target::LLVM_InitializeAllAsmParsers();
    }
}

pub fn llvm_dump(global: &Global, ast: &ASTree)
{
    preinit();
    let context = unsafe { core::LLVMContextCreate() };
    let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

    let root_mod = unsafe { core::LLVMModuleCreateWithName(b"root\0".as_ptr() as *const _) };

    let void = unsafe { core::LLVMVoidTypeInContext(context) };
    let print_type = unsafe { core::LLVMFunctionType(void, std::ptr::null_mut(), 0, 0) };

    let function = unsafe { core::LLVMAddFunction(root_mod, llvmstr!(print), print_type) };
}


