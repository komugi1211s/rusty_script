
use trace::Module;

#[derive(Debug)]
pub struct VirtualMachine {
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {}
    }
}

pub fn start_vm(vm: &mut VirtualMachine, module: &Module, code: &()) -> () {
}
