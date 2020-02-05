
use trace::SourceFile;

#[derive(Debug)]
pub struct VirtualMachine {
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {}
    }
}

pub fn start_vm(vm: &mut VirtualMachine, module: &SourceFile, code: &()) -> () {
}
