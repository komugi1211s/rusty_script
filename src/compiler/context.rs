use super::opcode::{ OpCode };
use syntax_ast::ast::{ Operator };

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    code: OpCode,
    operand: Option<[u8; 8]>,
    _c: u8,
}

impl Instruction {
    fn single(code: OpCode) -> Self {
        Self { 
            code,
            operand: None,
            _c: 0,
        }
    }

    fn operand(code: OpCode, oper: usize) -> Self {
        Self {
            code,
            operand: Some(oper.to_ne_bytes()),
            _c: 0,
        }
    }
}

impl From<OpCode> for Instruction {
     fn from(x: OpCode) -> Instruction {
     	Instruction::single(x)
     }
}

impl Iterator for Instruction {
    type Item=u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self._c == 0 {
            self._c += 1;
            Some(self.code as u8)
        } else {
            match self.operand {
                None => None,
                Some(addr) => {
                    if self._c > 9 {
                        None
                    } else {
                        let elem = addr[self._c as usize];
                        self._c += 1;
                        Some(elem)
                    }
                }
            }
        }
    }
}

pub struct Context {
    codes: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BranchMode {
    Empty,
    IfElse,
    While,
    For,
    Finalized,
}

pub struct ConditionalBranch {
    mode: BranchMode,
    code: Vec<Instruction>,
    finalized: bool,
    start_byte_offset: usize,
    true_route_pos: usize,
    false_route_pos: usize,
}

impl ConditionalBranch {
    fn new(addr: usize) -> Self {
        Self {
            mode: BranchMode::Empty,
            code: Vec::new(),
            finalized: false,
            start_byte_offset: addr,
            true_route_pos: 0,
            false_route_pos: 0,
        }
    }

    fn mode_if(&mut self) -> Result<(), ()> {
        if self.mode != BranchMode::Empty {
            return Err(())
        }

        let entered_if_mode = self.true_route_pos != 0;
        let entered_else_mode = self.false_route_pos != 0;
        if entered_if_mode || entered_else_mode {
            return Err(());
        }

        self.true_route_pos = self.inst_len();
        self.emit_op(OpCode::JNT);
        self.set_mode(BranchMode::IfElse)
    }

    fn mode_else(&mut self) -> Result<(), ()> {
        let already_entered_else_mode = self.false_route_pos != 0;
        if already_entered_else_mode {
            return Err(());
        }

        if self.mode == BranchMode::IfElse {
            self.false_route_pos = self.inst_len();
            self.emit_op(OpCode::Jump);
            Ok(())
        } else {
            Err(())
        }
    }
}

impl CodeGen for ConditionalBranch {
    fn emit_instruction(&mut self, inst: Instruction) -> Result<(), ()> {
        if self.is_finalized() {
            Err(())
        } else {
            self.code.push(inst);
            Ok(())
        }
    }

    fn emit_op(&mut self, code: OpCode) -> Result<(), ()> {
        if self.is_finalized() {
            Err(())
        } else {
            self.code.push(code.into());
            Ok(())
        }
    }

    fn eject(&self) -> Vec<Instruction> {
        self.code.clone()
    }

    fn compile(&self) -> Vec<u8> {
        self.code.clone().into_iter().flatten().collect()
    }

    fn byte_len(&self) -> usize {
        self.code.iter().map(|&x| x.code.len()).sum()
    }

    fn inst_len(&self) -> usize {
        self.code.len()
    }
}

impl Branch for ConditionalBranch {
    fn set_mode(&mut self, mode: BranchMode) -> Result<(), ()> {
        if self.is_finalized() {
            return Err(());
        }

        self.mode = mode;
        Ok(())
    }

    fn is_finalized(&self) -> bool {
        self.finalized
    }

    fn finalize(&mut self) -> Result<(), ()> {
        if self.is_finalized() {
            return Err(());
        }

        // Adjust and patch addresses.
        for inst in self.code.iter_mut() {
            if let Some(ref mut x) = inst.operand {
                let v = usize::from_ne_bytes(*x);
                *x = (v + self.start_byte_offset).to_ne_bytes();
            }
        }

        match self.mode {
            BranchMode::IfElse => {
                let else_is_empty = self.false_route_pos == 0;
                let bytelength_of_if: usize = self.code.as_slice()[self.true_route_pos .. self.false_route_pos].iter().map(|&x| x.code.len()).sum();
                let bytelength_of_else: usize = self.code.as_slice()[self.false_route_pos .. self.inst_len()-1].iter().map(|&x| x.code.len()).sum();

                // patches where to jump if expression is true.
                {
                    let bytelen = self.byte_len();
                    let falselen = self.false_route_pos;
                    match self.code.get_mut(self.true_route_pos) {
                        Some(ref mut if_pos_inst) => {
                            if else_is_empty {
                                if_pos_inst.operand = Some(bytelen.to_ne_bytes());
                            } else {
                                if_pos_inst.operand = Some(falselen.to_ne_bytes());
                            }
                        },
                        None => return Err(()),
                    }
                    // 
                }
            }
            _ => (),
        }
        self.finalized = true;
        Err(())
    }
}

impl Branchable for ConditionalBranch {
    fn new_branch(&self) -> ConditionalBranch {
        Self::new(self.byte_len() + self.start_byte_offset)
    }

    fn accept(&mut self, branch: impl Branch) -> Result<(), ()> {
        if self.is_finalized() || !branch.is_finalized() {
            return Err(());
        }
        self.code.extend(branch.eject());
        Ok(())
    }
}

pub trait Branchable: CodeGen {
    fn new_branch(&self) -> ConditionalBranch;
    fn accept(&mut self, _: impl Branch) -> Result<(), ()>;
}

pub trait Branch: CodeGen {
    fn set_mode(&mut self, mode: BranchMode) -> Result<(), ()>;
    fn is_finalized(&self) -> bool;
    fn finalize(&mut self) -> Result<(), ()>;
}

pub trait CodeGen {
    fn inst_len(&self) -> usize;
    fn byte_len(&self) -> usize;
    fn eject(&self) -> Vec<Instruction>;
    fn compile(&self) -> Vec<u8>;

    fn emit_op(&mut self, _: OpCode) -> Result<(), ()>;
    fn emit_instruction(&mut self, _: Instruction) -> Result<(), ()>;
    fn emit_arithmetic(&mut self, oper: Operator) -> Result<(), ()> {
        if !oper.is_arithmetic() {
            return Err(());
        }

        let code = match oper {
            Operator::Add => OpCode::Add,
            Operator::Sub => OpCode::Sub, 
            Operator::Div => OpCode::Div, 
            Operator::Mul => OpCode::Mul,
            Operator::Mod => OpCode::Mod, 
            Operator::Neg => OpCode::Neg,
            _      => unreachable!(),
        };

        self.emit_op(code)
    }
    fn emit_comparison(&mut self, oper: Operator) -> Result<(), ()> { 
        if !oper.is_comparison() {
            return Err(());
        }

        let code = match oper {
            Operator::EqEq   => OpCode::EqEq,
            Operator::NotEq  => OpCode::NotEq,
            Operator::LessEq => OpCode::LessEq, 
            Operator::MoreEq => OpCode::MoreEq, 
            Operator::Less   => OpCode::Less, 
            Operator::More   => OpCode::More,
            _ => unreachable!(),
        };

        self.emit_op(code)
    }
    fn emit_logical(&mut self, oper: Operator) -> Result<(), ()> {
        if !oper.is_logic() {
            return Err(());
        }
        let code = match oper {
            Operator::And => OpCode::And,
            Operator::Or  => OpCode::Or,
            Operator::Not => OpCode::Not,
            _ => unreachable!(),
        };

        self.emit_op(code)
    }
}

impl CodeGen for Context {
    fn emit_instruction(&mut self, inst: Instruction) -> Result<(), ()> {
        self.codes.push(inst);
        Ok(())
    }

    fn emit_op(&mut self, code: OpCode) -> Result<(), ()> {
        self.codes.push(code.into());
        Ok(())
    }

    fn eject(&self) -> Vec<Instruction> {
        self.codes.clone()
    }

    fn compile(&self) -> Vec<u8> {
        self.codes.clone().into_iter().flatten().collect()
    }

    fn byte_len(&self) -> usize {
        self.codes.iter().map(|&x| x.code.len()).sum()
    }

    fn inst_len(&self) -> usize {
        self.codes.len()
    }
}

impl Branchable for Context {
    fn new_branch(&self) -> ConditionalBranch {
        ConditionalBranch::new(self.byte_len())
    }

    fn accept(&mut self, branch: impl Branch) -> Result<(), ()> {
        if !branch.is_finalized() {
            return Err(());
        }

        self.codes.extend(branch.eject());
        Ok(())
    }
}

impl Context {
    pub fn new() -> Self {
        Context {
            codes: Vec::new(),
        }
    }
}



#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn context_binary() {
        let mut ctx = Context::new();

        ctx.emit_op(OpCode::ILoad);
        ctx.emit_op(OpCode::ILoad);
        ctx.emit_op(OpCode::Add);

        assert_eq!(vec![113, 113, 49], ctx.compile());
    }

    fn context_branch_binary() {
        let mut ctx = Context::new();
        let mut branch = ctx.new_branch();

        branch.mode_if();
            branch.emit_op(OpCode::ILoad);
            branch.emit_op(OpCode::ILoad);
            branch.emit_op(OpCode::ILoad);
        branch.mode_else();
            branch.emit_op(OpCode::SLoad);
            branch.emit_op(OpCode::SLoad);
            branch.emit_op(OpCode::SLoad);
        branch.finalize();

        ctx.accept(branch);
    }
}
