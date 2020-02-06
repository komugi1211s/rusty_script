use super::ir::{ IRCode, print_ir_vec };
use syntax_ast::ast::{ Operator };

pub struct Context {
    codes: Vec<IRCode>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BranchMode {
    Empty,
    If,
    Else,
    While,
    // For,
}

impl Default for BranchMode {
    fn default() -> Self { Self::Empty }
}

#[derive(Debug, Clone, Default)]
pub struct ConditionalBranch {
    mode: BranchMode,
    prev_mode: BranchMode,
    finalized: bool,

    // used when "finalize" gets called.
    // contains offset from parent branch / context.
    offset: usize,

    // used when finalized.
    // other true_code / false_code vector must get
    // emptied when it's finalized.
    finalized_code: Vec<IRCode>,

    // used when branch_mode is "If" "While" "For".
    // everything that's evaluated when initialized expression is true
    // goes into this vector.
    true_code : Vec<IRCode>,

    // used when branch_mode is "Else".
    // everything that's evaluated when initialized expression is false
    // goes into this vector.
    false_code: Vec<IRCode>,

    // used when branch_mode is While / For.
    break_call    : Vec<usize>,
    continue_call : Vec<usize>,

}

impl ConditionalBranch {
    fn new(addr: usize) -> Self {
        Self {
            offset: addr,
            .. Default::default()
        }
    }

    fn mode_if(&mut self) -> Result<(), ()> {
        if self.mode != BranchMode::Empty {
            return Err(())
        }

        self.set_mode(BranchMode::If)
    }

    fn mode_while(&mut self) -> Result<(), ()> {
        if self.mode != BranchMode::Empty {
            return Err(())
        }

        self.set_mode(BranchMode::While)
    }

    /*
    Unsupported for now.

    fn mode_for(&mut self) -> Result<(), ()> {
        if self.mode != BranchMode::Empty {
            return Err(())
        }

        self.set_mode(BranchMode::For)
    }
    */

    fn mode_else(&mut self) -> Result<(), ()> {
        if self.mode == BranchMode::Else {
            return Err(())
        }
        self.set_mode(BranchMode::Else)
    }
}

impl CodeGen for ConditionalBranch {
    fn new_branch(&self) -> ConditionalBranch {
        Self::new(self.len() + self.offset)
    }

    fn accept(&mut self, branch: impl Branch) -> Result<(), ()> {
        if self.is_finalized() || !branch.is_finalized() {
            return Err(());
        }
        if self.mode == BranchMode::Else {
            self.false_code.extend(branch.eject());
        } else {
            self.true_code.extend(branch.eject());
        }
        Ok(())
    }

    fn emit_op(&mut self, code: IRCode) -> Result<(), ()> {
        if self.is_finalized() || self.mode == BranchMode::Empty {
            Err(())
        } else {
            if self.mode == BranchMode::Else {
                self.false_code.push(code);
            } else {
                self.true_code.push(code);
            }
            Ok(())
        }
    }

    fn eject(&self) -> Vec<IRCode> {
        self.finalized_code.clone()
    }

    fn len(&self) -> usize {
        /* Depends on the mode!! */
        unimplemented!()
    }
}

impl Branch for ConditionalBranch {
    fn set_mode(&mut self, mode: BranchMode) -> Result<(), ()> {
        if self.is_finalized() {
            return Err(());
        }

        self.prev_mode = self.mode;
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

        use BranchMode::*;
        match (self.prev_mode, self.mode) {
            (_,  If)       => { 
                assert!(self.false_code.len() == 0);
                
                let jump_padding = 1;
                let length_true  = (self.offset + self.true_code.len()) as u32;
                self.finalized_code.push(IRCode::JNT(length_true + jump_padding));
                self.finalized_code.extend(self.true_code.drain(..));
            },
            (If, Else)     => { 
                let length_true  = (self.offset + self.true_code.len()) as u32;
                let length_false = (self.offset + self.false_code.len()) as u32;

                let jump_padding = 2;
                self.finalized_code.push(IRCode::JNT(length_true + jump_padding));
                self.finalized_code.extend(self.true_code.drain(..));
                self.finalized_code.push(IRCode::Jump(length_true + length_false + jump_padding));

                self.finalized_code.extend(self.false_code.drain(..));
            },

            (_, While)     => { 
                assert!(self.false_code.len() == 0);
                let length_true = (self.offset + self.true_code.len()) as u32;

                self.finalized_code.push(IRCode::JNT(length_true + 1));
                self.finalized_code.extend(self.true_code.drain(..));
                self.finalized_code.push(IRCode::Jump(self.offset as u32));
            },
            (While, Else)  => { 
                let length_true  = (self.offset + self.true_code.len()) as u32;
                let length_false = (self.offset + self.false_code.len()) as u32;

                self.finalized_code.push(IRCode::JNT(length_true + 1));
                self.finalized_code.extend(self.true_code.drain(..));
                self.finalized_code.push(IRCode::Jump(self.offset as u32));

                self.finalized_code.extend(self.false_code.drain(..));
            },

            // For is unsupported.
            /*
            (_, For)       => { /* For Code goes here. */ },
            (For,   Else)  => { /* For Else Code goes here. */ },
            */
            _ => (),
        }
        self.finalized = true;
        Ok(())
    }
}


pub trait Branch: CodeGen {
    fn set_mode(&mut self, mode: BranchMode) -> Result<(), ()>;
    fn is_finalized(&self) -> bool;
    fn finalize(&mut self) -> Result<(), ()>;
}

pub trait CodeGen {
    fn eject(&self) -> Vec<IRCode>;
    fn len(&self) -> usize;

    fn new_branch(&self) -> ConditionalBranch;
    fn accept(&mut self, _: impl Branch) -> Result<(), ()>;

    fn emit_op(&mut self, _: IRCode) -> Result<(), ()>;
    fn emit_from_oper(&mut self, oper: Operator) -> Result<(), ()> {
        if !oper.is_arithmetic() {
            return Err(());
        }

        let code = match oper {
            Operator::Add    => IRCode::Add,
            Operator::Sub    => IRCode::Sub, 
            Operator::Div    => IRCode::Div, 
            Operator::Mul    => IRCode::Mul,
            Operator::Mod    => IRCode::Mod, 
            Operator::Neg    => IRCode::Neg,
	    Operator::EqEq   => IRCode::EqEq,
            Operator::NotEq  => IRCode::NotEq,
            Operator::LessEq => IRCode::LessEq, 
            Operator::MoreEq => IRCode::MoreEq, 
            Operator::Less   => IRCode::Less, 
            Operator::More   => IRCode::More,
            Operator::And    => IRCode::And,
            Operator::Or     => IRCode::Or,
            Operator::Not    => IRCode::Not,
            _      => unreachable!(),
        };

        self.emit_op(code)
    }

}

impl CodeGen for Context {
    fn new_branch(&self) -> ConditionalBranch {
        ConditionalBranch::new(self.len())
    }

    fn accept(&mut self, branch: impl Branch) -> Result<(), ()> {
        if !branch.is_finalized() {
            return Err(());
        }

        self.codes.extend(branch.eject());
        Ok(())
    }

    fn emit_op(&mut self, code: IRCode) -> Result<(), ()> {
        self.codes.push(code);
        Ok(())
    }

    fn eject(&self) -> Vec<IRCode> {
        self.codes.clone()
    }

    fn len(&self) -> usize {
        self.codes.len()
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
    use crate::vm::VirtualMachine;

    #[test]
    fn context_binary() {
        let mut ctx = Context::new();

        ctx.emit_op(IRCode::ILoad(0));
        ctx.emit_op(IRCode::ILoad(1));
        ctx.emit_op(IRCode::Add);
        ctx.emit_op(IRCode::DebugPrint);
    }

    #[test]
    fn context_branch_binary() {
        let mut ctx = Context::new();
        let mut branch = ctx.new_branch();

        branch.mode_if();
            branch.emit_op(IRCode::ILoad(0));
            branch.emit_op(IRCode::ILoad(1));
            branch.emit_op(IRCode::Add);
            branch.emit_op(IRCode::DebugPrint);
        branch.mode_else();
            branch.emit_op(IRCode::ILoad(0));
            branch.emit_op(IRCode::ILoad(1));
            branch.emit_op(IRCode::Add);
            branch.emit_op(IRCode::DebugPrint);
        branch.finalize();

        ctx.accept(branch);

        let vec = ctx.eject();
        print_ir_vec(&vec);
    }
}
