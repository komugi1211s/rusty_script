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
    start_byte_offset: usize,

    // used when finalized.
    // other true_code / false_code vector must get
    // emptied when it's finalized.
    finalized_code: Vec<Instruction>,

    // used when branch_mode is "If" "While" "For".
    // everything that's evaluated when initialized expression is true
    // goes into this vector.
    true_code : Vec<Instruction>,

    // used when branch_mode is "Else".
    // everything that's evaluated when initialized expression is false
    // goes into this vector.
    false_code: Vec<Instruction>,

    // used when branch_mode is While / For.
    break_call    : Vec<usize>,
    continue_call : Vec<usize>,

}

impl ConditionalBranch {
    fn new(addr: usize) -> Self {
        Self {
            start_byte_offset: addr,
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
        Self::new(self.byte_len() + self.start_byte_offset)
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

    fn emit_instruction(&mut self, inst: Instruction) -> Result<(), ()> {
        if self.is_finalized() || self.mode == BranchMode::Empty {
            Err(())
        } else {
            if self.mode == BranchMode::Else {
                self.false_code.push(inst);
            } else {
                self.true_code.push(inst);
            }
            Ok(())
        }
    }

    fn emit_op(&mut self, code: OpCode) -> Result<(), ()> {
        if self.is_finalized() || self.mode == BranchMode::Empty {
            Err(())
        } else {
            if self.mode == BranchMode::Else {
                self.false_code.push(code.into());
            } else {
                self.true_code.push(code.into());
            }
            Ok(())
        }
    }

    fn eject(&self) -> Vec<Instruction> {
        self.finalized_code.clone()
    }

    fn compile(&self) -> Vec<u8> {
        self.finalized_code.clone().into_iter().flatten().collect()
    }

    fn byte_len(&self) -> usize {
        // Byte length is calculated as follows:
        //
        // Code is "if(expr) {}" 
        //      - 9(1 jump opcode + 8 operand, JT jump) + true_code bytelength.

        // Code is "if(expr) {} else {}" 
        //      - 9 (1 jump opcode + 8 operand, conditional JT jump) 
        //      + true_code bytelength
        //      + 9 (1 jump opcode + 8 operand, end of if)
        //      + false_code bytelength
        
        //  Code is "while(expr) {}"
        //      - 9 (1 jump opcode + 8 operand, conditional JNT jump)
        //      + true_code bytelength
        //      + 9 (1 jump opcode + 8 operand, re-evaluate conditional)
        
        //  Code is "while(expr) {} else {}"
        //      - 9 (1 jump opcode + 8 operand, conditional JNT jump)
        //      + true_code bytelength
        //      + 9 (1 jump opcode + 8 operand, re-evaluate conditional)
        //      + false_code bytelength

        // padding for jump opcode + usize operand.
        let JUMP_PADDING: usize = 9;
        let true_path_length: usize = self.true_code.iter().map(|&x| x.code.len()).sum();
        let false_path_length: usize = self.false_code.iter().map(|&x| x.code.len()).sum();

        match (self.prev_mode, self.mode) {
            (_, If)       => JUMP_PADDING + true_path_length,
            (If, Else)    => JUMP_PADDING + true_path_length + JUMP_PADDING + false_path_length,
            (_, While)    => JUMP_PADDING + true_path_length + JUMP_PADDING,
            (While, Else) => JUMP_PADDING + true_path_length + JUMP_PADDING + false_path_length, 
        }
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

        // Adjust and patch addresses.
        // FIXME - @Bug: is it really required ??????????????
        // what you patch??? is there something that is relative to an address??
        // Is there something that needs to be absolute???
        //
        // 
        for inst in self.true_code.iter_mut() {
            if let Some(ref mut x) = inst.operand {
                let v = usize::from_ne_bytes(*x);
                *x = (v + self.start_byte_offset).to_ne_bytes();
            }
        }

        for inst in self.false_code.iter_mut() {
            if let Some(ref mut x) = inst.operand {
                let v = usize::from_ne_bytes(*x);
                *x = (v + self.start_byte_offset).to_ne_bytes();
            }
        }

        use BranchMode::*;
        match (self.prev_mode, self.mode) {
            (_,  If)       => { /* If Code goes here. */ },
            (If, Else)     => { /* If Else Code goes here. */ },

            (_, While)     => { /* While Code goes here. */ },
            (While, Else)  => { /* While Else Code goes here. */ },

            // For is unsupported.
            /*
            (_, For)       => { /* For Code goes here. */ },
            (For,   Else)  => { /* For Else Code goes here. */ },
            */
            _ => (),
        }
        self.finalized = true;
        Err(())
    }
}


pub trait Branch: CodeGen {
    fn set_mode(&mut self, mode: BranchMode) -> Result<(), ()>;
    fn is_finalized(&self) -> bool;
    fn finalize(&mut self) -> Result<(), ()>;
}

pub trait CodeGen {
    fn byte_len(&self) -> usize;
    fn eject(&self) -> Vec<Instruction>;
    fn compile(&self) -> Vec<u8>;

    fn new_branch(&self) -> ConditionalBranch;
    fn accept(&mut self, _: impl Branch) -> Result<(), ()>;

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
