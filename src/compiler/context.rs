use super::opcode::{ OpCode };
use syntax_ast::ast::{ Operator };

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    code: OpCode,
    operand: Option<usize>,
    _c: u8,
}

impl Instruction {
    fn simple(code: OpCode) -> Self {
        Self { 
            code,
            operand: None,
            _c: 0,
        }
    }

    fn operand(code: OpCode, oper: usize) -> Self {
        Self {
            code,
            operand: Some(oper),
            _c: 0,
        }
    }
}

impl Iterator for Instruction {
    type Item=u8;

    fn next(&mut self) -> Option<Self::Item> {

    }
}

pub struct Context {
    codes: Vec<Instruction>,
}

pub struct ConditionalBranch<'a> {
    mode: BranchMode,
    code: Vec<Instruction>,

    finalized: bool,
    start_address: usize,
    parent_cond: Option<&'a ConditionalBranch<'a>>,
    // parent_func: Option<&'a FunctionContext>,
}

enum BranchMode {
    Empty,
    If,
    Else,
    While,
    For,
    Finalized,
}

pub trait Branchable: CodeGen {
    fn new_branch(&self) -> ConditionalBranch;
    fn commit(&mut self, _: ConditionalBranch) -> Result<(), ()>;
}

pub trait CodeGen {
    fn emit_op(&mut self, _: OpCode) -> Result<(), ()>;
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
    fn emit_op(&mut self, code: OpCode) -> Result<(), ()> {
        self.codes.push(code);
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
    #[test]
    fn context_binary() {
        let ctx = Context::new();

        ctx.emit_op(OpCode::Const64);
    }
}
