use super::{BytecodeGenerator, OpCode};
use trace::position::{CodeSpan, EMPTY_SPAN};

use syntax_ast::{ast::*, tokenizer::token::TokenType};

// use super::expr::ExpressionHandleResult;
use types::{Type}; // , TypeKind};


/*
 * TODO: more useful™ Instruction System
 * // simple
 * self.code.op_single(a);
 *
 * // operand simple
 * self.code.op_with_operand(a, b);
 *
 * // Reserve
 * let address = self.code.reserve_single();
 * self.code.apply_single(address, OpCode);
 *
 * // Self handle
 * let address = self.code.reserve_sizeof(OpCode::Call);
 * let instruction = BC::code(OpCode::Call).operand(abc).put_to(address);
 *
 * self.code.apply(instruction);
 * */

#[derive(Debug)]
pub struct StatementHandleResult {
    pub index: usize,
    pub span: CodeSpan,
    pub info: StatementInfo,
}

#[derive(Debug)]
pub enum StatementInfo {
    Nothing,
    Break(usize),
    Continue(usize),
    Declaration { is_initialized: bool, def: (usize, bool /* is_global */) },
    Return(Option<Type>),
}

impl BytecodeGenerator {
    pub(super) fn handle_stmt(
        &mut self,
        ast: &ParsedResult,
        data: &StmtId,
        span: CodeSpan,
    ) -> StatementHandleResult {
        let mut handled_result = StatementHandleResult {
            span,
            index: 0,
            info: StatementInfo::Nothing,
        };

        let data = ast.get_stmt(*data);
        match data {
            Statement::Expression(expr) => {
                handled_result.index = self.handle_expr(ast, expr, span).index;
                handled_result
            }
            Statement::Decralation(declaration_info) => {
                self.handle_declaration_data(ast, &mut handled_result, declaration_info);
                handled_result
            }

            Statement::Print(expr) => {
                self.handle_expr(ast, expr, span);
                handled_result.index = self.code.push_opcode(OpCode::DebugPrint, span);
                handled_result
            }

            Statement::If(expr, if_id, opt_else_id) => {
                self.handle_if_statement(ast, expr, if_id, opt_else_id.as_ref(), span)
            }

            Statement::Block(block_data) => {
                // self.code.push_opcode(OpCode::BlockIn, span);
                self.enter_local();
                for i in block_data.statements.iter() {
                    match self.handle_stmt(ast, i, span).info {
                        StatementInfo::Break(usize) => {
                            self.break_call.push(usize);
                        }
                        StatementInfo::Continue(..) => panic!("Continue is not supported"),
                        _ => (),
                    };
                }
                self.leave_local();
                // handled_result.index = self.code.push_opcode(OpCode::BlockOut, span);
                handled_result.index = self.code.current_length();
                handled_result
            }

            Statement::While(expr, while_id) => {
                self.handle_while_statement(ast, expr, while_id, span)
            }

            Statement::Break => {
                if self.last_loop_start != usize::max_value() {
                    let break_index = self.code.push_opcode(OpCode::Jump, span);
                    self.code
                        .push_operands(usize::max_value().to_ne_bytes().to_vec(), span);
                    handled_result.info = StatementInfo::Break(break_index);
                }
                handled_result.index = self.code.current_length();
                handled_result
            }

            Statement::Continue => {
                if self.last_loop_start != usize::max_value() {
                    let continue_index = self.code.push_opcode(OpCode::Jump, span);
                    self.code
                        .push_operands(self.last_loop_start.to_ne_bytes().to_vec(), span);
                    handled_result.info = StatementInfo::Continue(continue_index);
                }
                handled_result.index = self.code.current_length();
                handled_result
            }
            Statement::Function(func_data) => {
                // self.handle_function_data(&mut handled_result, func_data, span);
                handled_result
            }
            Statement::Return(opt_expr) => {
                let result_type = if let Some(expr) = opt_expr {
                    let result_expr = self.handle_expr(ast, expr, span);
                    Some(result_expr._type)
                } else {
                    self.code.write_null(span);
                    None
                };
                handled_result.index = self.code.push_opcode(OpCode::Return, span);
                handled_result.info = StatementInfo::Return(result_type);
                handled_result
            }
            _ => unreachable!(),
        }
    }

    fn handle_if_statement(
        &mut self,
        ast: &ParsedResult,
        expr: &ExprId,
        if_id: &StmtId,
        opt_else_id: Option<&StmtId>,
        span: CodeSpan,
    ) -> StatementHandleResult {
        let mut handled_result = StatementHandleResult {
            span,
            index: 0,
            info: StatementInfo::Nothing,
        };
        self.handle_expr(ast, expr, span);

        // ジャンプ用のインデックスを作っておく
        let jump_opcode_index = self.code.push_opcode(OpCode::JNT, span);
        self.code
            .push_operands(usize::max_value().to_ne_bytes().to_vec(), span);

        // Ifの終わりにまでJumpする為のIndexが要る
        let end_of_if_block = self.handle_stmt(ast, if_id, span).index;

        // jump opcodeがある位置のオペランドを、Ifブロックの終了アドレスで上書き
        self.code
            .rewrite_operands(end_of_if_block.to_ne_bytes().to_vec(), jump_opcode_index);

        if let Some(else_id) = opt_else_id {
            //
            // Elseがあるので、Elseを避けるためのJump命令をIf命令の最後に叩き込む
            let jump_block = self.code.push_opcode(OpCode::Jump, span);
            let after_operand = self
                .code
                .push_operands(usize::max_value().to_ne_bytes().to_vec(), span);

            // Ifブロックの終了アドレスがあった部分を、Else避けJump分を加味して調整
            self.code
                .rewrite_operands(after_operand.to_ne_bytes().to_vec(), jump_opcode_index);

            // Ifブロックが丁度終わる位置のJumpオペランドを、Elseブロックの終了アドレスで上書き
            let end_of_else_block = self.handle_stmt(ast, else_id, span).index;
            self.code
                .rewrite_operands(end_of_else_block.to_ne_bytes().to_vec(), jump_block);
        }

        handled_result.index = self.code.current_length();
        handled_result
    }

    fn handle_while_statement(
        &mut self,
        ast: &ParsedResult,
        expr: &ExprId,
        while_id: &StmtId,
        span: CodeSpan,
    ) -> StatementHandleResult {
        /*
         どうやらアセンブラではWhileループは以下のように書かれるらしい:

         Jump to Loop1
         InsideLoop:
           Loop Inside
           Done, goes through to loop1

         loop1:
           Check if condition is true
           when True, Jump to InsideLoop

         なので、取り敢えずExprを判定する前に先にWhileBlockを処理しないとならない
         Breakはloop1の後に、ContinueはInsideLoopの頭にジャンプする命令になる
         https://stackoverflow.com/questions/28665528/while-do-while-for-loops-in-assembly-language-emu8086
        */

        let mut handled_result = StatementHandleResult {
            span,
            index: 0,
            info: StatementInfo::Nothing,
        };
        let _before_loop = self.code.current_length();
        let jump_conditional = self.code.push_opcode(OpCode::Jump, span);
        let after_jump_conditional = self
            .code
            .push_operands(usize::max_value().to_ne_bytes().to_vec(), span);

        // この時点でContinue命令は after_jump_conditional に飛ぶようになる
        let previous_loop_start = self.last_loop_start;
        self.last_loop_start = after_jump_conditional;

        self.handle_stmt(ast, while_id, span);

        let before_expr = self.code.current_length();
        self.handle_expr(ast, expr, span);
        self.code.push_opcode(OpCode::JT, span);
        let end_of_loop = self
            .code
            .push_operands(after_jump_conditional.to_ne_bytes().to_vec(), span);

        for i in self.break_call.drain(..) {
            self.code
                .rewrite_operands(end_of_loop.to_ne_bytes().to_vec(), i);
        }
        self.code
            .rewrite_operands(before_expr.to_ne_bytes().to_vec(), jump_conditional);
        self.last_loop_start = previous_loop_start;
        handled_result.index = end_of_loop;
        handled_result
    }
}
