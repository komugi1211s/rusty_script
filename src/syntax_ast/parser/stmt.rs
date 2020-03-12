
use super::Parser;
use trace::position::CodeSpan;
use crate::tokenizer::token::{ TokenType, Token };
use crate::ast::{BlockData, Statement, Stmt, StmtId};

impl<'m> Parser<'m> {
    pub(super) fn statement(&mut self) -> Result<StmtId, ()> {
        let possible_stmt = self.get_current().tokentype.clone();
        let result = match possible_stmt {
            // Close Bracket Expected, They'll handle the close bracket themselves
            // so no need for check
            TokenType::If => return self.if_statement(),
            TokenType::While => return self.while_statement(),
            TokenType::OpenBrace => return self.block_statement(),

            // Semicolon Expected, I have to handle it here
            // Go through, no early return
            TokenType::Print => self.parse_print_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::Break => self.parse_break_stmt(),
            TokenType::Continue => self.parse_continue_stmt(),
            _ => {

                let start_span = self.get_current().span;
                let expr = self.expression()?;
                let expr_id = self.ast.add_expr(expr);

                let statement = Statement {
                    module: self.module,
                    span: CodeSpan::combine(&start_span, &self.get_current().span),
                    data: Stmt::Expression(expr_id),
                };

                Ok(self.ast.add_stmt(statement))
            }
        };

        self.consume(TokenType::SemiColon)?;
        result
    }

    fn parse_print_stmt(&mut self) -> Result<StmtId, ()> {

        let stmt = Statement {
            module: self.module,
            span: self.consume(TokenType::Print)?.span,
            data: Stmt::Print({
                let expr = self.expression()?;
                self.ast.add_expr(expr)
            }),
        };

        let id = self.ast.add_stmt(stmt);
        Ok(id)
    }

    fn parse_break_stmt(&mut self) -> Result<StmtId, ()> {
        let stmt = Statement {
            module: self.module,
            span: self.consume(TokenType::Break)?.span,
            data: Stmt::Break,
        };


        Ok(self.ast.add_stmt(stmt))
    }

    fn parse_continue_stmt(&mut self) -> Result<StmtId, ()> {
        let stmt = Statement {
            module: self.module,
            span: self.consume(TokenType::Continue)?.span,
            data: Stmt::Continue,
        };


        Ok(self.ast.add_stmt(stmt))
    }

    fn parse_return_stmt(&mut self) -> Result<StmtId, ()> {
        let start = self.consume(TokenType::Return)?.span;
        let mut result = None;
        if !self.is(TokenType::SemiColon) {
            let expr = self.expression()?;
            result = Some(self.ast.add_expr(expr));
        }

        let end = self.get_current().span;
        let stmt = Statement {
            module: self.module,
            span: CodeSpan::combine(&start, &end),
            data: Stmt::Return(result)
        };

        Ok(self.ast.add_stmt(stmt))
    }

    fn if_statement(&mut self) -> Result<StmtId, ()> {
        let start = self.consume(TokenType::If)?.span;
        let condition = {
            let expr = self.expression()?;
            self.ast.add_expr(expr)
        };
        let true_route = self.statement()?;
        let false_route = if self.is(TokenType::Else) {
            self.advance();
            Some(self.statement()?)
        } else {
            None
        };
        let end = self.get_current().span;

        let stmt = Statement {
            module: self.module,
            span: CodeSpan::combine(&start, &end),
            data: Stmt::If(condition, true_route, false_route)
        };

        Ok(self.ast.add_stmt(stmt))
    }

    fn while_statement(&mut self) -> Result<StmtId, ()> {
        let start = self.consume(TokenType::While)?.span;
        let condition = {
            let expr = self.expression()?;
            self.ast.add_expr(expr)
        };
        let whileloop = self.statement()?;
        let end = self.get_current().span;

        let stmt = Statement {
            module: self.module,
            span: CodeSpan::combine(&start, &end),
            data: Stmt::While(condition, whileloop),
        };

        Ok(self.ast.add_stmt(stmt))
    }

    pub(super) fn block_statement(&mut self) -> Result<StmtId, ()> {
        let start_span = self.consume(TokenType::OpenBrace)?.span;
        let mut vector = Vec::new();

        let previous_count = self.assign_count;
        self.assign_count = 0;
        self.block_count += 1;

        while !self.is_at_end() && !self.is(TokenType::CloseBrace) {
            vector.push(self.declaration()?);
        }

        let assign_count = self.assign_count;
        self.assign_count = previous_count;
        self.block_count -= 1;

        let end_span = self.consume(TokenType::CloseBrace)?.span;
        let block = Stmt::Block(BlockData {
            statements: vector,
            local_count: assign_count,
        });

        let stmt = Statement {
            module: self.module,
            span: CodeSpan::combine(&start_span, &end_span),
            data: block,
        };

        Ok(self.ast.add_stmt(stmt))
    }
}
