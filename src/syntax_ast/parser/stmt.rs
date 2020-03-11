
use super::Parser;
use trace::position::CodeSpan;
use crate::tokenizer::token::{ TokenType, Token };
use crate::ast::{BlockData, Statement, Stmt, StmtId};

impl<'m> Parser<'m> {
    pub(super) fn statement(&'m mut self) -> Result<StmtId, ()> {
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
                let statement: Statement;

                let start_span = self.get_current().span;
                let expr = self.expression()?;

                statement.module = self.module;
                statement.span = CodeSpan::combine(&start_span, &self.get_current().span);
                statement.data = Stmt::Expression(expr);

                Ok(self.ast.add_stmt(statement))
            }
        };

        self.consume(TokenType::SemiColon)?;
        result
    }

    fn parse_print_stmt(&'m mut self) -> Result<StmtId, ()> {
        let tok = self.consume(TokenType::Print)?;
        let expr = self.expression()?;
        let id = self.ast.add_stmt(Stmt::Print(expr).complete(tok));
        Ok(id)
    }

    fn parse_break_stmt(&'m mut self) -> Result<StmtId, ()> {
        let tok = self.consume(TokenType::Break)?;
        Ok(self.ast.add_stmt(Stmt::Break.complete(tok)))
    }

    fn parse_continue_stmt(&'m mut self) -> Result<StmtId, ()> {
        let tok = self.consume(TokenType::Continue)?;
        Ok(self.ast.add_stmt(Stmt::Continue.complete(tok)))
    }

    fn parse_return_stmt(&'m mut self) -> Result<StmtId, ()> {
        let start = self.consume(TokenType::Return)?.span;
        let mut result = None;
        if !self.is(TokenType::SemiColon) {
            result = Some(self.expression()?);
        }

        let Token{ file, span: end, .. } = self.get_current();
        let stmt = Statement {
            module: file,
            span: CodeSpan::combine(&start, &end),
            data: Stmt::Return(result)
        };

        Ok(self.ast.add_stmt(stmt))
    }

    fn if_statement(&'m mut self) -> Result<StmtId, ()> {
        let start = self.consume(TokenType::If)?.span;
        let condition = self.expression()?;
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

    fn while_statement(&'m mut self) -> Result<StmtId, ()> {
        let start = self.consume(TokenType::While)?.span;
        let condition = self.expression()?;
        let whileloop = self.statement()?;
        let end = self.get_current().span;

        let stmt = Statement {
            module: self.module,
            span: CodeSpan::combine(&start, &end),
            data: Stmt::While(condition, whileloop),
        };

        Ok(self.ast.add_stmt(stmt))
    }

    pub(super) fn block_statement(&'m mut self) -> Result<StmtId, ()> {
        let start_span = self.consume(TokenType::OpenBrace)?.span;
        let mut vector = Vec::new();

        fn parse_inside_block<'t>(parser: &'t mut Parser<'t>, vector: &mut Vec<StmtId>) -> Result<(), ()> {
            while !parser.is_at_end() && !parser.is(TokenType::CloseBrace) {
                vector.push(parser.declaration()?);
            }
            Ok(())
        };

        let assign_count = self.enter_block::<Vec<StmtId>>(&mut vector, parse_inside_block);
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
