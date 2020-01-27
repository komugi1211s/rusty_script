use trace::Error;

use super::ast::{
    Statement, StmtId, BlockData
};

use crate::tokenizer::token::{TokenType};


use super::Parser;

impl<'m, 't> Parser<'m, 't> {
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
                let expr = self.expression()?;
                Ok(self.ast.add_stmt(Statement::Expression(expr)))
            }
        };

        self.consume(TokenType::SemiColon)?;
        result
    }

    fn parse_print_stmt(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::Print)?;
        let expr = self.expression()?;
        let id = self.ast.add_stmt(Statement::Print(expr));
        Ok(id)
    }

    fn parse_break_stmt(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::Break)?;
        Ok(self.ast.add_stmt(Statement::Break))
    }

    fn parse_continue_stmt(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::Continue)?;
        Ok(self.ast.add_stmt(Statement::Continue))
    }

    fn parse_return_stmt(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::Return)?;
        let mut result = None;
        if !self.is(TokenType::SemiColon) {
            result = Some(self.expression()?);
        }

        let stmt = Statement::Return(result);

        Ok(self.ast.add_stmt(stmt))
    }

    fn if_statement(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::If)?;
        let condition = self.expression()?;
        let true_route = self.statement()?;
        let false_route = if self.is(TokenType::Else) {
            self.advance();
            Some(self.statement()?)
        } else {
            None
        };

        let stmt = Statement::If(condition, true_route, false_route);

        Ok(self.ast.add_stmt(stmt))
    }

    fn while_statement(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::While)?;
        let condition = self.expression()?;
        let whileloop = self.statement()?;
        let stmt = Statement::While(condition, whileloop);

        Ok(self.ast.add_stmt(stmt))
    }

    pub(super) fn block_statement(&mut self) -> Result<StmtId, ()> {
        self.consume(TokenType::OpenBrace)?;
        let mut vector = Vec::new();

        fn parse_inside_block(parser: &mut Parser, vector: &mut Vec<StmtId>) -> Result<(), ()> {
            while !parser.is_at_end() && !parser.is(TokenType::CloseBrace) {
                vector.push(parser.declaration()?);
            }
            Ok(())
        };

        let assign_count = self.enter_block::<Vec<StmtId>>(&mut vector, parse_inside_block);

        self.consume(TokenType::CloseBrace)?;

        let block = BlockData {
            statements: vector,
            local_count: assign_count,
        };

        Ok(self.ast.add_stmt(Statement::Block(block)))
    }
}
