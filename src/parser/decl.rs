/*
 * タイプ及び定義をハンドリングする
 * */

use super::Parser;
use std::cell::Cell;

use crate::{
    ast,
    tokenizer::token::{TokenType},
    trace::prelude::*,
};


impl<'m> Parser<'m>
{
    fn parse_type(&mut self, prefix: &ast::DeclPrefix) -> Result<ast::ParsedType, ()>
    {
        // Separate Array Consumption to other function
        if self.is(TokenType::OpenSquareBracket)
        {
            self.advance();
            let inner_type = self.parse_type(prefix)?;
            if self.is(TokenType::SemiColon)
            {
                self.advance();
                // TODO - @Improvement: explicitly parsing digit prevents you from
                // using expression as an Array length
                // e.g. [int; 1 + 2]
                let digit = self.consume(TokenType::Digit)?.clone();
                let number_inside = digit.lexeme.as_ref().unwrap();
                if number_inside.contains('.')
                {
                    digit.report(
                        "Invalid array declaration",
                        "静的配列は現在定数でのみ長さを初期化出来ます。",
                    );
                    return Err(());
                }

                // TODO: do not use unwrap
                let length = number_inside.parse::<u32>().unwrap();
                self.consume(TokenType::CloseSquareBracket)?;
                return Ok(ast::ParsedType::Array(Box::new(inner_type), Some(length)));
            }
            else
            {
                self.consume(TokenType::CloseSquareBracket)?;
                return Ok(ast::ParsedType::Array(Box::new(inner_type), None));
            }
        }

        // TODO - @Broken: are you sure that there's no more prefix??
        let mut core_type = if self.is(TokenType::Iden)
        {
            let token = self.consume(TokenType::Iden)?;
            let candidate = token.lexeme.as_ref().unwrap();

            match ast::ParsedType::match_primitive(candidate)
            {
                Some(x) => x,
                None => ast::ParsedType::Userdef(candidate.to_string()),
            }
        }
        else
        {
            ast::ParsedType::Unknown
        };
        loop
        {
            match self.get_current().tokentype
            {
                TokenType::Caret =>
                {
                    self.advance();
                    core_type = ast::ParsedType::Pointer(Box::new(core_type));
                }
                TokenType::Question =>
                {
                    self.advance();
                    core_type = ast::ParsedType::Optional(Box::new(core_type));
                }
                _ => break,
            }
        }
        Ok(core_type)
    }

    fn parse_struct(&mut self, name: String, decl_span: CodeSpan) -> Result<ast::StmtId, ()>
    {
        self.consume(TokenType::Struct)?;
        self.consume(TokenType::OpenBrace)?;
        self.block_count += 1;

        let mut vector = Vec::new();
        while !self.is_at_end() && !self.is(TokenType::CloseBrace)
        {
            let field_name = self.consume(TokenType::Iden)?.lexeme.clone().unwrap();
            self.consume(TokenType::Colon)?;
            let parsed_type = self.parse_type(&ast::DeclPrefix::empty())?;
            self.consume(TokenType::SemiColon)?;

            let field = ast::Field(field_name, parsed_type);
            vector.push(field);
        }

        self.block_count -= 1;
        let end_span = self.consume(TokenType::CloseBrace)?.span;

        let combined_span = CodeSpan::combine(&decl_span, &end_span);
        let decl_data = ast::DeclarationData {
            kind: ast::DeclKind::Struct,
            name,
            dectype: ast::ParsedType::Struct(vector),
            prefix:  ast::DeclPrefix::empty(),
            expr: None,
            span: combined_span,
        };

        let stmt = ast::Statement {
            module: self.module,
            span: combined_span,
            data: ast::Stmt::Declaration(decl_data),
            parent: None,
        };

        Ok(self.ast.add_stmt(stmt))
    }

    // TODO: what EVEN is prefix?
    fn parse_type_prefix(&mut self) -> ast::DeclPrefix
    {
        let mut prefix = ast::DeclPrefix::empty();
        loop
        {
            match self.get_current().tokentype
            {
                TokenType::Constant =>
                {
                    self.advance();
                    prefix.insert(ast::DeclPrefix::CONST);
                }
                TokenType::Public =>
                {
                    self.advance();
                    prefix.insert(ast::DeclPrefix::PUBLIC);
                }
                _ => break,
            }
        }
        prefix
    }

    pub(super) fn parse_function_decl(
        &mut self,
        name: String,
        decl_span: CodeSpan
    ) -> Result<ast::StmtId, ()>
    {
        self.consume(TokenType::Fn)?;
        let args = self.parse_arguments()?;
        let prefix = self.parse_type_prefix();
        let dectype = self.parse_type(&prefix)?;

        let mut body = Vec::new();
        self.parse_function_body(&mut body)?;

        let end_span = self.get_current().span;

        let parent = ast::StmtId(self.ast.stmt.len() as u32); // Finding like this feels itchy.
        for body_id in body.iter()
        {
            self.ast.set_parent_to_statement(parent, *body_id);
        }

        let data = ast::FunctionData {
            own_stmt: parent,
            it: ast::DeclarationData {
                kind: ast::DeclKind::Variable,
                name,
                dectype,
                prefix,
                expr: None,
                span: decl_span,
            },
            args,
            body,
            implicit_return_required: Cell::new(true),
        };
        let idx = self.ast.add_fn(data);

        let stmt = ast::Statement {
            module: self.module,
            span: CodeSpan::combine(&decl_span, &end_span),
            data: ast::Stmt::Function(idx),
            parent: None,
        };

        Ok(self.ast.add_stmt(stmt))
    }

    fn parse_function_body(&mut self, vec: &mut Vec<ast::StmtId>) -> Result<(), ()>
    {
        self.consume(TokenType::OpenBrace)?;
        self.block_count += 1;

        while !self.is_at_end() && !self.is(TokenType::CloseBrace)
        {
            vec.push(self.declaration()?);
        }

        self.block_count -= 1;
        self.consume(TokenType::CloseBrace)?;
        Ok(())
    }

    pub(super) fn parse_arguments(&mut self) -> Result<Vec<ast::DeclarationData>, ()>
    {
        let _span = self.consume(TokenType::OpenParen)?.span;
        let mut args = vec![];

        if self.is(TokenType::CloseParen)
        {
            self.consume(TokenType::CloseParen)?;
            return Ok(args);
        }

        while self.is(TokenType::Iden)
        {
            self.assign_count += 1;
            let (name, span) = {
                let tok = self.consume(TokenType::Iden)?;
                (tok.lexeme.to_owned().unwrap(), tok.span)
            };

            self.consume(TokenType::Colon)?;

            let prefix = self.parse_type_prefix();
            let dectype = self.parse_type(&prefix)?;

            let decl_info = ast::DeclarationData {
                kind: ast::DeclKind::Variable,
                name,
                dectype,
                prefix,
                expr: None,
                span
            };

            if self.is(TokenType::Equal)
            {
                self.get_current()
                    .report("Unimplemented", "デフォルト引数は未実装です。");
                return Err(());
            }
            if self.is(TokenType::Comma)
            {
                args.push(decl_info);
                self.consume(TokenType::Comma)?;
            }
            else if self.is(TokenType::CloseParen)
            {
                args.push(decl_info);
                break;
            }
            else
            {
                self.get_current().report(
                    "Invalid Token",
                    "`)` を想定しましたが、それ以外のトークンを検知しました。",
                );
                return Err(());
            }
        }

        self.consume(TokenType::CloseParen)?;
        Ok(args)
    }

    pub(super) fn parse_variable(&mut self) -> Result<ast::StmtId, ()>
    {
        self.assign_count += 1;

        let (name, start_span) = {
            let token = self.consume(TokenType::Iden)?;
            (token.lexeme.to_owned().unwrap(), token.span)
        };

        self.consume(TokenType::Colon)?;

        if self.is(TokenType::Fn)     { return self.parse_function_decl(name, start_span); }
        if self.is(TokenType::Struct) { return self.parse_struct(name, start_span);        }

        let prefix = self.parse_type_prefix();
        let dectype = self.parse_type(&prefix)?;

        let mut decl_info = ast::DeclarationData {
            kind: ast::DeclKind::Variable,
            name,
            dectype,
            prefix,
            expr: None,
            span: start_span.clone(),
        };
        let mut statement: ast::StmtInit = Default::default();
        statement.module = Some(self.module);

        if self.is(TokenType::Equal)
        {
            let assign_span = self.advance().span;
            let rvalue_expr = self.expression()?;
            let variable_expr = ast::ExprInit {
                kind: ast::ExprKind::Variable,
                module: Some(self.module),
                span: Some(start_span),
                variable_name: Some(decl_info.name.clone()),
                ..Default::default()
            }.init();

            let assign_expr = ast::ExprInit {
                kind: ast::ExprKind::Assign,
                module: Some(self.module),
                span: Some(assign_span),
                lhs: Some(Box::new(variable_expr)),
                rhs: Some(Box::new(rvalue_expr)),
                end_type: None,
                oper: Some(ast::Operator::Asgn),
                ..Default::default()
            }.init();

            decl_info.expr = Some(self.ast.add_expr(assign_expr));
            let end_span = self.consume(TokenType::SemiColon)?.span;

            statement.span = Some(CodeSpan::combine(&start_span, &end_span));
            statement.data = Some(ast::Stmt::Declaration(decl_info));
            Ok(self.ast.add_stmt(statement.init()))
        }
        else if self.is(TokenType::SemiColon)
        {
            let end_span = self.advance().span;
            let span = CodeSpan::combine(&start_span, &end_span);
            statement.span = Some(span);
            statement.data = Some(ast::Stmt::Declaration(decl_info));
            Ok(self.ast.add_stmt(statement.init()))
        }
        else
        {
            self.get_current()
                .report("Invalid Token", "'=', ';', '(' のうち一つを期待しました.");
            Err(())
        }
    }
}
