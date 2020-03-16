/*
 * タイプ及び定義をハンドリングする
 * */

use super::Parser;

use crate::ast;
use crate::tokenizer::token::{Token, TokenType};

use trace::err_fatal;
use trace::position::CodeSpan;
use trace::source::SourceFile;

fn is_prefix_banned(cand: &str) -> bool
{
    match cand
    {
        "struct" => true,
        _ => false,
    }
}

fn is_suffix_banned(cand: &str) -> bool
{
    match cand
    {
        "struct" => true,
        _ => false,
    }
}

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
                return Ok(ast::ParsedType::pArray(Box::new(inner_type), Some(length)));
            }
            else
            {
                self.consume(TokenType::CloseSquareBracket)?;
                return Ok(ast::ParsedType::pArray(Box::new(inner_type), None));
            }
        }

        // TODO - @Broken: are you sure that there's no more prefix??
        let mut core_type = if self.is(TokenType::Iden)
        {
            let token = self.consume(TokenType::Iden)?;

            let candidate = token.lexeme.as_ref().unwrap();
            if is_prefix_banned(candidate) && !prefix.is_empty()
            {
                token.report(
                    "Invalid Prefix placement",
                    &format!("Prefix {:?} はこの定義では使用できません。", prefix),
                );
                return Err(());
            }

            if candidate == "struct"
            {
                token.report("Unimplemented", "Structはまだ未実装です。");
                return Err(());
                // return self.parse_struct(prefix);
            }

            match ast::ParsedType::match_primitive(candidate)
            {
                Some(x) => x,
                None => ast::ParsedType::pUserdef(candidate.to_string()),
            }
        }
        else
        {
            ast::ParsedType::pUnknown
        };
        loop
        {
            match self.get_current().tokentype
            {
                TokenType::Caret =>
                {
                    self.advance();
                    core_type = ast::ParsedType::pPointer(Box::new(core_type));
                }
                TokenType::Question =>
                {
                    self.advance();
                    core_type = ast::ParsedType::pOptional(Box::new(core_type));
                }
                _ => break,
            }
        }
        Ok(core_type)
    }

    fn parse_struct(&mut self, _prefix: &ast::DeclPrefix) -> Result<ast::ParsedType, ()>
    {
        Err(())
    }

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
                    prefix.insert(ast::DeclPrefix::Const);
                }
                TokenType::Public =>
                {
                    self.advance();
                    prefix.insert(ast::DeclPrefix::Public);
                }
                _ => break,
            }
        }
        prefix
    }

    pub(super) fn parse_function_decl(
        &mut self,
        baseinfo: ast::DeclarationData,
    ) -> Result<ast::StmtId, ()>
    {
        let start_span = self.get_current().span;
        let args = self.parse_arguments()?;
        let func = self.block_statement()?;
        let end_span = self.get_current().span;

        let data = ast::FunctionData {
            it: baseinfo,
            args,
            block_id: func,
        };

        let idx = self.ast.add_fn(data);
        let stmt = ast::Statement {
            module: self.module,
            span: CodeSpan::combine(&start_span, &end_span),
            data: ast::Stmt::Function(idx),
            parent: None,
        };

        Ok(self.ast.add_stmt(stmt))
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
            let name = self.consume(TokenType::Iden)?.lexeme.to_owned().unwrap();
            self.consume(TokenType::Colon)?;

            let prefix = self.parse_type_prefix();
            let dectype = self.parse_type(&prefix)?;

            let decl_info = ast::DeclarationData {
                kind: ast::DeclKind::Argument,
                name,
                dectype,
                prefix,
                expr: None,
            };

            if self.is(TokenType::Equal)
            {
                self.get_current()
                    .report("Unimplemented", "デフォルト引数は未実装です。");
                return Err(());
            }
            else if self.is(TokenType::Comma)
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

        let prefix = self.parse_type_prefix();
        let dectype = self.parse_type(&prefix)?;

        let mut decl_info = ast::DeclarationData {
            kind: ast::DeclKind::Variable,
            name,
            dectype,
            prefix,
            expr: None,
        };
        let mut statement: ast::StmtInit = Default::default();
        statement.module = Some(self.module);

        if self.is(TokenType::Equal)
        {
            self.advance();
            let item = self.expression()?;
            decl_info.expr = Some(self.ast.add_expr(item));
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
        else if self.is(TokenType::OpenParen)
        {
            self.parse_function_decl(decl_info)
        }
        else
        {
            self.get_current()
                .report("Invalid Token", "'=', ';', '(' のうち一つを期待しました.");
            Err(())
        }
    }
}
