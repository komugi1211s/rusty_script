/*
 * タイプ及び定義をハンドリングする
 * */

use super::Parser;

use crate::ast;
use crate::tokenizer::token::{Token, TokenType};

use trace::position::CodeSpan;
use trace::Error;
use trace::{info, warn, err_fatal, err_internal, code_line};

fn is_prefix_banned(cand: &str) -> bool {
    match cand {
        "struct" => true,
        _ => false,
    }
}

fn is_suffix_banned(cand: &str) -> bool {
    match cand {
        "struct" => true,
        _ => false,
    }
}

impl<'m, 't> Parser<'m, 't> {
    fn parse_type(&mut self, prefix: &ast::DeclPrefix) -> Result<ast::ParsedType, ()> {
        // Separate Array Consumption to other function
        if self.is(TokenType::OpenSquareBracket) {
            self.advance();
            let inner_type = self.parse_type(prefix)?;
            if self.is(TokenType::SemiColon) {
                self.advance();
                // TODO - @Improvement: explicitly parsing digit prevents you from
                // using expression as an Array length
                // e.g. [int; 1 + 2]
                let digit = self.consume(TokenType::Digit)?.clone();
                let number_inside = digit.lexeme.as_ref().unwrap();
                if number_inside.contains('.') {
                    err_fatal!(
                        src: self.ast.file,
                        span: digit.span,
                        title: "Invalid array declaration",
                        msg: "静的配列は現在定数でのみ長さを初期化出来ます。"
                    );
                    code_line!(src: self.ast.file, span: digit.span);
                    return Err(());
                }

                // TODO: do not use unwrap
                let length = number_inside.parse::<u32>().unwrap();
                self.consume(TokenType::CloseSquareBracket)?;
                return Ok(ast::ParsedType::pArray(Box::new(inner_type), Some(length)))
            } else {
                self.consume(TokenType::CloseSquareBracket)?;
                return Ok(ast::ParsedType::pArray(Box::new(inner_type), None))
            }
        }

        // TODO - @Broken: are you sure that there's no more prefix??
        let mut core_type = if self.is(TokenType::Iden) {
            let Token {
                tokentype: _,
                span: span,
                lexeme: candidate,
            } = self.consume(TokenType::Iden)?.clone();

            let candidate = candidate.as_ref().unwrap();
            if is_prefix_banned(candidate) && !prefix.is_empty() {
                    err_fatal!(
                        src: self.ast.file,
                        span: span,
                        title: "Invalid Prefix placement",
                        msg: "Prefix {:?} はこの定義では使用できません。", prefix
                    );
                    code_line!(src: self.ast.file, span: span);
                    return Err(());
            }

            if candidate == "struct" {
                return self.parse_struct(prefix);
            }

            match ast::ParsedType::match_primitive(candidate) {
                Some(x) => x,
                None => ast::ParsedType::pUserdef(candidate.to_string()),
            }
        } else {
            ast::ParsedType::pUnknown
        };
        loop {
            match self.get_current().tokentype {
                TokenType::Caret => { self.advance(); core_type = ast::ParsedType::pPointer(Box::new(core_type)); }, 
                TokenType::Question => { self.advance(); core_type = ast::ParsedType::pOptional(Box::new(core_type)); }, 
                _ => break,
            }
        }
        Ok(core_type)
    }

    fn parse_struct(&mut self, _prefix: &ast::DeclPrefix) -> Result<ast::ParsedType, ()> {
        unimplemented!();
    }

    fn parse_type_prefix(&mut self) -> ast::DeclPrefix {
        let mut prefix = ast::DeclPrefix::empty();
        loop {
            match self.get_current().tokentype {
                TokenType::Constant => { self.advance(); prefix.insert(ast::DeclPrefix::Const); },
                TokenType::Public => { self.advance(); prefix.insert(ast::DeclPrefix::Public); },
                _ => break,
            }
        }
        prefix
    }

    pub(super) fn parse_function_decl(
        &mut self,
        baseinfo: ast::DeclarationData,
    ) -> Result<ast::StmtId, ()> {
        let args = self.parse_arguments()?;
        let func = self.block_statement()?;

        let data = ast::FunctionData {
            it: baseinfo,
            args,
            block_id: func,
        };

        let idx = self.ast.add_fn(data);
        let stmt = ast::Statement::Function(idx);
        Ok(self.ast.add_stmt(stmt))
    }

    pub(super) fn parse_arguments(&mut self) -> Result<Vec<ast::DeclarationData>, ()> {
        let _span = self.consume(TokenType::OpenParen)?.span;
        let mut args = vec![];

        if self.is(TokenType::CloseParen) {
            self.consume(TokenType::CloseParen)?;
            return Ok(args);
        }

        while self.is(TokenType::Iden) {
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

            if self.is(TokenType::Equal) {
                unimplemented!();
            } else if self.is(TokenType::Comma) {
                args.push(decl_info);
                self.consume(TokenType::Comma)?;
            } else if self.is(TokenType::CloseParen) {
                args.push(decl_info);
                break;
            } else {
                err_fatal!(
                    src: self.ast.file,
                    span: _span,
                    title: "Invalid Token",
                    msg: "`)` を想定しましたが、それ以外のトークンを検知しました。"
                );
                code_line!(src: self.ast.file, span: _span);
                panic!();
            }
        }

        self.consume(TokenType::CloseParen)?;
        Ok(args)
    }

    pub(super) fn parse_variable(&mut self) -> Result<ast::StmtId, ()> {
        self.assign_count += 1;

        let name = self.consume(TokenType::Iden)?.lexeme.to_owned().unwrap();
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

        if self.is(TokenType::Equal) {
            self.advance();
            let item = self.expression()?;
            decl_info.expr = Some(item);

            self.consume(TokenType::SemiColon)?;
            Ok(self.ast.add_stmt(ast::Statement::Declaration(decl_info)))
        } else if self.is(TokenType::SemiColon) {
            self.advance();
            Ok(self.ast.add_stmt(ast::Statement::Declaration(decl_info)))
        } else if self.is(TokenType::OpenParen) {
            self.parse_function_decl(decl_info)
        } else {
            let t = self.get_current();
            err_fatal!(
                src: self.ast.file,
                span: t.span,
                title: "Invalid Token",
                msg: " '=', ';', '(' のうち一つを期待しましたが、代わりに{:?}が見つかりました",
                t
            );
            code_line!(src: self.ast.file, span: t.span, pad: 1);
            Err(())
        }
    }
}
