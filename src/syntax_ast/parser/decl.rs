/*
 * タイプ及び定義をハンドリングする
 * */

use super::{ Parser };

use crate::ast;
use crate::tokenizer::token::{Token, TokenType};

use trace::{trace, info, warn};
use trace::Error;
use trace::position::CodeSpan;


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

impl<'tok> Parser<'tok> {
    fn parse_type(&mut self, prefix: &ast::DeclPrefix) -> ast::ParsedType {
        // Separate Array Consumption to other function
        if self.consume(TokenType::Asterisk).is_ok() {
            return ast::ParsedType::pPointer(Box::new(self.parse_type(prefix)));
        } else if self.consume(TokenType::OpenSquareBracket).is_ok() {
            let inner_type = self.parse_type(prefix);
            if self.consume(TokenType::SemiColon).is_ok() {
                // TODO - @Improvement: explicitly parsing digit prevents you from
                // using expression as an Array length
                // e.g. [int; 1 + 2]
                let digit = self.consume(TokenType::Digit).expect("Digit Expected.");
                let number_inside = digit.lexeme.as_ref().unwrap();
                if number_inside.contains(".") {
                    panic!("Array length cannot be fractional");
                }

                // TODO: do not use unwrap
                let length = number_inside.parse::<u32>().unwrap();
                self.consume(TokenType::CloseSquareBracket).expect("Close Square Bracket not found.");
                return ast::ParsedType::pArray(Box::new(inner_type), Some(length));
            } else {
                self.consume(TokenType::CloseSquareBracket).expect("Close Square Bracket not found.");
                return ast::ParsedType::pArray(Box::new(inner_type), None);
            }
        }

        // TODO - @Broken: are you sure that there's no more prefix??
        let Token { tokentype, span, lexeme: candidate } = self.advance();
        if tokentype != &TokenType::Iden {
            panic!("Tokentype is not iden??");
        }

        let candidate = candidate.as_ref().unwrap();
        if is_prefix_banned(candidate) && prefix != &ast::DeclPrefix::Empty {
            panic!("Type is not allowed to combine with prefix.");
        }

        if candidate == "struct" {
            return self.parse_struct(prefix);
        }

        match ast::ParsedType::match_primitive(candidate) {
            Some(x) => x,
            None => ast::ParsedType::pUserdef(candidate.to_string())
        }
    }

    fn parse_struct(&mut self, prefix: &ast::DeclPrefix) -> ast::ParsedType {
        unimplemented!();
    }

    fn parse_type_prefix(&mut self) -> ast::DeclPrefix {
        if self.consume(TokenType::Constant).is_ok() {
            ast::DeclPrefix::Constant
        } else {
            trace!("Empty prefix.");
            ast::DeclPrefix::Empty
        }
    }

    fn parse_type_suffix(&mut self) -> ast::DeclSuffix {
        if self.consume(TokenType::Question).is_ok() {
            ast::DeclSuffix::Optional
        } else if self.consume(TokenType::Bang).is_ok() {
            ast::DeclSuffix::Resulted
        } else {
            trace!("Empty suffix.");
            ast::DeclSuffix::Empty
        }
    }

    pub(super) fn parse_function_decl(&mut self, baseinfo: ast::DeclarationData) -> Result<ast::StmtId, Error> {
        let args = self.parse_arguments()?;

        self.consume(TokenType::OpenBrace);
        let func = self.block()?;

        let data = ast::FunctionData {
            it: baseinfo,
            args: args,
            block: func,
        };

        let idx = self.ast.add_fn(data);
        let stmt = ast::Statement::Function(idx);
        Ok(self.ast.add_stmt(stmt))
    }

    pub(super) fn parse_arguments(&mut self) -> Result<Vec<ast::DeclarationData>, Error> {
        let span = self.consume(TokenType::OpenParen)?.span;
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
            let dectype = self.parse_type(&prefix);
            let suffix = self.parse_type_suffix();

            let mut decl_info = ast::DeclarationData {
                kind: ast::DeclKind::Argument,
                name,
                dectype,
                prefix,
                suffix,
                expr: None
            };

            if self.is(TokenType::Equal) {
                unimplemented!();
            } else if self.is(TokenType::Comma) {
                args.push(decl_info);
                self.consume(TokenType::Comma)?;
            } else {
                break;
            }
        }

        self.consume(TokenType::CloseParen)?;
        Ok(args)
    }

    pub(super) fn parse_variable(&mut self) -> Result<ast::StmtId, Error> {
        self.assign_count += 1;
        self.current += 1;

        let name = self.advance().lexeme.to_owned().unwrap();
        self.consume(TokenType::Colon)?;

        let prefix = self.parse_type_prefix();
        let dectype = self.parse_type(&prefix);
        let suffix = self.parse_type_suffix();

        let mut decl_info = ast::DeclarationData {
            kind: ast::DeclKind::Variable,
            name,
            dectype,
            prefix,
            suffix,
            expr: None
        };

        if self.consume(TokenType::Equal).is_ok() {
            let item = self.expression()?;
            decl_info.expr = Some(item);

            self.consume(TokenType::SemiColon)?;
            Ok(self.ast.add_stmt(ast::Statement::Decralation(decl_info)))
        } else if self.consume(TokenType::SemiColon).is_ok() {
            if decl_info.suffix == ast::DeclSuffix::Optional {
                Ok(self.ast.add_stmt(ast::Statement::Decralation(decl_info)))
            } else {
                unreachable!();
            }
        } else if self.is(TokenType::OpenParen) {
            self.parse_function_decl(decl_info)
        } else {
            Err(Error::new_while_parsing(
                "Failed to parse the declaration process.",
                CodeSpan::oneline(self.current_line),
            ))
        }
    }
}
