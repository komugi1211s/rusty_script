/*
 * タイプ及び定義をハンドリングする
 * */

use super::{ Parser };

use crate::ast;
use crate::tokenizer::token::{Token, TokenType};

use trace::{trace, info, warn};
use trace::Error;
use trace::position::CodeSpan;
fn declare_argument(&mut self) -> Result<Vec<DeclarationData>, Error> {
        let start_span = self.consume(TokenType::OpenParen)?.span;
        let mut arguments: Vec<DeclarationData> = Vec::new();

        if self.is(TokenType::CloseParen) {
            self.consume(TokenType::CloseParen)?;
            return Ok(arguments);
        }

        while self.is(TokenType::Iden) {
            self.current += 1;
            let mut decl_info = self.initialize_decl_data()?;
            decl_info.kind = DeclKind::Argument;

            let bridge_token = self.advance();
            let bridge_span = bridge_token.span;
            match bridge_token.tokentype {
                TokenType::Equal => {
                    let item = self.expression()?;
                    decl_info.expr = Some(item);
                    arguments.push(decl_info);

                    match self.advance().tokentype {
                        TokenType::Comma => continue,
                        TokenType::CloseParen => return Ok(arguments),
                        _ => {
                            return Err(Error::new_while_parsing(
                                "Argument declaration must end with close paren",
                                CodeSpan::new(start_span.start_usize(), self.current_line),
                            ))
                        }
                    }
                }
                TokenType::Comma => {
                    arguments.push(decl_info);
                    continue;
                }
                TokenType::CloseParen => {
                    arguments.push(decl_info);
                    return Ok(arguments);
                }
                _ => {
                    return Err(Error::new_while_parsing(
                        "Unknown Token detected while parsing arguments",
                        bridge_span,
                    ))
                }
            }
        }
        Ok(arguments)
    }
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
            return ast::ParsedType::pPointer(box self.parse_type(prefix));
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
                return ast::ParsedType::pArray(box inner_type, Some(length));
            } else {
                self.consume(TokenType::CloseSquareBracket).expect("Close Square Bracket not found.");
                return ast::ParsedType::pArray(box inner_type, None);
            }
        }

        // TODO - @Broken: are you sure that there's no more prefix??
        let Token { tokentype, span, lexeme: candidate } = self.advance();
        if tokentype != TokenType::Iden {
            panic!("Tokentype is not iden??");
        }

        let candidate = candidate.as_ref().unwrap();
        if is_prefix_banned(candidate) && prefix != &ast::DeclPrefix::Empty {
            panic!("Type is not allowed to combine with prefix.");
        }
        
        match ast::ParsedType::match_primitive(candidate) {
            Some(x) => x,
            None =>  todo!();
        }
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

    pub(super) fn parse_variable(&mut self) -> Result<ast::StmtId, Error> {
        self.assign_count += 1;
        self.current += 1;

        let name = self.get_previous().lexeme.to_owned().unwrap();
        self.consume(TokenType::Colon)?;

        let prefix = self.parse_type_prefix();
        let dectype = self.parse_type(&prefix);
        let suffix = self.parse_type_suffix();
        let mut decl_info = DeclarationData {
            kind: DeclKind::Variable,
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
            Ok(self.ast.add_stmt(Statement::Decralation(decl_info)))
        } else if self.consume(TokenType::SemiColon).is_ok() {
            if decl_info.suffix == ast::DeclSuffix::Optional {
                Ok(self.ast.add_stmt(Statement::Decralation(decl_info)))
            } else {
                unreachable!();
            }
        } else if self.is(TokenType::OpenParen) {
            self.declare_function(decl_info)
        } else {
            Err(Error::new_while_parsing(
                "Failed to parse the declaration process.",
                CodeSpan::oneline(self.current_line),
            ))
        }
    }
}
