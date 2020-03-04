use trace::Error;

use super::ast::{
    self, ASTree, AstNode, BlockData, DeclKind, DeclarationData, Expr, ExprId, FunctionData,
    Literal, Operator, Statement, StmtId,
};

use crate::tokenizer::token::{Token, TokenType};
use trace::position::CodeSpan;
use trace::{code_line, err_fatal, err_internal, SourceFile};

mod decl;
mod stmt;

const MAX_IDENTIFIER_LENGTH: usize = 255;
pub struct Parser<'m, 't> {
    tokens: &'t Vec<Token>,
    ast: ASTree<'m>,
    current: usize,
    assign_count: usize,
    block_count: usize,
}

impl<'m, 't> Parser<'m, 't> {
    pub fn new(modu: &'m SourceFile, _tok: &'t Vec<Token>) -> Self {
        Self {
            tokens: _tok,
            ast: ASTree {
                file: modu,
                ast: vec![],
                stmt: vec![],
                expr: vec![],
                functions: vec![],
            },
            current: 0,
            assign_count: 0,
            block_count: 0,
        }
    }

    pub fn parse(mut self) -> Result<ASTree<'m>, ()> {
        while !self.is_at_end() {
            let start_line = self.get_current().span.clone();
            let declaration = self.declaration()?;
            let end_line = self.get_current().span.clone();
            let codespan = CodeSpan::combine(&start_line, &end_line);
            self.ast.add_ast(declaration, codespan);
        }
        Ok(self.ast)
    }

    fn is_at_end(&self) -> bool {
        let current_token = self.tokens.get(self.current);
        match current_token {
            None => true,
            Some(inside) => inside.tokentype == TokenType::EOF,
        }
    }

    fn enter_block<T>(
        &mut self,
        pass: &mut T,
        wrapped_func: fn(&mut Parser<'m, 't>, &mut T) -> Result<(), ()>,
    ) -> usize {
        let previous_count = self.assign_count;
        self.assign_count = 0;
        self.block_count += 1;

        wrapped_func(self, pass);

        let new_assign = self.assign_count;
        self.assign_count = previous_count;
        self.block_count -= 1;
        new_assign
    }

    fn declaration(&mut self) -> Result<StmtId, ()> {
        if self.is(TokenType::Iden) && self.is_next(TokenType::Colon) {
            return self.parse_variable();
        }

        self.statement()
    }

    fn expression(&mut self) -> Result<ExprId, ()> {
        let x = self.assignment();
        x
    }

    fn assignment(&mut self) -> Result<ExprId, ()> {
        let expr = self.logical_or()?;

        // @Improvement - 左辺値のハンドリングをもっとまともに出来れば良いかも知れない
        if self.is(TokenType::Equal) {
            let assign_span = self.advance().span;
            let value = self.assignment()?;

            // FIXME - @DumbCode: 借用不可の筈 ParserじゃなくCodegenでチェックしたほうが良いかも
            // 普通に借用できてしまったけどまあ当たり前ながら意図した動作ではなかった
            if let Expr::Variable(_) = self.ast.get_expr(expr) {
                let expr = self.ast.add_expr(Expr::Assign(expr, value));
                return Ok(expr);
            } else {
                let span = CodeSpan::oneline(assign_span.start_usize());
                err_fatal!(
                    src: self.ast.file,
                    span: span,
                    title: "Invalid Assignment Target",
                    msg: "\n値の割当を行う対象が不正です。",
                );
                code_line!(src: self.ast.file, span: span, pad: 1);
                return Err(());
            }
        }

        Ok(expr)
    }

    pub fn logical_or(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.logical_and()?;

        while self.is(TokenType::Or) {
            self.advance();
            let right = self.logical_and()?;
            expr = self.ast.add_expr(Expr::Logical(expr, right, Operator::Or));
        }

        Ok(expr)
    }
    pub fn logical_and(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.equality()?;

        while self.is(TokenType::And) {
            self.advance();
            let right = self.equality()?;
            expr = self.ast.add_expr(Expr::Logical(expr, right, Operator::And));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.comparison()?;
        while !self.is_at_end() {
            let operator = match self.get_current().tokentype {
                TokenType::EqualEqual => Operator::EqEq,
                TokenType::NotEqual => Operator::NotEq,
                _ => break,
            };
            self.advance();
            let right = self.comparison()?;
            expr = self.ast.add_expr(Expr::Binary(expr, right, operator));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.addition()?;

        use TokenType::*;
        while !self.is_at_end() {
            let operator = match self.get_current().tokentype {
                LessEqual => Operator::LessEq,
                MoreEqual => Operator::MoreEq,
                Less => Operator::Less,
                More => Operator::More,
                _ => break,
            };
            self.advance();

            let right = self.addition()?;
            expr = self.ast.add_expr(Expr::Binary(expr, right, operator));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.multiplification()?;

        while !self.is_at_end() {
            let operator = match self.get_current().tokentype {
                TokenType::Plus => Operator::Add,
                TokenType::Minus => Operator::Sub,
                _ => break,
            };
            self.advance();
            let right = self.multiplification()?;
            expr = self.ast.add_expr(Expr::Binary(expr, right, operator));
        }

        Ok(expr)
    }

    fn multiplification(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.unary()?;

        while !self.is_at_end() {
            let operator = match self.get_current().tokentype {
                TokenType::Slash => Operator::Div,
                TokenType::Asterisk => Operator::Mul,
                TokenType::Percent => Operator::Mod,
                _ => break,
            };
            self.advance();
            let right = self.unary()?;

            expr = self.ast.add_expr(Expr::Binary(expr, right, operator));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<ExprId, ()> {
        let operator = match self.get_current().tokentype {
            TokenType::Bang => Some(Operator::Not),
            TokenType::Minus => Some(Operator::Neg),
            TokenType::Question => Some(Operator::Wrap),
            TokenType::Caret => Some(Operator::Ref),
            _ => None,
        };
        if let Some(oper) = operator {
            self.advance();
            let right = self.unary()?;
            let unary = self.ast.add_expr(Expr::Unary(right, oper));

            return Ok(unary);
        }

        self.postfix()
    }

    fn postfix(&mut self) -> Result<ExprId, ()> {
        let mut expr = self.primary()?;

        'parse: loop {
            expr = match self.get_current().tokentype {
                TokenType::OpenParen => self.parse_func_call(expr)?,
                TokenType::Bang => self.parse_unwrap(expr)?,
                TokenType::OpenSquareBracket => self.parse_array_ref(expr)?,
                TokenType::Caret => self.parse_deref(expr)?,
                _ => break 'parse,
            }
        }
        Ok(expr)
    }

    fn parse_unwrap(&mut self, _e: ExprId) -> Result<ExprId, ()> {
        let span = self.get_current().span;
        err_fatal!(src: self.ast.file, span: span, title: "Unimplemented Feature", msg: "Unwrap機能は実装されていません。");
        code_line!(src: self.ast.file, span: span);
        Err(())
    }

    fn parse_deref(&mut self, _e: ExprId) -> Result<ExprId, ()> {
        let span = self.get_current().span;
        err_fatal!(src: self.ast.file, span: span, title: "Unimplemented Feature", msg: "Deref機能は実装されていません。");
        code_line!(src: self.ast.file, span: span);
        Err(())
    }

    fn parse_array_ref(&mut self, _e: ExprId) -> Result<ExprId, ()> {
        let span = self.get_current().span;
        err_fatal!(src: self.ast.file, span: span, title: "Unimplemented Feature", msg: "ArrayRef機能は実装されていません。");
        code_line!(src: self.ast.file, span: span);
        Err(())
    }

    fn parse_func_call(&mut self, expr: ExprId) -> Result<ExprId, ()> {
        self.consume(TokenType::OpenParen)?;
        let mut v = Vec::<ExprId>::new();

        if !self.is(TokenType::CloseParen) {
            loop {
                v.push(self.expression()?);
                if self.is(TokenType::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        let _paren = self.consume(TokenType::CloseParen)?;
        let expr = Expr::FunctionCall(expr, v);
        Ok(self.ast.add_expr(expr))
    }

    fn is(&mut self, _type: TokenType) -> bool {
        (!self.is_at_end() && self.get_current().tokentype == _type)
    }

    fn is_next(&mut self, _type: TokenType) -> bool {
        (!self.is_at_end() && self.get_next().tokentype == _type)
    }

    fn is_previous(&mut self, _type: TokenType) -> bool {
        (!self.is_at_end() && self.get_previous().tokentype == _type)
    }

    fn get_current(&self) -> &Token {
        let x = self.tokens.get(self.current).unwrap();
        x
    }

    fn get_previous(&self) -> &Token {
        let x = self.tokens.get(self.current - 1).unwrap();
        x
    }

    fn get_next(&self) -> &Token {
        if self.current + 1 >= self.tokens.len() {
            return self.get_current();
        }
        let x = self.tokens.get(self.current + 1).unwrap();
        x
    }

    fn advance(&mut self) -> &Token {
        self.current += 1;
        let x = self.tokens.get(self.current - 1).unwrap();
        x
    }

    fn primary(&mut self) -> Result<ExprId, ()> {
        self.advance();
        let inside = self.get_previous();
        use TokenType::*;
        let result = match &inside.tokentype {
            Digit => {
                let inside_lexeme = inside.lexeme.clone().unwrap();
                let contain_dot = inside_lexeme.contains('.');
                let lit = if contain_dot {
                    Literal::new_float(inside)
                } else {
                    Literal::new_int(inside)
                };
                let expr_id = self.ast.add_expr(Expr::Literal(lit));
                Ok(expr_id)
                // Err(Error::new_while_parsing("Digit does not match either int or float", self.current_line))
            }
            Str => {
                let lit = Literal::new_str(inside);
                let expr_id = self.ast.add_expr(Expr::Literal(lit));
                Ok(expr_id)
            }
            Iden => {
                if let Some(ref inside_lexeme) = inside.lexeme {
                    match inside_lexeme.as_str() {
                        "true" | "false" => {
                            let lit = Literal::new_bool(inside);
                            let expr_id = self.ast.add_expr(Expr::Literal(lit));
                            Ok(expr_id)
                        }
                        "null" => {
                            let lit = Literal::new_null(inside);
                            let expr_id = self.ast.add_expr(Expr::Literal(lit));
                            Ok(expr_id)
                        }
                        _ => {
                            if inside_lexeme.len() > MAX_IDENTIFIER_LENGTH {
                                err_fatal!(
                                src: self.ast.file,
                                span: inside.span,
                                title: "Maximum Identifier Length Exceeded",
                                msg: "\n識別子の長さ( {} )が許容範囲を超過しました。\n 識別子は長さ最大{}文字までです。",
                                    inside_lexeme.len(),
                                    MAX_IDENTIFIER_LENGTH
                                );

                                code_line!(
                                    src: self.ast.file,
                                    span: inside.span,
                                    pad:1
                                );
                                Err(())
                            } else {
                                let expr = Expr::Variable(inside_lexeme.to_owned());
                                let expr_id = self.ast.add_expr(expr);
                                Ok(expr_id)
                            }
                        }
                    }
                } else {
                    unreachable!();
                }
            }
            OpenParen => {
                let inside_paren = self.expression()?;
                let _closed_paren = self.consume(CloseParen)?;
                let expr = Expr::Grouping(inside_paren);
                Ok(self.ast.add_expr(expr))
            }
            _s => {
                err_fatal!(
                    src: self.ast.file,
                    span: inside.span,
                    title: "Unknown Token",
                    msg: "\n未知のトークンを受け付けました。処理できません。\n未知のトークン: {:?}", inside
                );
                code_line!(
                    src: self.ast.file,
                    span: inside.span,
                    pad: 1
                );

                Err(())
            }
        };

        result
    }

    fn consume(&mut self, expected: TokenType) -> Result<&Token, ()> {
        // TODO - @DumbCode: 無駄なクローンを許すな
        if self.is(expected) {
            return Ok(self.advance());
        }
        let span = self.get_current().span;
        let actual = self.get_current().tokentype;
        err_fatal!(
            src: self.ast.file,
            span: span,
            title: "Invalid Token",
            msg: "想定していたトークン ({:?}) と違うもの ({:?}) が検知されました。",
            expected, actual
        );

        code_line!(
            src: self.ast.file,
            span: span,
            pad: 1
        );
        Err(())
    }
}
