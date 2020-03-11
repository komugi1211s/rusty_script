
use super::ast::*;

use crate::tokenizer::token::{Token, TokenType};
use trace::prelude::*;

mod decl;
mod stmt;

const MAX_IDENTIFIER_LENGTH: usize = 255;
pub struct Parser<'m> {
    ast: ASTree<'m>,
    module: &'m SourceFile,
    tokens: &'m Vec<Token<'m>>,
    current: usize,
    assign_count: usize,
    block_count: usize,
}

impl<'m> Parser<'m> {
    pub fn new(module: &'m SourceFile, tokens: &'m Vec<Token<'m>>) -> Self {
        assert!(module == tokens[0].file);
        Self {
            module,
            tokens,
            ast: ASTree::new(),
            current: 0,
            assign_count: 0,
            block_count: 0,
        }
    }

    pub fn parse(mut self) -> Result<ASTree<'m>, ()> {
        while !self.is_at_end() {
            let declaration = self.declaration()?;
            self.ast.add_root(declaration);
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
        &'m mut self,
        pass: &mut T,
        wrapped_func: fn(&mut Parser<'m>, &mut T) -> Result<(), ()>,
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

    fn declaration(&'m mut self) -> Result<StmtId, ()> {
        if self.is(TokenType::Iden) && self.is_next(TokenType::Colon) {
            return self.parse_variable();
        }

        self.statement()
    }

    fn expression(&'m mut self) -> Result<ExprId, ()> {
        let x = self.assignment();
        x
    }

    fn assignment(&'m mut self) -> Result<ExprId, ()> {
        let left_hand = self.logical_or()?;

        // @Improvement - 左辺値のハンドリングをもっとまともに出来れば良いかも知れない
        if self.is(TokenType::Equal) {
            let start_span = self.advance().span;
            let value = self.assignment()?;
            let end_span = self.get_current().span;

            // FIXME - @DumbCode: 借用不可の筈 ParserじゃなくCodegenでチェックしたほうが良いかも
            // 普通に借用できてしまったけどまあ当たり前ながら意図した動作ではなかった
            
            let assign_target = self.ast.get_expr(left_hand);
            if let Expr::Variable(_) = assign_target.data {
                let assign = Expression { 
                    module: self.module,
                    span: CodeSpan::combine(&start_span, &end_span),
                    data: Expr::Assign(left_hand, value),
                    end_type: None,
                };

                let result = self.ast.add_expr(assign);
                return Ok(result);
            } else {
                assign_target.report("Invalid Assignment Target", "値の割当を行う対象が不正です.");
                return Err(());
            }
        }

        Ok(left_hand)
    }

    pub fn logical_or(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
        let mut expr = self.logical_and()?;

        while self.is(TokenType::Or) {
            self.advance();
            let right = self.logical_and()?;
            let end_span = self.get_current().span;

            let span = CodeSpan::combine(&start_span, &end_span);
            let expression = Expression {
                module: self.module, 
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Logical(expr, right, Operator::Or),
                end_type: None,
            };
            expr = self.ast.add_expr(expression);
        }

        Ok(expr)
    }

    pub fn logical_and(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
        let mut expr = self.equality()?;

        while self.is(TokenType::And) {
            self.advance();
            let right = self.equality()?;
            let end_span = self.get_current().span;

            let expression = Expression {
                module: self.module, 
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Logical(expr, right, Operator::And),
                end_type: None,
            };
            expr = self.ast.add_expr(expression);
        }

        Ok(expr)
    }

    fn equality(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
        let mut expr = self.comparison()?;
        while !self.is_at_end() {
            let operator = match self.get_current().tokentype {
                TokenType::EqualEqual => Operator::EqEq,
                TokenType::NotEqual => Operator::NotEq,
                _ => break,
            };
            self.advance();
            let right = self.comparison()?;
            let end_span = self.get_current().span;

            let expression = Expression {
                module: self.module, 
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Binary(expr, right, operator),
                end_type: None,
            };

            expr = self.ast.add_expr(expression);
        }
        Ok(expr)
    }

    fn comparison(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
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
            let end_span = self.get_current().span;

            expr = self.ast.add_expr(Expression { 
                module: self.module,
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Binary(expr, right, operator),
                end_type: None,
            });
        }

        Ok(expr)
    }

    fn addition(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
        let mut expr = self.multiplification()?;

        while !self.is_at_end() {
            let operator = match self.get_current().tokentype {
                TokenType::Plus => Operator::Add,
                TokenType::Minus => Operator::Sub,
                _ => break,
            };
            self.advance();
            let right = self.multiplification()?;
            let end_span = self.get_current().span;

            expr = self.ast.add_expr(Expression { 
                module: self.module,
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Binary(expr, right, operator),
                end_type: None,
            });
        }

        Ok(expr)
    }

    fn multiplification(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
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
            let end_span = self.get_current().span;

            expr = self.ast.add_expr(Expression { 
                module: self.module,
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Binary(expr, right, operator),
                end_type: None,
            });
        }

        Ok(expr)
    }

    fn unary(&'m mut self) -> Result<ExprId, ()> {
        let start_span = self.get_current().span;
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
            let end_span = self.get_current().span;
            let expression: Expression;
            expression.module = self.module;
            expression.span = CodeSpan::combine(&start_span, &end_span);
            expression.data = Expr::Unary(right, oper);

            let unary = self.ast.add_expr(expression);

            /*
            let unary = self.ast.add_expr(Expression {
                module: self.module,
                span: CodeSpan::combine(&start_span, &end_span),
                data: Expr::Unary(right, oper)
            });
            */

            return Ok(unary);
        }

        self.postfix()
    }

    fn postfix(&'m mut self) -> Result<ExprId, ()> {
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
        self.get_current().report("Unimplemented Feature", "Unwrap機能は実装されていません。");
        Err(())
    }

    fn parse_deref(&mut self, _e: ExprId) -> Result<ExprId, ()> {
        self.get_current().report("Unimplemented Feature", "Deref機能は実装されていません。");
        Err(())
    }

    fn parse_array_ref(&mut self, _e: ExprId) -> Result<ExprId, ()> {
        self.get_current().report("Unimplemented Feature", "ArrayRef機能は実装されていません。");
        Err(())
    }

    fn parse_func_call(&'m mut self, expr: ExprId) -> Result<ExprId, ()> {
        let mut builder = ExprBuilder::default().token(self.get_previous());
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
        builder.expand_span(self.consume(TokenType::CloseParen)?.span)
               .data(Expr::FunctionCall(expr, v));
        Ok(self.ast.add_expr(builder.build()))
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

    fn primary(&'m mut self) -> Result<ExprId, ()> {
        let mut builder = ExprBuilder::default().token(self.advance());
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

                let result = builder.data(Expr::Literal(lit)).build();
                Ok(self.ast.add_expr(result))
                // Err(Error::new_while_parsing("Digit does not match either int or float", self.current_line))
            }
            Str => {
                let lit = Literal::new_str(inside);
                let result = builder.data(Expr::Literal(lit)).build();
                Ok(self.ast.add_expr(result))
            }
            Iden => {
                if let Some(ref inside_lexeme) = inside.lexeme {
                    match inside_lexeme.as_str() {
                        "true" | "false" => {
                            let lit = Literal::new_bool(inside);
                            let result = builder.data(Expr::Literal(lit)).build();
                            Ok(self.ast.add_expr(result))
                        }
                        "null" => {
                            let lit = Literal::new_null(inside);
                            let result = builder.data(Expr::Literal(lit)).build();
                            Ok(self.ast.add_expr(result))
                        }
                        _ => {
                            if inside_lexeme.len() > MAX_IDENTIFIER_LENGTH {
                                let msg = 
                                    format!("\n識別子の長さ({})が許容範囲を超過しました。\n 識別子は長さ最大{}文字までです。", inside_lexeme.len(), MAX_IDENTIFIER_LENGTH);
                                inside.report("Maximum Identifier Length Exceeded", &msg);
                                Err(())
                            } else {
                                let var = Expr::Variable(inside_lexeme.to_owned());
                                let result = builder.data(var).build();
                                Ok(self.ast.add_expr(result))
                            }
                        }
                    }
                } else {
                    unreachable!();
                }
            }
            OpenParen => {
                let inside_paren = self.expression()?;
                let data = Expr::Grouping(inside_paren);

                let end_token = self.consume(CloseParen)?;
                let built = builder.expand_span(end_token.span).data(data).build();
                Ok(self.ast.add_expr(built))
            }
            _s => {
                let formatted = 
                    format!("\n未知のトークンを受け付けました。処理できません。
                             \n未知のトークン: {:?}", inside);
                inside.report("Unknown Token", &formatted);
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
        let actual = self.get_current();
        let message = 
            format!("想定していたトークン ({:?}) と違うもの ({:?}) が検知されました。",
                expected, actual.tokentype);

        actual.report("Invalid Token", &message);
        Err(())
    }
}
