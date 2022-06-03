/*
TODO:
    (17) ASTをマージできるようにしてディレクティブに対応できるように
 * */

use super::{
    ast::*,
    tokenizer::token::{Token, TokenType},
    trace::prelude::*,
    types::{ Type, Value },
};

mod decl;
mod stmt;

const MAX_IDENTIFIER_LENGTH: usize = 255;
pub struct Parser<'m>
{
    module: &'m SourceFile,
    ast: ASTree<'m>,
    tokens: &'m [Token<'m>],
    current: usize,
    assign_count: usize,
    block_count: usize,
}

impl<'m> Parser<'m>
{
    pub fn new(module: &'m SourceFile, tokens: &'m Vec<Token<'m>>) -> Self
    {
        // FIXME - @DumbCode
        assert!(module == tokens[0].file);
        Self {
            ast: ASTree::new(),
            module,
            tokens,
            current: 0,
            assign_count: 0,
            block_count: 0,
        }
    }

    pub fn parse(mut self) -> Result<ASTree<'m>, ()>
    {
        while !self.is_at_end()
        {
            /*
            if self.is(TokenType::AtMark)
            {
                let directive = self.directive()?;
                self.ast.add_directive(directive);
            }
            else
            */
            {
                let declaration = self.declaration()?;
                self.ast.add_root(declaration);
            }
        }
        Ok(self.ast)
    }

    fn is_at_end(&self) -> bool
    {
        let current_token = self.tokens.get(self.current);
        match current_token
        {
            None => true,
            Some(inside) => inside.tokentype == TokenType::EOF,
        }
    }

    fn declaration(&mut self) -> Result<StmtId, ()>
    {

        let res = if self.is(TokenType::Iden) && self.is_next(TokenType::Colon)
        {
            self.parse_variable()
        }
        else
        {
            self.statement()
        };

        res
    }

    #[allow(dead_code)]
    fn directive(&mut self) -> Result<StmtId, ()>
    {
        unimplemented!()
    }

    fn expression(&mut self) -> Result<Expression<'m>, ()>
    {
        let x = self.assignment();
        x
    }

    fn assignment(&mut self) -> Result<Expression<'m>, ()>
    {
        let left_hand = self.logical_or()?;

        // @Improvement - 左辺値のハンドリングをもっとまともに出来れば良いかも知れない
        if self.is(TokenType::Equal)
        {
            let start_span = self.advance().span;
            let value = self.assignment()?;
            let end_span = self.get_current().span;

            // FIXME - @DumbCode: 借用不可の筈 ParserじゃなくCodegenでチェックしたほうが良いかも
            // 普通に借用できてしまったけどまあ当たり前ながら意図した動作ではなかった

            if left_hand.is_lvalue()
            {
                let assign = ExprInit {
                    kind: ExprKind::Assign,
                    module: Some(self.module),
                    lhs: Some(Box::new(left_hand)),
                    rhs: Some(Box::new(value)),
                    span: Some(CodeSpan::combine(&start_span, &end_span)),
                    end_type: None,
                    oper: Some(Operator::Asgn),
                    ..Default::default()
                }
                .init();

                return Ok(assign);
            }
            else
            {
                left_hand.report("Invalid Assignment Target", "値の割当を行う対象が不正です.");
                return Err(());
            }
        }

        Ok(left_hand)
    }

    pub fn logical_or(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let mut expr = self.logical_and()?;

        while self.is(TokenType::Or)
        {
            self.advance();
            let right = self.logical_and()?;
            let end_span = self.get_current().span;

            let span = CodeSpan::combine(&start_span, &end_span);
            expr = ExprInit {
                kind: ExprKind::Logical,
                module: Some(self.module),
                span: Some(span),
                lhs: Some(Box::new(expr)),
                rhs: Some(Box::new(right)),
                oper: Some(Operator::Or),
                end_type: None,
                ..Default::default()
            }
            .init();
        }

        Ok(expr)
    }

    pub fn logical_and(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let mut expr = self.equality()?;

        while self.is(TokenType::And)
        {
            self.advance();
            let right = self.equality()?;
            let end_span = self.get_current().span;

            expr = ExprInit {
                kind: ExprKind::Logical,
                module: Some(self.module),
                span: Some(CodeSpan::combine(&start_span, &end_span)),
                lhs: Some(Box::new(expr)),
                rhs: Some(Box::new(right)),
                oper: Some(Operator::And),
                end_type: None,
                ..Default::default()
            }
            .init();
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let mut expr = self.comparison()?;
        while !self.is_at_end()
        {
            let operator = match self.get_current().tokentype
            {
                TokenType::EqualEqual => Operator::EqEq,
                TokenType::NotEqual => Operator::NotEq,
                _ => break,
            };
            self.advance();
            let right = self.comparison()?;
            let end_span = self.get_current().span;

            expr = ExprInit {
                kind: ExprKind::Binary,
                module: Some(self.module),
                span: Some(CodeSpan::combine(&start_span, &end_span)),
                lhs: Some(Box::new(expr)),
                rhs: Some(Box::new(right)),
                oper: Some(operator),
                end_type: None,
                ..Default::default()
            }
            .init();
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let mut expr = self.addition()?;

        use TokenType::*;
        while !self.is_at_end()
        {
            let operator = match self.get_current().tokentype
            {
                LessEqual => Operator::LessEq,
                MoreEqual => Operator::MoreEq,
                Less => Operator::Less,
                More => Operator::More,
                _ => break,
            };
            self.advance();
            let right = self.addition()?;
            let end_span = self.get_current().span;

            expr = ExprInit {
                kind: ExprKind::Binary,
                module: Some(self.module),
                span: Some(CodeSpan::combine(&start_span, &end_span)),
                lhs: Some(Box::new(expr)),
                rhs: Some(Box::new(right)),
                oper: Some(operator),
                end_type: None,
                ..Default::default()
            }
            .init();
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let mut expr = self.multiplification()?;

        while !self.is_at_end()
        {
            let operator = match self.get_current().tokentype
            {
                TokenType::Plus => Operator::Add,
                TokenType::Minus => Operator::Sub,
                _ => break,
            };
            self.advance();
            let right = self.multiplification()?;
            let end_span = self.get_current().span;

            expr = ExprInit {
                kind: ExprKind::Binary,
                module: Some(self.module),
                span: Some(CodeSpan::combine(&start_span, &end_span)),
                lhs: Some(Box::new(expr)),
                rhs: Some(Box::new(right)),
                oper: Some(operator),
                end_type: None,
                ..Default::default()
            }
            .init();
        }

        Ok(expr)
    }

    fn multiplification(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let mut expr = self.unary()?;

        while !self.is_at_end()
        {
            let operator = match self.get_current().tokentype
            {
                TokenType::Slash => Operator::Div,
                TokenType::Asterisk => Operator::Mul,
                TokenType::Percent => Operator::Mod,
                _ => break,
            };
            self.advance();
            let right = self.unary()?;
            let end_span = self.get_current().span;

            expr = ExprInit {
                kind: ExprKind::Binary,
                module: Some(self.module),
                span: Some(CodeSpan::combine(&start_span, &end_span)),
                lhs: Some(Box::new(expr)),
                rhs: Some(Box::new(right)),
                oper: Some(operator),
                end_type: None,
                ..Default::default()
            }
            .init();
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression<'m>, ()>
    {
        let start_span = self.get_current().span;
        let operator = match self.get_current().tokentype
        {
            TokenType::Bang => Some(Operator::Not),
            TokenType::Minus => Some(Operator::Neg),
            TokenType::Question => Some(Operator::Wrap),
            _ => None,
        };
        if let Some(oper) = operator
        {
            self.advance();
            let right = self.unary()?;
            let end_span = self.get_current().span;

            let unary = ExprInit {
                kind: ExprKind::Unary,
                module: Some(self.module),
                span: Some(CodeSpan::combine(&start_span, &end_span)),
                rhs: Some(Box::new(right)),
                oper: Some(oper),
                ..Default::default()
            }
            .init();

            return Ok(unary);
        }

        self.postfix()
    }

    fn postfix(&mut self) -> Result<Expression<'m>, ()>
    {
        let mut expr = self.primary()?;

        'parse: loop
        {
            expr = match self.get_current().tokentype
            {
                TokenType::OpenParen => self.parse_func_call(expr)?,
                TokenType::Bang => self.parse_unwrap(expr)?,
                TokenType::OpenSquareBracket => self.parse_array_ref(expr)?,
                TokenType::Dot => self.parse_field(expr)?,
                _ => break 'parse,
            }
        }
        Ok(expr)
    }

    fn parse_field(&mut self, variable: Expression<'m>) -> Result<Expression<'m>, ()>
    {
        self.consume(TokenType::Dot)?;
        let right_hand_expr = self.postfix()?;
        let combined_span   = CodeSpan::combine(&variable.span, &right_hand_expr.span);

        let expr = ExprInit {
            kind:   ExprKind::FieldAccess,
            module: Some(self.module),
            lhs:    Some(Box::new(variable)),
            rhs:    Some(Box::new(right_hand_expr)),
            span:   Some(combined_span),
            ..Default::default()
        };

        Ok(expr.init())
    }

    fn parse_unwrap(&mut self, _e: Expression<'m>) -> Result<Expression<'m>, ()>
    {
        self.get_current()
            .report("Unimplemented Feature", "Unwrap機能は実装されていません。");
        Err(())
    }

    fn parse_array_ref(&mut self, var: Expression<'m>) -> Result<Expression<'m>, ()>
    {
        let mut expr = ExprInit {
            kind: ExprKind::ArrayRef,
            module: Some(self.module),
            lhs: Some(Box::new(var)),
            ..Default::default()
        };

        let start_span = self.consume(TokenType::OpenSquareBracket)?.span;
        expr.rhs = Some(Box::new(self.expression()?));
        let end_span = self.consume(TokenType::CloseSquareBracket)?.span;
        expr.span = Some(CodeSpan::combine(&start_span, &end_span));

        Ok(expr.init())
    }

    fn parse_func_call(&mut self, var: Expression<'m>) -> Result<Expression<'m>, ()>
    {
        let mut expr = ExprInit {
            kind: ExprKind::FunctionCall,
            module: Some(self.module),
            ..Default::default()
        };
        let start_span = self.consume(TokenType::OpenParen)?.span;

        let mut v = Vec::<Expression<'m>>::new();
        if !self.is(TokenType::CloseParen)
        {
            loop
            {
                v.push(self.expression()?);
                if self.is(TokenType::Comma)
                {
                    self.advance();
                }
                else
                {
                    break;
                }
            }
        }

        let end_span = self.consume(TokenType::CloseParen)?.span;

        expr.span = Some(CodeSpan::combine(&start_span, &end_span));
        expr.lhs = Some(Box::new(var));
        expr.arg_expr = v;

        Ok(expr.init())
    }

    fn is(&mut self, _type: TokenType) -> bool
    {
        !self.is_at_end() && self.get_current().tokentype == _type
    }

    fn is_next(&mut self, _type: TokenType) -> bool
    {
        !self.is_at_end() && self.get_next().tokentype == _type
    }

    #[allow(dead_code)]
    fn is_previous(&mut self, _type: TokenType) -> bool
    {
        !self.is_at_end() && self.get_previous().tokentype == _type
    }

    fn get_current(&self) -> &Token<'m>
    {
        let x = self.tokens.get(self.current).unwrap();
        x
    }

    #[allow(dead_code)]
    fn get_previous(&self) -> &Token<'m>
    {
        let x = self.tokens.get(self.current - 1).unwrap();
        x
    }

    fn get_next(&self) -> &Token<'m>
    {
        if self.current + 1 >= self.tokens.len()
        {
            return self.get_current();
        }
        let x = self.tokens.get(self.current + 1).unwrap();
        x
    }

    fn advance(&mut self) -> &Token<'m>
    {
        self.current += 1;
        let x = self.tokens.get(self.current - 1).unwrap();
        x
    }

    fn primary(&mut self) -> Result<Expression<'m>, ()>
    {
        let inside = self.advance().clone();

        let mut expr = ExprInit {
            kind: ExprKind::Literal,
            module: Some(self.module),
            span: Some(inside.span),
            ..Default::default()
        };

        use TokenType::*;
        let result = match &inside.tokentype
        {
            Digit =>
            {
                let inside_lexeme = inside.lexeme.as_ref().unwrap();
                let contain_dot   = inside_lexeme.contains('.');
                let lit = if contain_dot {
                    if let Ok(n) = inside_lexeme.parse() {
                        Value::Float(n)
                    } else {
                        inside.report("Broken Float Literal",
                                      "実数を期待しましたがパースに失敗しました。");
                        return Err(());
                    }
                }
                else
                {
                    if let Ok(n) = inside_lexeme.parse() {
                        Value::Int(n)
                    } else {
                        inside.report("Broken Integer Literal",
                                      "整数を期待しましたがパースに失敗しました。");
                        return Err(());
                    }
                };

                expr.end_type = Some(lit.to_type());
                expr.literal = Some(lit);

                Ok(expr.init())
                // Err(Error::new_while_parsing("Digit does not match either int or float", self.current_line))
            }
            Str =>
            {
                let inside_lexeme = inside.lexeme.unwrap();
                let lit = Value::Str(inside_lexeme.clone());
                expr.literal = Some(lit);
                expr.end_type = Some(Type::string());

                Ok(expr.init())
            }
            Iden =>
            {
                if let Some(ref inside_lexeme) = inside.lexeme
                {
                    match inside_lexeme.as_str()
                    {
                        "true" | "false" =>
                        {
                            expr.literal = Some(Value::Boolean(inside_lexeme.as_str() == "true"));
                            expr.end_type = Some(Type::boolean());
                            Ok(expr.init())
                        }
                        "null" =>
                        {
                            expr.literal = Some(Value::Null);
                            expr.end_type = Some(Type::null());
                            Ok(expr.init())
                        }
                        _ =>
                        {
                            if inside_lexeme.len() > MAX_IDENTIFIER_LENGTH
                            {
                                let msg = format!(
                                    "\n識別子の長さ({})が許容範囲を超過しました。
                                    \n 識別子は長さ最大{}文字までです。",
                                    inside_lexeme.len(),
                                    MAX_IDENTIFIER_LENGTH
                                );
                                inside.report("Maximum Identifier Length Exceeded", &msg);
                                Err(())
                            }
                            else
                            {
                                expr.kind = ExprKind::Variable;
                                expr.variable_name = Some(inside_lexeme.to_owned());
                                Ok(expr.init())
                            }
                        }
                    }
                }
                else
                {
                    unreachable!();
                }
            }
            OpenParen =>
            {
                let inside_paren = self.expression()?;
                let end_span = self.consume(CloseParen)?.span;

                expr.kind = ExprKind::Grouping;
                expr.lhs = Some(Box::new(inside_paren));
                expr.span = Some(CodeSpan::combine(&expr.span.unwrap(), &end_span));
                Ok(expr.init())
            }
            OpenSquareBracket =>
            {
                let mut vector = Vec::new();
                while !self.is_at_end()
                {
                    if self.is(CloseSquareBracket) { break; }

                    let expr = self.expression()?;
                    vector.push(expr);

                    if self.is(Comma) { self.consume(Comma)?; }
                }

                let end_span = self.consume(CloseSquareBracket)?.span;
                expr.kind = ExprKind::ArrayInst;
                expr.array_expr = vector;
                expr.span = Some(CodeSpan::combine(&expr.span.unwrap(), &end_span));
                Ok(expr.init())
            }
            _s =>
            {
                let formatted = format!(
                    "\n未知のトークンを受け付けました。処理できません。
                             \n未知のトークン: {:?}",
                    inside
                );
                inside.report("Unknown Token", &formatted);
                Err(())
            }
        };

        result
    }

    fn consume(&mut self, expected: TokenType) -> Result<&Token, ()>
    {
        // TODO - @DumbCode: 無駄なクローンを許すな
        if self.is(expected)
        {
            return Ok(self.advance());
        }
        let actual = self.get_current();
        let message = format!(
            "想定していたトークン ({:?}) と違うもの ({:?}) が検知されました。",
            expected, actual.tokentype
        );

        actual.report("Invalid Token", &message);
        Err(())
    }
}
