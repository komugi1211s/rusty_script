use trace::Error;

use super::{
    ast::{
        self,
        BlockData, DeclKind, DeclarationData, Expr, FunctionData, Literal, Operator,
        ParsedResult, Statement, AstNode, StmtId, ExprId
    },
};

use crate::tokenizer::token::{Token, TokenType};
use trace::position::CodeSpan;

mod decl;
mod stmt;

// TODO:
// All clone call to a lifetime management
// More refined Match system™
// Refactor

// FIXME:
// Primary + ; does not work ( 10; spits error ) -> I think it fixed?

/*
    NOTE:
    Identifierとして使える最高の文字列の長さ
    そもそもIdentifierとして使える文字(a~Z, _)のうち、u8にした時最も大きい文字(z, 122)を
    u16の範囲内で繰り返せるのが537文字 ( 122 * 537 = 65514 ) なので、
    それより少し小さい530文字を許容できる最大値として使う

    理由としては単にVMがバイトコードを生成する際に
    いちいちu64の大きさでIdentifierを保存してほしくないから
    u16範囲内で抑えられる文字列の長さを指定しておけば変数名を保存する際u16で足りる

*/
const MAX_IDENTIFIER_LENGTH: usize = 530;

pub struct Parser<'tok> {
    tokens: &'tok Vec<Token>,
    ast: ParsedResult,
    current: usize,
    start_line: usize,
    current_line: usize,
    assign_count: usize,
    block_count: usize,
}

impl<'tok> Parser<'tok> {
    pub fn new(_tok: &'tok Vec<Token>) -> Self {
        Self {
            tokens: _tok,
            ast: ParsedResult {
                ast: vec![],
                stmt: vec![],
                expr: vec![],
                functions: vec![],
            },
            current: 0,
            start_line: 0,
            current_line: 0,
            assign_count: 0,
            block_count: 0,
        }
    }

    pub fn parse(mut self) -> Result<ParsedResult, Error> {
        while !self.is_at_end() {
            self.start_line = self.current_line;
            let declaration = self.declaration()?;
            let codespan = CodeSpan::new(self.start_line, self.current_line);
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
        wrapped_func: fn(&mut Parser<'tok>, &mut T) -> Result<(), Error>
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

    fn declaration(&mut self) -> Result<StmtId, Error> {
        if self.is(TokenType::Iden) && self.is_next(TokenType::Colon) {
            return self.parse_variable();
        }

        self.statement()
    }

    fn expression(&mut self) -> Result<ExprId, Error> {
        let x = self.assignment();
        x
    }

    fn assignment(&mut self) -> Result<ExprId, Error> {
        let expr = self.logical_or()?;

        // @Improvement - 左辺値のハンドリングをもっとまともに出来れば良いかも知れない
        if self.is(TokenType::Equal) {
            let assign_span = self.advance().span;
            let value = self.assignment()?;

            // FIXME - @DumbCode: 借用不可の筈 ParserじゃなくCodegenでチェックしたほうが良いかも
            // 普通に借用できてしまったけどまあ当たり前ながら意図した動作ではなかった
            if let Expr::Variable(s) = self.ast.get_expr(expr) {
                let expr = self.ast.add_expr(Expr::Assign(expr, value));
                return Ok(expr);
            } else {
                return Err(Error::new_while_parsing(
                    "Invalid Assignment target",
                    CodeSpan::oneline(assign_span.start_usize()),
                ));
            }
        }

        Ok(expr)
    }

    pub fn logical_or(&mut self) -> Result<ExprId, Error> {
        let mut expr = self.logical_and()?;

        while self.is(TokenType::Or) {
            self.advance();
            let right = self.logical_and()?;
            expr = self.ast.add_expr(Expr::Logical(expr, right, Operator::Or));
        }

        Ok(expr)
    }
    pub fn logical_and(&mut self) -> Result<ExprId, Error> {
        let mut expr = self.equality()?;

        while self.is(TokenType::And) {
            self.advance();
            let right = self.equality()?;
            expr = self.ast.add_expr(Expr::Logical(expr, right, Operator::And));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<ExprId, Error> {
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

    fn comparison(&mut self) -> Result<ExprId, Error> {
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

    fn addition(&mut self) -> Result<ExprId, Error> {
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

    fn multiplification(&mut self) -> Result<ExprId, Error> {
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

    fn unary(&mut self) -> Result<ExprId, Error> {
        let operator = match self.get_current().tokentype {
            TokenType::Bang     => Some(Operator::Not),
            TokenType::Minus    => Some(Operator::Neg),
            TokenType::Question => Some(Operator::Wrap),
            TokenType::Caret    => Some(Operator::Ref),
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

    fn postfix(&mut self) -> Result<ExprId, Error> {
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

    fn parse_unwrap(&mut self, e: ExprId) -> Result<ExprId, Error> {
        let span = self.get_current().span;
        Err(Error::new_while_parsing("Unimplemented Unwrap.", span))
    }

    fn parse_deref(&mut self, e: ExprId) -> Result<ExprId, Error> {
        let span = self.get_current().span;
        Err(Error::new_while_parsing("Unimplemented Deref.", span))
    }

    fn parse_array_ref(&mut self, e: ExprId) -> Result<ExprId, Error> {
        let span = self.get_current().span;
        Err(Error::new_while_parsing("Unimplemented ArrayRef.", span))
    }


    fn parse_func_call(&mut self, expr: ExprId) -> Result<ExprId, Error> {
        self.consume(TokenType::OpenParen)?;
        let mut v = Vec::<ExprId>::new();

        if !self.is(TokenType::CloseParen) {
            let mut z = true;
            while z {
                v.push(self.expression()?);
                z = self.consume(TokenType::Comma).is_ok();
            }
        }
        let paren = self.consume(TokenType::CloseParen)?;
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

    fn get_current(&mut self) -> &Token {
        let x = self.tokens.get(self.current).unwrap();
        self.current_line = x.span.end_usize();
        x
    }

    fn get_previous(&mut self) -> &Token {
        let x = self.tokens.get(self.current - 1).unwrap();
        self.current_line = x.span.end_usize();
        x
    }

    fn get_next(&mut self) -> &Token {
        if self.current + 1 >= self.tokens.len() {
            return self.get_current();
        }
        let x = self.tokens.get(self.current + 1).unwrap();
        self.current_line = x.span.end_usize();
        x
    }

    fn advance(&mut self) -> &Token {
        self.current += 1;
        let x = self.tokens.get(self.current - 1).unwrap();
        self.current_line = x.span.end_usize();
        x
    }

    fn primary(&mut self) -> Result<ExprId, Error> {
        let previous_line = self.current_line;
        let inside = self.advance();
        use TokenType::*;
        let result = match &inside.tokentype {
            Digit => {
                let inside_lexeme = inside.lexeme.clone().unwrap();
                let contain_dot = inside_lexeme.contains(".");
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
                                let formatted = format!(
                                    "Identifier maximum length exceeded: {}, max length is {}",
                                    inside_lexeme.len(),
                                    MAX_IDENTIFIER_LENGTH
                                );
                                return Err(Error::new_while_parsing(
                                    formatted.as_str(),
                                    CodeSpan::new(previous_line, self.current_line)
                                ));
                            }
                            let expr = Expr::Variable(inside_lexeme.to_owned());
                            let expr_id = self.ast.add_expr(expr);
                            Ok(expr_id)
                        }
                    }
                } else {
                    unreachable!();
                }
            }
            OpenParen => {
                let inside_paren = self.expression()?;
                let closed_paren = self.consume(CloseParen)?;
                let expr = Expr::Grouping(inside_paren);
                Ok(self.ast.add_expr(expr))
            }
            _s => Err(Error::new_while_parsing(
                format!("Received unknown token while parsing code: {:?}", _s).as_str(),
                CodeSpan::new(previous_line, self.current_line)
            )),
        };

        result
    }

    fn consume(&mut self, until: TokenType) -> Result<&Token, Error> {
        // TODO - @DumbCode: 無駄なクローンを許すな
        let x = until.clone();

        if self.is(until) {
            return Ok(self.advance());
        }

        let current = self.get_current();
        let formatted = format!(
            "Tried to consume {:?}, got {:?} at range {} - {}",
            x, current.tokentype, current.span.start, current.span.end
        );
        Err(Error::new_while_parsing(formatted.as_str(), current.span))
    }
}
