
use super::{
    types::{ Value, Type },
    token::{ TokenType, Token },
    parse::{ Expr, Statement },
};


// TODO:
// All clone call to a lifetime management
// More refined Match system™
// Refactor

// FIXME:
// Primary + ; does not work ( 10; spits error ) -> I think it fixed?


pub struct Parser
{
    tokens: Vec<Token>,
    current: usize
}

impl Parser
{
    pub fn new(_tok: Vec<Token>) -> Self
    {
        Self { tokens: _tok, current: 0 }
    }

    pub fn parse(&mut self) -> Vec<Statement>
    {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end()
        {
            statements.push(self.decralation());
        }
        statements
    }

    fn is_at_end(&self) -> bool
    {
        let current_token = self.tokens.get(self.current);
        match current_token 
        {
            None => true,
            Some(inside) => inside.tokentype == TokenType::EOF
        }
    }

    fn statement(&mut self) -> Statement
    {
        if self.consume(TokenType::Print).is_ok()
        {
            let statement = Statement::Print(self.expression());
            return statement;
        }
        else if self.consume(TokenType::OpenBrace).is_ok()
        {
            return Statement::Block(self.block());
        }
        else if self.consume(TokenType::If).is_ok()
        {
            return self.if_statement();
        }
        else if self.consume(TokenType::While).is_ok()
        {
            return self.while_statement();
        }
        else if self.consume(TokenType::Break).is_ok()
        {
            self.current += 1;
            return Statement::Break;
        }
        else if self.consume(TokenType::Continue).is_ok()
        {
            self.current += 1;
            return Statement::Continue;
        }
        else
        {
            return Statement::Expression(self.expression());
        }
    }

    fn while_statement(&mut self) -> Statement
    {
        let condition = self.expression();
        let _loop = self.statement();

        return Statement::While(condition, Box::new(_loop));
    }

    fn if_statement(&mut self) -> Statement
    {
        let condition = self.expression();
        let true_route = self.statement();
        
        return Statement::If(condition,
            Box::new(true_route),
            match self.is(TokenType::Else) {
                true => 
                {
                    self.current += 1;
                    Some(Box::new(self.statement()))
                },
                false => None,
            });
    }

    fn block(&mut self) -> Vec<Statement>
    {
        let mut vector = Vec::new();
        
        while !self.is_at_end() && !self.is(TokenType::CloseBrace) 
        {
            vector.push(self.decralation());
        }

        self.consume(TokenType::CloseBrace).expect("ParserError: Expected Close Bracket");
        vector
    }
    
    fn decralation(&mut self) -> Statement
    {
        // Called at [type]: iden = value
        //           ^ right here
        //
        // self.current     = typename ( or var )
        // self.current + 1 = colon
        // self.current + 2 = identity
        // self.current + 3 = equal
        // self.current + 4 = value
        //
        let x = self.tokens.get(self.current).unwrap();
        if TokenType::is_typekind(&x.tokentype)
        {
            return self.declare_variable();
        }

        return self.statement();
    }

    fn declare_variable(&mut self) -> Statement
    {
        let _type = self.tokens.get(self.current).unwrap();
        let _type = Type::from_tokentype(&_type.tokentype);

        self.current += 1;

        let should_be_colon = self.consume(TokenType::Colon);
        if should_be_colon.is_err() 
        { 
            panic!("Expected Colon, got: {:?}", self.tokens.get(self.current)) 
        }

        let possible_iden = self.tokens.get(self.current).unwrap();
        if TokenType::Iden == possible_iden.tokentype {
            let iden = possible_iden.lexeme.clone();
            self.current += 1;
            if self.is(TokenType::Equal)
            {
                self.current += 1;
                let item = self.expression();
                // self.consume(TokenType::SemiColon).expect("ParserError: Expected After Decralation.");
                return Statement::Decralation(iden, _type, item);
            }
            else if self.is(TokenType::SemiColon)
            {
                self.current += 1;
                return Statement::Decralation(iden, _type, Expr::Literal(Value::Null));
            }
            else if self.is(TokenType::OpenParen)
            {
                // Declaration of Functions
                self.consume(TokenType::OpenParen).expect("Why it failed?");
                let mut argument_is_type = {
                    let x = self.tokens.get(self.current);
                    x.is_some() && TokenType::is_typekind(&x.unwrap().tokentype)
                };

                let mut arguments: Vec<(Type, Token)> = Vec::new();
                while argument_is_type {
                    self.current += 1;
                    let colon_ = self.consume(TokenType::Colon).expect("Colon Expected for Argument.");
                    let argument_iden = self.consume(TokenType::Iden).expect("Identity Expected for Argument");
                    
                }
            }

            unreachable!("ParserError: Expected Equal, got: {:?}", self.tokens.get(self.current));
        }

        unreachable!("ParserError: Expected Identity, got: {:?}", possible_iden.tokentype);
    }

    fn expression(&mut self) -> Expr
    {
        let x = self.assignment();
        // println!("{:?}", &x);
        self.consume(TokenType::SemiColon);
        x
    }

    fn assignment(&mut self) -> Expr
    {
        let mut expr = self.logical_or();

        if self.is(TokenType::Equal)
        {
            self.current += 1;
            let variable_name = self.tokens.get(self.current - 1).unwrap();
            let value = self.assignment();
            
            if let Expr::Variable(s) = expr {
                self.consume(TokenType::SemiColon).expect("ParserError: Expected Semicolon after Assignment.");
                return Expr::Assign(s, Box::new(value));
            }
            else
            {
                unreachable!("ParserError: Invalid Assignment Target: {:?}", expr);
            }
        }

        return expr;
    }

    pub fn logical_or(&mut self) -> Expr
    {
        let mut expr = self.logical_and();

        while self.is(TokenType::Or)
        {
            let operator = self.tokens.get(self.current).unwrap().clone();
            self.current += 1;
            let right = self.logical_and();
            expr = Expr::Logical(Box::new(expr), Box::new(right), operator);
        }
        
        expr
    }
    pub fn logical_and(&mut self) -> Expr
    {
        let mut expr = self.equality();

        while self.is(TokenType::And)
        {
            let operator = self.tokens.get(self.current).unwrap().clone();
            self.current += 1;
            let right = self.equality();
            expr = Expr::Logical(Box::new(expr), Box::new(right), operator);
        }
        
        expr
    }

    fn equality(&mut self) -> Expr 
    {
        let mut expr = self.comparison();
        while self.is(TokenType::EqualEqual) || self.is(TokenType::NotEqual)
        {
            // 一歩進めてからオペレータを取る
            self.current += 1;
            let equal_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(expr), Box::new(right), equal_oper);
        }

        expr
    }

    fn comparison(&mut self) -> Expr
    {
        let mut start = self.addition();
        
        use TokenType::*;
        // 分かりづらッ！！
        while (!self.is_at_end() 
            && match self.tokens.get(self.current).unwrap().tokentype {
                    LessEqual => true,
                    MoreEqual => true,
                    Less      => true,
                    More      => true,
                    _         => false,
                }
        )
        {
            self.current += 1;
            let compare_operator = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.addition();
            start = Expr::Binary(Box::new(start), Box::new(right), compare_operator);
        }

        return start;
    }

    fn addition(&mut self) -> Expr
    {
        let mut start = self.multiplification();

        while self.is(TokenType::Plus) || self.is(TokenType::Minus)
        {
            self.current += 1;
            let addition_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.multiplification();

            start = Expr::Binary(Box::new(start), Box::new(right), addition_oper);
        }

        return start;
    }

    fn multiplification(&mut self) -> Expr
    {
        let mut start = self.unary();
        
        while self.is(TokenType::Slash) || self.is(TokenType::Asterisk) || self.is(TokenType::Percent)
        {
            self.current += 1;
            let multiply_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.unary();

            start = Expr::Binary(Box::new(start), Box::new(right), multiply_oper);
        }

        return start;
    }

    fn unary(&mut self) -> Expr
    {
        if self.is(TokenType::Bang) || self.is(TokenType::Minus)
        {
            self.current += 1;
            let unary_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.unary();
            let unary = Expr::Unary(Box::new(right), unary_oper);

            
            return unary;
        }

        return self.func_call();
    }

    fn func_call(&mut self) -> Expr
    {
        let mut expr = self.primary();

        while true {
            if self.is(TokenType::OpenParen) {
                expr = self.finish_func_call(expr);
            } else {
                break;
            }
        }
        expr
    }

    fn finish_func_call(&mut self, _expr: Expr) -> Expr
    {
        let v = Vec::new<Expr>();
        self.current += 1;

        if !self.is(TokenType::CloseParen) 
        {
            let mut z = true;
            while z {
                self.current += 1;
                v.push(self.expression());
                z = self.is(TokenType::Comma);
            }
        }
    }

    fn is(&self, _type: TokenType) -> bool
    {
        (!self.is_at_end() &&
            self.tokens.get(self.current).unwrap().tokentype == _type)
    }

    fn primary(&mut self) -> Expr
    {
        let inside = self.tokens.get(self.current).unwrap();
        use TokenType::*;
        let result = match &inside.tokentype
        {
            False => {
                self.current += 1;
                Expr::Literal(Value::Boolean(false))
            },
            True => 
            {
                self.current += 1;
                Expr::Literal(Value::Boolean(true))
            },
            Null =>
            {
                // TODO:
                // This is a bug.
                // once something gets initialized with Null, that variable becomes "Any" type.
                self.current += 1;
                Expr::Literal(Value::Null)
            },
            Digit => {
                self.current += 1;

                /*
                    TODO:
                    This is a bug.
                    checking if the digit is int or not by using this method(trunc) is dangerous
                */
                match inside.lexeme.parse::<i64>() {
                    Ok(n) => Expr::Literal(Value::Int(n)),
                    Err(_) => Expr::Literal(Value::Float(inside.lexeme.parse::<f64>().unwrap()))
                }
            },
            Str => {
                self.current += 1;
                Expr::Literal(Value::Str(inside.lexeme.to_string()))
            },
            Iden => {
                self.current += 1;
                Expr::Variable(inside.lexeme.to_string())
            },
            OpenParen => {
                self.current += 1;
                let inside_paren = self.expression();
                let closed_paren = self.consume(CloseParen);
                if closed_paren.is_err() {
                    panic!("ParserError: We could not find a closed Paren! current: {}, current_Token: {}", self.current, self.tokens.get(self.current).unwrap());
                }
                else {
                    Expr::Grouping(Box::new(inside_paren))
                }
            },
            s => 
            {
                unreachable!("ParserError: while Handling Primary: Token {:?}", s)
            },
        };

        result
    }

    fn consume(&mut self, until: TokenType) -> Result<&Token, ()>
    {
        if self.is(until) {
            self.current += 1;
            return Ok(self.tokens.get(self.current).unwrap());
        }

        return Err(())
    }
}
