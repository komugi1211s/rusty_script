
use super::{
    types::{ Value },
    token::{ TokenType, Token },
    parse::{ Expr, Statement },
};


// TODO:
// All clone call to a lifetime management
// More refined Match system™
// Refactor

// FIXME:
// Primary + ; does not work ( 10; spits error )

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
        if self.is(TokenType::Print) 
        {
            self.current += 1;
            let statement = Statement::Print(self.expression());
            return statement;
        }
        else if self.is(TokenType::OpenBrace)
        {
            self.current += 1;
            return Statement::Block(self.block());
        }
        else if self.is(TokenType::If)
        {
            self.current += 1;
            return self.if_statement();
        }
        else
        {
            return Statement::Expression(self.expression());
        }
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
        if self.is(TokenType::TypeAny) 
            || self.is(TokenType::TypeInt) 
            || self.is(TokenType::TypeFloat) 
            || self.is(TokenType::TypeBool) 
            || self.is(TokenType::TypeStr) 
            // || self.is(TokenType::TypeVoid)
        {
            return self.declare_variable();
        }

        return self.statement();
    }

    fn declare_variable(&mut self) -> Statement
    {
        let _type = self.tokens.get(self.current).unwrap().clone();
        let _type = Type::from_token(_type);

        self.current += 1;

        let should_be_colon = self.consume(TokenType::Colon);
        if should_be_colon.is_err() 
        { 
            panic!("Expected Colon, got: {:?}", self.tokens.get(self.current)) 
        }

        let possible_iden = self.tokens.get(self.current).unwrap();
        if let TokenType::Iden(iden) = &possible_iden.tokentype {
            let iden = iden.clone();
            self.current += 1;
            if self.is(TokenType::Equal)
            {
                self.current += 1;
                let item = self.expression();
                // self.consume(TokenType::SemiColon).expect("ParserError: Expected SemiColon.");
                return Statement::Decralation(iden, _type.tokentype, item);
            }
            else if self.is(TokenType::SemiColon)
            {
                self.current += 1;
                return Statement::Decralation(iden, _type.tokentype, Expr::Literal(Value::Null));
            }

            unreachable!("ParserError: Expected Equal, got: {:?}", self.tokens.get(self.current));
        }

        unreachable!("ParserError: Expected Identity, got: {:?}", possible_iden.tokentype);
    }

    fn expression(&mut self) -> Expr
    {
        let x = self.assignment();
        println!("{:?}", &x);
        self.consume(TokenType::SemiColon).expect("ParserError: Expected SemiColon after Expression.");
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
                // self.consume(TokenType::SemiColon).expect("ParserError: Expected SemiColon.");
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
        
        while self.is(TokenType::Slash) || self.is(TokenType::Asterisk)
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

        return self.primary();
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
            Digit(i) => {
                self.current += 1;

                /*
                    TODO:
                    This is a bug.
                    checking if the digit is int or not by using this method(trunc) is dangerous
                */
                if i.trunc() == *i {
                    Expr::Literal(Value::Int(*i as i64))
                }
                else
                {
                    Expr::Literal(Value::Float(*i))
                }
            },
            Str(s) => {
                self.current += 1;
                Expr::Literal(Value::Str(s.to_string()))
            },
            Iden(s) => {
                self.current += 1;
                Expr::Variable(s.to_string())
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
