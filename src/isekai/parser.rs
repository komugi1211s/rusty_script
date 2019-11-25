
use super::{
    types::{ Types },
    token::{ TokenType, Token },
    parse::{ Expr, Statement },
};

// TODO:
// Why only one line comes back from parse?
// All clone call to a lifetime management
// More refined Match system™
// Refactor

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
            statements.push(self.statement());
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
        // TODO: fix this long conditional
        if self.is(TokenType::Var) || self.is(TokenType::TypeInt) || self.is(TokenType::TypeFloat) || self.is(TokenType::TypeBool) || self.is(TokenType::TypeStr) || self.is(TokenType::TypeVoid)
        {
            return self.decralation();
        }
        
        return Statement::Expression(self.expression());
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
        let _type = self.tokens.get(self.current).unwrap().clone();
        let should_be_colon = self.tokens.get(self.current + 1).unwrap();
        let possible_iden = self.tokens.get(self.current + 2).unwrap().clone();

        if let TokenType::Iden(iden) = possible_iden.tokentype {
            let iden = iden.clone();
            self.current += 3;
            if self.is(TokenType::Equal)
            {
                self.current += 1;
                let item = self.expression();
                return Statement::Decralation(iden, _type.tokentype, item);
            }

            unreachable!("Expected Equal, got: {:?}", self.tokens.get(self.current));
        }

        unreachable!("Expected Identity, got: {:?}", possible_iden.tokentype);
    }

    fn expression(&mut self) -> Expr
    {
        let x = self.equality();
        x
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

        return expr;
    }

    fn comparison(&mut self) -> Expr
    {
        let mut start = self.addition();
        
        use TokenType::*;
        // 分かりづらッ！！
        while (!self.is_at_end() 
            && match &self.tokens.get(self.current).unwrap().tokentype {
                    LessEqual => true,
                    MoreEqual => true,
                    Less      => true,
                    More      => true,
                    _other    => false,
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
                Expr::Literal(Types::Boolean(false))
            },
            True => 
            {
                self.current += 1;
                Expr::Literal(Types::Boolean(true))
            }
            Digit(i) => {
                self.current += 1;
                if i.trunc() == *i {
                    Expr::Literal(Types::Int(*i as i64))
                }
                else
                {
                    Expr::Literal(Types::Float(*i))
                }
            },
            Str(s) => {
                self.current += 1;
                Expr::Literal(Types::Str(s.to_string()))
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
                    panic!("We could not find a closed Paren! current: {}, current_Token: {}", self.current, self.tokens.get(self.current).unwrap());
                }
                else {
                    Expr::Grouping(Box::new(inside_paren))
                }
            },
            s => 
            {
                unreachable!(" while Handling Primary: Token {:?}", s)
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
