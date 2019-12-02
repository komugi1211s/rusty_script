
use super::{
    types::{ Value, Type },
    token::{ TokenType, Token },
    parse::{ ParsedData, Expr, Statement },
};


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


pub struct Parser
{
    tokens: Vec<Token>,
    current: usize
}


fn util_string_to_u16(string: &str) -> u16
{
    string.as_bytes().iter().map(|x| *x as u16).sum()
}

impl Parser
{
    pub fn new(_tok: Vec<Token>) -> Self
    {
        Self { tokens: _tok, current: 0 }
    }

    pub fn parse(&mut self) -> Vec<ParsedData>
    {
        let mut statements: Vec<ParsedData> = Vec::new();
        while !self.is_at_end()
        {
            let current_line = self.tokens[self.current].line;
            let x = self.decralation();
            statements.push(ParsedData::new(x, current_line));
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
        let possible_stmt = self.get_current();
        let result = match possible_stmt.tokentype
        {

            // Close Bracket Expected, They'll handle the close bracket themselves
            // so no need for check
            TokenType::If => { self.advance(); return self.if_statement(); },
            TokenType::While => { self.advance(); return self.while_statement(); },
            TokenType::OpenBrace => { self.advance(); return Statement::Block(self.block()); },

            // Semicolon Expected, I have to handle it here
            TokenType::Print => { self.advance(); Statement::Print(self.expression()) },
            TokenType::Return => { self.advance(); self.return_statement() },
            TokenType::Break => { self.advance(); Statement::Break },
            TokenType::Continue => { self.advance(); Statement::Continue },
            _  => Statement::Expression(self.expression()),
        };

        match self.consume(TokenType::SemiColon)
        {
            Ok(_) => result,
            Err(e) => unreachable!("SemiColon Expected, got {:?}", e),
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

    fn while_statement(&mut self) -> Statement
    {
        let condition = self.expression();
        let _loop = self.statement();

        return Statement::While(condition, Box::new(_loop));
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

    fn return_statement(&mut self) -> Statement
    {
        let mut result = Expr::Literal(Value::Null);
        if !self.is(TokenType::SemiColon)
        {
            result = self.expression();
        }

        return Statement::Return(result);
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
        let x = self.get_current();
        if TokenType::is_typekind(&x.tokentype)
        {
            return self.declare_variable();
        }

        return self.statement();
    }

    fn get_variable_type_and_identifier(&mut self) -> (Type, String)
    {
        let _type = self.advance();
        let mut _type = Type::from_tokentype(&_type.tokentype);
        if self.is(TokenType::Question)
        {
            self.advance();
            _type.insert(Type::Null);
        }

        let should_be_colon = self.consume(TokenType::Colon).expect("Expected Colon, Got Something Different");
        let possible_iden = self.advance();

        if TokenType::Iden != possible_iden.tokentype
        {
            panic!("Identity Expected, got {:?}", possible_iden);
        }


        (_type, possible_iden.lexeme.clone())
    }

    fn declare_argument(&mut self) -> Vec<Statement>
    {
        self.consume(TokenType::OpenParen).expect("Why it failed?");
        let mut arguments: Vec<Statement> = Vec::new();

        while TokenType::is_typekind(&self.get_current().tokentype)
        {
            let (_type, iden) = self.get_variable_type_and_identifier();
            let iden_id = util_string_to_u16(&iden);
            let bridge_token = self.advance();
            match bridge_token.tokentype
            {
                TokenType::Equal => {
                    let item = self.expression();
                    println!("{}", item);
                    arguments.push(Statement::Decralation(iden_id, _type, item));
                    match self.advance().tokentype
                    {
                        TokenType::Comma => continue,
                        TokenType::CloseParen => return arguments,
                        _ => unreachable!("Function definition must end with Close Paren"),
                    }
                },
                TokenType::Comma =>
                {
                    arguments.push(Statement::Decralation(iden_id, _type, Expr::Literal(Value::Null)));
                    continue;
                },
                TokenType::CloseParen =>
                { 
                    arguments.push(Statement::Decralation(iden_id, _type, Expr::Literal(Value::Null)));
                    return arguments;
                },
                _ => unreachable!("About to declare argument, found {:?}", bridge_token)
            }
        }
        if self.is(TokenType::CloseParen)
        {
            self.consume(TokenType::CloseParen);
        }
        arguments
    }

    fn declare_variable(&mut self) -> Statement
    {
        /*
         * NOTE: The difference between "Variable" "Function" "Argument" are really tough to
         * understand, so I'll leave a note here.
         *
         * Declaration starts - 
         *  TYPE: IDENTIFIER
         *     Variable - TYPE: IDENTIFIER;
         *     Function - TYPE: IDENTIFIER(
         *         Argument - TYPE: IDENTIFIER,
         *         Argument - TYPE: IDENTIFIER = X,
         *     Variable - TYPE: IDENTIFIER = X;
         *
         * */
        let (_type, iden) = self.get_variable_type_and_identifier();
        let iden_id = util_string_to_u16(&iden);
        // self.current += 1;

        let mut state = Statement::Empty;

        // Initialization, Outside
        if self.is(TokenType::Equal)
        {
            self.current += 1;
            let item = self.expression();
            self.consume(TokenType::SemiColon).expect("ParserError: Expected After Decralation.");
            state = Statement::Decralation(iden_id, _type, item);
        }
        // Declaration, Outside
        else if self.is(TokenType::SemiColon)
        {
            self.current += 1;
            if !_type.is_nullable() {
                panic!("Uninitialized non-nullable variable: {}", iden);
            }
            return Statement::Decralation(iden_id, _type, Expr::Literal(Value::Null));
        }

        // This is a function.
        else if self.is(TokenType::OpenParen)
        {
            // Declaration of Functions
            let arguments = self.declare_argument();

            let inside_func = self.statement();
            return Statement::Function(iden_id, _type, arguments, Box::new(inside_func));
        }

        match state
        {
            Statement::Empty => panic!("Failed to parse the declaration process."),
            _ => state,
        }
    }

    fn expression(&mut self) -> Expr
    {
        let x = self.assignment();
        x
    }

    fn assignment(&mut self) -> Expr
    {
        let mut expr = self.logical_or();
        if self.is(TokenType::Equal)
        {
            self.advance();
            let value = self.assignment();
            
            if let Expr::Variable(s) = expr {
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
            let operator = self.advance().clone();
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
            let operator = self.get_current().clone();
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
            && match self.get_current().tokentype {
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
        let mut v = Vec::<Expr>::new();
        self.current += 1;

        if !self.is(TokenType::CloseParen) 
        {
            let mut z = true;
            while z {
                v.push(self.expression());
                z = self.consume(TokenType::Comma).is_ok();
            }
        }
        let paren = self.consume(TokenType::CloseParen).expect("Close Parentheses Expected.");
        return Expr::FunctionCall(Box::new(_expr), paren.clone(), v);
    }

    fn is(&self, _type: TokenType) -> bool
    {
        (!self.is_at_end() &&
            self.get_current().tokentype == _type)
    }

    fn get_current(&self) -> &Token
    {
        self.tokens.get(self.current).unwrap()
    }

    fn advance(&mut self) -> &Token
    {
        self.current += 1;
        self.tokens.get(self.current-1).unwrap()
    }

    fn primary(&mut self) -> Expr
    {
        let inside = self.advance();
        use TokenType::*;
        let result = match &inside.tokentype
        {
            False => {
                Expr::Literal(Value::Boolean(false))
            },
            True => 
            {
                Expr::Literal(Value::Boolean(true))
            },
            Null =>
            {
                // TODO:
                // This is a bug.
                // once something gets initialized with Null, that variable becomes "Any" type.
                Expr::Literal(Value::Null)
            },
            Digit => {

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
                Expr::Literal(Value::Str(inside.lexeme.to_string()))
            },
            Iden => {
                if inside.lexeme.len() > MAX_IDENTIFIER_LENGTH {
                    panic!("Identifier maximum length exceeded: {}, max length is {}", inside.lexeme.len(), MAX_IDENTIFIER_LENGTH);
                }
                Expr::Variable(util_string_to_u16(&inside.lexeme))
            },
            OpenParen => {
                let inside_paren = self.expression();
                let closed_paren = self.consume(CloseParen);
                if closed_paren.is_err() {
                    panic!("ParserError: We could not find a closed Paren! current: {}, current_Token: {}", self.current, self.get_current());
                }
                else {
                    Expr::Grouping(Box::new(inside_paren))
                }
            },
            s => 
            {
                if TokenType::is_typekind(s)
                {
                    Expr::Literal(Value::Type(Type::from_tokentype(s)))
                }
                else
                {
                    unreachable!("ParserError: while Handling Primary: Token {:?}", inside);
                }
            },
        };

        result
    }

    fn consume(&mut self, until: TokenType) -> Result<&Token, &Token>
    {
        if self.is(until) {
            return Ok(self.advance());
        }

        return Err(self.get_current());
    }
}
