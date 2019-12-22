use super::{
    parse::{
        BlockData, DeclarationData, Expr, FunctionData,
        StatementNode,
        ParsedResult, 
        Statement,
    },
    token::{Token, TokenType},
    types::{Constant, Type, TypeKind, TypeOption},
    report::Error,
    utils,
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

pub struct Parser<'tok> {
    tokens: &'tok Vec<Token>,
    parsed_result: ParsedResult,
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
            parsed_result: ParsedResult { functions: vec![], statements: vec![] },
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
            self.parsed_result.statements.push(StatementNode::new(declaration, self.start_line));
        }
        Ok(self.parsed_result)
    }

    fn is_at_end(&self) -> bool {
        let current_token = self.tokens.get(self.current);
        match current_token {
            None => true,
            Some(inside) => inside.tokentype == TokenType::EOF,
        }
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        let possible_stmt = self.get_current();
        let result = match possible_stmt.tokentype {
            // Close Bracket Expected, They'll handle the close bracket themselves
            // so no need for check
            TokenType::If => {
                self.advance();
                return self.if_statement();
            }
            TokenType::While => {
                self.advance();
                return self.while_statement();
            }
            TokenType::OpenBrace => {
                // Keep track of local assignment
                self.advance();
                let block = self.block()?;
                return Ok(Statement::Block(block));
            }

            // Semicolon Expected, I have to handle it here
            // Go through, no early return
            TokenType::Print => {
                self.advance();
                Ok(Statement::Print(self.expression()?))
            }
            TokenType::Return => {
                self.advance();
                self.return_statement()
            }
            TokenType::Break => {
                self.advance();
                Ok(Statement::Break)
            }
            TokenType::Continue => {
                self.advance();
                Ok(Statement::Continue)
            }
            _ => Ok(Statement::Expression(self.expression()?)),
        };

        self.consume(TokenType::SemiColon)?; 
        result
    }

    fn if_statement(&mut self) -> Result<Statement, Error> {
        let condition = self.expression()?;
        let true_route = self.statement()?;

        Ok(Statement::If(
            condition,
            Box::new(true_route),
            if self.is(TokenType::Else) {
                self.current += 1;
                Some(Box::new(self.statement()?))
            } else {
                None
            },
        ))
    }

    fn while_statement(&mut self) -> Result<Statement, Error> {
        let condition = self.expression()?;
        let _loop = self.statement()?;

        Ok(Statement::While(condition, Box::new(_loop)))
    }

    fn block(&mut self) -> Result<BlockData, Error> {
        let previous_count = self.assign_count;
        self.assign_count = 0;
        self.block_count += 1;
        let mut vector = Vec::new();
        while !self.is_at_end() && !self.is(TokenType::CloseBrace) {
            vector.push(self.declaration()?);
        }

        let local_assign_count = self.assign_count;
        self.assign_count = previous_count;
        self.block_count -= 1;

        self.consume(TokenType::CloseBrace)?;
        Ok(BlockData {
            statements: vector,
            local_count: local_assign_count,
        })
    }

    fn return_statement(&mut self) -> Result<Statement, Error> {
        let mut result = None;
        if !self.is(TokenType::SemiColon) {
            result = Some(self.expression()?);
        }

        Ok(Statement::Return(result))
    }

    fn declaration(&mut self) -> Result<Statement, Error> {
        if self.is(TokenType::Iden) && self.is_next(TokenType::Colon) {
            return self.declare_variable();
        }

        self.statement()
    }

    fn initialize_decl_data(&mut self) -> Result<DeclarationData, Error> {
        let identity = { self.get_previous().lexeme.clone() };
        let colon = self.consume(TokenType::Colon)?;

        let mut decl_data = DeclarationData {
            name: identity,
            _type: Type::default(),
            is_inferred: true,
            is_const: false,
            is_nullable: false,

            is_argument: false,
            expr: None,
        };

        if TokenType::is_typekind(&self.get_current().tokentype) {
            let type_token = self.advance();
            let mut dtype = Type::from_tokentype(&type_token.tokentype);
            let is_all_uppercase = type_token.lexeme.as_str().to_ascii_uppercase() == type_token.lexeme;

            decl_data._type = dtype;
            decl_data.is_inferred = false;

            if is_all_uppercase {
                decl_data.is_const = true;
            }

            if self.consume(TokenType::Question).is_ok() {
                decl_data.is_nullable = true;
            }

        }
        return Ok(decl_data);
    }

    fn declare_argument(&mut self) -> Result<Vec<DeclarationData>, Error> {
        self.consume(TokenType::OpenParen)?;
        let mut arguments: Vec<DeclarationData> = Vec::new();

        if self.is(TokenType::CloseParen) {
            self.consume(TokenType::CloseParen)?;
            return Ok(arguments);
        }

        while self.is(TokenType::Iden) {
            self.current += 1;
            let mut decl_info = self.initialize_decl_data()?;
            decl_info.is_argument = true;

            let bridge_token = self.advance();
            let bridge_line = bridge_token.line;
            match bridge_token.tokentype {
                TokenType::Equal => {
                    let item = self.expression()?;
                    decl_info.expr = Some(item);
                    arguments.push(decl_info);

                    match self.advance().tokentype {
                        TokenType::Comma => continue,
                        TokenType::CloseParen => return Ok(arguments),
                        _ => return Err(Error::new_while_parsing("Argument declaration must end with close paren", self.current_line)),
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
                _ => return Err(Error::new_while_parsing("Unknown Token detected while parsing arguments", bridge_line)),
            }
        }
        Ok(arguments)
    }

    fn declare_variable(&mut self) -> Result<Statement, Error> {
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
        self.assign_count += 1;
        self.current += 1;
        let mut decl_info = self.initialize_decl_data()?;

        let mut state = Statement::Empty;
        // Initialization, Outside
        if self.consume(TokenType::Equal).is_ok() {
            let item = self.expression()?;
            decl_info.expr = Some(item);

            self.consume(TokenType::SemiColon)?;
            state = Statement::Decralation(decl_info);
        } else if self.consume(TokenType::SemiColon).is_ok() {
            // return if nullable
            if decl_info.is_nullable {
                state = Statement::Decralation(decl_info);
            } else {
                return Err(Error::new_while_parsing("tried to initialize non-null variable with null value", self.current_line))
            }
        }
        // This is a function.
        else if self.is(TokenType::OpenParen) {
            return self.declare_function(decl_info);
        }

        match state {
            Statement::Empty => Err(Error::new_while_parsing("Failed to parse the declaration process.", self.current_line)),
            _ => Ok(state),
        }
    }

    fn declare_function(&mut self, mut info: DeclarationData) -> Result<Statement, Error> {

        info._type.option.insert(TypeOption::Func);
        let arguments = self.declare_argument()?;

        self.consume(TokenType::OpenBrace)?;
        let inside_func = self.block()?;

        let data = FunctionData {
            it: info,
            args: arguments,
            block: inside_func,
        };
        self.parsed_result.functions.push(data);
        Ok(Statement::Function(self.parsed_result.functions.len() - 1))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        let x = self.assignment();
        x
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.logical_or()?;

        // @Improvement - 左辺値のハンドリングをもっとまともに出来れば良いかも知れない
        if self.is(TokenType::Equal) {
            let assign_line = self.advance().line;
            let value = self.assignment()?;

            if let Expr::Variable(s) = expr {
                return Ok(Expr::Assign(s, Box::new(value)));
            } else {
                return Err(Error::new_while_parsing("Invalid Assignment target", assign_line));
            }
        }

        Ok(expr)
    }

    pub fn logical_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.logical_and()?;

        while self.is(TokenType::Or) {
            let operator = self.advance().clone();
            let right = self.logical_and()?;
            expr = Expr::Logical(Box::new(expr), Box::new(right), operator);
        }

        Ok(expr)
    }
    pub fn logical_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.is(TokenType::And) {
            let operator = self.get_current().clone();
            self.current += 1;
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), Box::new(right), operator);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;
        while self.is(TokenType::EqualEqual) || self.is(TokenType::NotEqual) {
            // TODO - @Cleanup: advance?
            // 一歩進めてからオペレータを取る
            self.current += 1;
            let equal_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), Box::new(right), equal_oper);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut start = self.addition()?;

        use TokenType::*;
        // 分かりづらッ！！
        while !self.is_at_end()
            && match self.get_current().tokentype {
                LessEqual => true,
                MoreEqual => true,
                Less => true,
                More => true,
                _ => false,
            }
        {
            // TODO - @Cleanup: advance?
            self.current += 1;
            let compare_operator = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.addition()?;
            start = Expr::Binary(Box::new(start), Box::new(right), compare_operator);
        }

        Ok(start)
    }

    fn addition(&mut self) -> Result<Expr, Error> {
        let mut start = self.multiplification()?;

        while self.is(TokenType::Plus) || self.is(TokenType::Minus) {
            // TODO - @Cleanup: advance?
            self.current += 1;
            let addition_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.multiplification()?;

            start = Expr::Binary(Box::new(start), Box::new(right), addition_oper);
        }

        Ok(start)
    }

    fn multiplification(&mut self) -> Result<Expr, Error> {
        let mut start = self.unary()?;

        while self.is(TokenType::Slash)
            || self.is(TokenType::Asterisk)
            || self.is(TokenType::Percent)
        {
            self.current += 1;
            let multiply_oper = self.tokens.get(self.current - 1).unwrap().clone();
            let right = self.unary()?;

            start = Expr::Binary(Box::new(start), Box::new(right), multiply_oper);
        }

        Ok(start)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.is(TokenType::Bang) || self.is(TokenType::Minus) {
            let unary_oper = self.advance().clone();
            let right = self.unary()?;
            let unary = Expr::Unary(Box::new(right), unary_oper);

            return Ok(unary);
        }

        self.func_call()
    }

    fn func_call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.is(TokenType::OpenParen) {
                expr = self.finish_func_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_func_call(&mut self, _expr: Expr) -> Result<Expr, Error> {
        let mut v = Vec::<Expr>::new();
        self.current += 1;

        if !self.is(TokenType::CloseParen) {
            let mut z = true;
            while z {
                v.push(self.expression()?);
                z = self.consume(TokenType::Comma).is_ok();
            }
        }
        let paren = self.consume(TokenType::CloseParen)?;
        Ok(Expr::FunctionCall(Box::new(_expr), paren.clone(), v))
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
        self.current_line = x.line;
        x
    }

    fn get_previous(&mut self) -> &Token {
        let x = self.tokens.get(self.current - 1).unwrap();
        self.current_line = x.line;
        x
    }

    fn get_next(&mut self) -> &Token {
        if self.current + 1 >= self.tokens.len() {
            return self.get_current();
        }
        let x = self.tokens.get(self.current + 1).unwrap();
        self.current_line = x.line;
        x
    }

    fn advance(&mut self) -> &Token {
        self.current += 1;
        let x = self.tokens.get(self.current - 1).unwrap();
        self.current_line = x.line;
        x
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let previous_line = self.current_line;
        let inside = self.advance();
        use TokenType::*;
        let result = match &inside.tokentype {
            False => Ok(Expr::Literal(Constant::from(&false))),
            True => Ok(Expr::Literal(Constant::from(&true))),
            Null => {
                // TODO:
                // This is a bug.
                // once something gets initialized with Null, that variable becomes "Any" type.
                Ok(Expr::Literal(Constant::null()))
            }
            Digit => {
                match inside.lexeme.parse::<i64>() {
                    Ok(n) => Ok(Expr::Literal(Constant::from(&n))),
                    Err(_) => match inside.lexeme.parse::<f64>() {
                        Ok(f) => Ok(Expr::Literal(Constant::from(&f))),
                        Err(x) => Err(Error::new_while_parsing("Digit does not match either int or float", self.current_line)),
                    }
                }
            }
            Str => Ok(Expr::Literal(Constant::from(&inside.lexeme))),
            Iden => {
                if inside.lexeme.len() > MAX_IDENTIFIER_LENGTH {
                    let formatted = format!("Identifier maximum length exceeded: {}, max length is {}", inside.lexeme.len(), MAX_IDENTIFIER_LENGTH);
                    return Err(Error::new_while_parsing(formatted.as_str(), self.current_line));
                }
                Ok(Expr::Variable(inside.lexeme.clone()))
            }
            OpenParen => {
                let inside_paren = self.expression()?;
                let closed_paren = self.consume(CloseParen)?;
                Ok(Expr::Grouping(Box::new(inside_paren)))
            }
            _s => Err(Error::new_while_parsing(format!("Received unknown token while parsing code: {:?}", _s).as_str(), previous_line))
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
        let formatted = format!("Tried to consume {:?}, got {:?} at line {}", x, current.tokentype, current.line);
        Err(Error::new_while_parsing(formatted.as_str(), current.line))
    }
}