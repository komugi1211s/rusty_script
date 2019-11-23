
use super::{
    token::{ TokenType, Token },
    parse::{ Expr },
};

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

    pub fn parse(&mut self) {
        while !self.is_at_end() {
        }
    }

    fn peek(&self) -> Token {
        self.tokens[self.current]
    }

    fn advance(&self) -> Token
    {
    }

    fn previous(&self) -> Token {
        
        self.tokens[self.current]
    }
}
