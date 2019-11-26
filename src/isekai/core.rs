use super::token::{ Token, TokenType };
use super::tokenizer::{ Tokenizer, SyntaxError };
use super::parse::{ Visitor, Expr, Statement };
use super::parser::Parser;
use super::evaluate::Interpreter;
use super::types::Types;
// use self::error::*;

use std::collections::HashMap;

pub fn start(code: &str) -> Result<(), SyntaxError>
{
    let _tokens: Vec<Token> = Tokenizer::new(code).scan()?;
    let mut _parser: Parser = Parser::new(_tokens);
    let mut interpreter = Interpreter::new();
    let result = _parser.parse();
    for i in &result 
    {
        interpreter.interpret(i);
    }

    Ok(())
}
