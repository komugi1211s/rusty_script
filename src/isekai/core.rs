use super::token::{ Token, TokenType };
use super::tokenizer::{ Tokenizer, SyntaxError };
use super::parse::{ Visitor, Expr, Statement };
use super::parser::Parser;
use super::evaluate::Interpreter;
use super::types::Value;
// use self::error::*;

use std::collections::HashMap;
use std::time::{ Instant, Duration };

pub fn start(code: &str) -> Result<(), SyntaxError>
{
    let start = Instant::now();
    let _tokens: Vec<Token> = Tokenizer::new(code).scan()?;
    println!("Tokenizer took: {} ns", start.elapsed().as_nanos());

    let mut _parser: Parser = Parser::new(_tokens);

    let parser_time = Instant::now();
    let result = _parser.parse();
    println!("Parser took: {} ns", parser_time.elapsed().as_nanos());

    let mut interpreter = Interpreter::new();
    let interp_time = Instant::now();
    for i in &result 
    {
        interpreter.visit(i);
    }
    println!("Interpreter took: {} ns", interp_time.elapsed().as_nanos());

    println!("TOTAL took: {} ms", start.elapsed().as_nanos());
    Ok(())
}
