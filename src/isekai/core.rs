use super::token::Token;
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
    // println!("{}", &result);
    //
    for i in &result {
        match i {
            Statement::Expression(e) => println!("{}", interpreter.interpret(&e)),
            Statement::Decralation(_str, lit) => { 
                let literal = interpreter.interpret(&lit);
                interpreter.globals.insert(_str.to_string(), literal);
            },
            b => println!("Can't handle that right now: {:?}", b),
        }
    }

    Ok(())
}
