use super::token::Token;
use super::tokenizer::{Tokenizer, SyntaxError};
use super::parser::Parser;
// use self::error::*;

pub fn start(code: &str) -> Result<(), SyntaxError> {
    let _tokens: Vec<Token> = Tokenizer::new(code).scan()?;

    for t in &_tokens {
        println!("{}", t.to_string());
    }

    let mut _parser: Parser = Parser::new(_tokens);


    Ok(())
}
