
use super::scanner::{CodeScanner, Token, SyntaxError};
// use self::error::*;

pub fn start(code: &str)  -> Result<(), SyntaxError> {
    let mut _scanner: CodeScanner = CodeScanner::new(code);
    let _tokens: Vec<Token> = _scanner.scan()?;
    
    for t in _tokens.iter() {
        println!("{}", t.to_string());
    }

    Ok(())
}
