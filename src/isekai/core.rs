
use super::scanner::{CodeScanner, Token, ScannerError};
// use self::error::*;

pub fn start(code: &String)  -> Result<(), ScannerError> {
    let mut _scanner: CodeScanner = CodeScanner::new(code);
    let _tokens: Vec<Token> = _scanner.scan()?;
    
    for t in _tokens.iter() {
        println!("{}", t.to_info_str());
    }

    Ok(())
}
