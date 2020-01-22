
pub mod position;
pub mod macros;

use position::CodeSpan;
pub use log::{self, info, trace, warn};
use std::str::Lines;

/*

    Tokenizer
    make...

    self.current_line = 15;
    self.end_line = 16;
    Token {
        span: CodeSpan::new(self.current_line, self.end_line);
    }

    then when error happens...
    if err {
        TraceBuilder::error(ErrorKind::TokenizerError)
            .message("Tokenizer knows what's up")
            .caused_at(span)
            .related_info(error)
            .build()?;
    }

    this would result to...
    TokenizerError: Tokenizer knows what's up.
    14  | ...
    15!!| ... .., ...
    16!!| ... ...... .. .. . . . ...
    17  | .. .. . . ...

    Error related info provided:
    XXXXX...

    XX  |
    XX  |
    XX!!|
    XX  |

*/



#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub msg: String,
    pub span: CodeSpan,
}

impl Error {
    pub fn new_while_tokenizing(msg: &str, span: CodeSpan) -> Self {
        Self {
            kind: ErrorKind::TokenError,
            msg: String::from(msg),
            span,
        }
    }

    pub fn new_on_runtime(msg: &str, span: CodeSpan) -> Self {
        Self {
            kind: ErrorKind::RuntimeError,
            msg: String::from(msg),
            span,
        }
    }

    pub fn new_while_parsing(msg: &str, span: CodeSpan) -> Self {
        Self {
            kind: ErrorKind::ParserError,
            msg: String::from(msg),
            span,
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    AssertionError,
    ParserError,
    TokenError,
    RuntimeError,
}

pub struct IsekaiLogger;


impl IsekaiLogger {
    pub fn report_error(&self, err: Error) {
    }
}

impl log::Log for IsekaiLogger {
    fn enabled(&self, meta: &log::Metadata) -> bool {
        meta.level() <= log::Level::Trace
    }

    fn log(&self, rec: &log::Record) {
        if !self.enabled(rec.metadata()) {
            return;
        }

        println!("[{}] {} --------------------------------", rec.level(), rec.args());

        match (rec.module_path(), rec.file(), rec.line()) {
            (None, _, _) => {
                println!("Error file not provided... Something is wrong with error handler too.");
            }
            (Some(name), _, None) => {
                println!("Error ocurred in file {}, but position not provided.", name);
            }
            (Some(name), None, Some(num)) => {
                println!("Error ocurred in file {}, Line {}. but I cannot show the specific line of given file.", name, num);
            }
            (Some(name), Some(file), Some(line)) => {
                println!("Line {} :: File {}", line, name);
                let line_vec: Vec<&'_ str> = file.lines().collect();
                println!("{} |: {}", line, line_vec[line as usize]);
            }
        }
    }

    fn flush(&self) {}
}


#[cfg(tests)]
mod tests {
    use super::*;
    use std::sync::Once;
    use std::cell::RefCell;

    static once: Once = Once::new();
    static file: RefCell<String> = RefCell::new(String::new());

    fn open_file() {
        once.call_once(|| {
            let mut f = match File::open("test_file") {
                Ok(n) => n,
                _ => panic!(),
            };

            let mut string = file.borrow_mut();
            f.read_to_string(&mut string).expect("File open failed.");
        });
    }

    #[test]
    fn log_file() {
        open_file();
    }

}


