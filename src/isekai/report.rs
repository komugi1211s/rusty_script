
use std::str::Lines;

pub struct Error {
    pub kind: ErrorKind,
    pub msg: String,
    pub line: usize,
}

impl Error {
    pub fn new_while_tokenizing(msg: &str, line: usize) -> Self {
        Self {
            kind: ErrorKind::TokenError,
            msg: String::from(msg),
            line
        }
    }

    pub fn new_on_runtime(msg: &str, line: usize) -> Self {
        Self {
            kind: ErrorKind::RuntimeError,
            msg: String::from(msg),
            line
        }
    }

    pub fn new_while_parsing(msg: &str, line: usize) -> Self {
        Self {
            kind: ErrorKind::ParserError,
            msg: String::from(msg),
            line
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

pub struct ErrorReporter<'a> {
    pub contexts: Vec<&'a str>
}


impl<'a> ErrorReporter<'a> {
    pub fn empty() -> Self {
        Self {
            contexts: vec![],
        }
    }

    pub fn new(ctx: Vec<&'a str>) -> Self {
        Self {
            contexts: ctx,
        }
    }

    pub fn from_lineiter(ctx: Lines<'a>) -> Self {
        Self {
            contexts: ctx.collect(),
        }
    }

    pub fn report_error(&self, err: Error) {
        println!("\x1b[30m{:?}\x1b[0m: {}\n", err.kind, err.msg);
        println!("around line {}... :", err.line);
        println!("{}", self.contexts[err.line - 1]);
        println!("{}", self.contexts[err.line]);
        println!("{}", self.contexts[err.line + 1]);
    }
}
