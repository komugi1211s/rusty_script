use log;
pub use log::{info, trace, warn};
use std::str::Lines;
pub mod position;
use position::CodeSpan;

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

pub struct ErrorReporter<'a> {
    pub contexts: Vec<&'a str>,
}

impl<'a> ErrorReporter<'a> {
    pub fn empty() -> Self {
        Self { contexts: vec![] }
    }

    pub fn new(ctx: Vec<&'a str>) -> Self {
        Self { contexts: ctx }
    }

    pub fn from_lineiter(ctx: Lines<'a>) -> Self {
        Self {
            contexts: ctx.collect(),
        }
    }

    pub fn report_error(&self, err: Error) {
        println!("\x1b[31m{:?}\x1b[0m - {}", err.kind, err.msg);

        if err.span.is_oneliner() {
            let bottom_line = err.span.end_usize() - 1;
            println!("around line {}:", bottom_line);
            println!(
                "\x1b[36m{}| {}\x1b[0m",
                bottom_line - 1,
                self.contexts[bottom_line - 1]
            );
            println!(
                "\x1b[31m{}| {}\x1b[0m",
                bottom_line, self.contexts[bottom_line]
            );
            println!(
                "\x1b[36m{}| {}\x1b[0m",
                bottom_line + 1,
                self.contexts[bottom_line + 1]
            );
        } else {
            let start_line = err.span.start_usize() - 1;
            let end_line = err.span.end_usize() - 1;
            println!(
                "Error detected within range {} to {}:",
                start_line, end_line
            );
            for i in start_line..end_line {
                println!("\x1b[31m{}| {}\x1b[0m", i, self.contexts[i]);
            }
        }
    }
}

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, meta: &log::Metadata) -> bool {
        meta.level() <= log::Level::Trace
    }

    fn log(&self, _record: &log::Record) {}

    fn flush(&self) {}
}
