#[macro_use] 
extern crate lazy_static;
pub extern crate log;

pub mod source;
pub use source::{ Module };
pub mod position;
pub mod macros;

use position::CodeSpan;
pub use log::{info, trace, warn, error, debug};

pub static Logger: IsekaiLogger = IsekaiLogger;

pub fn init_logger() {
    log::set_logger(&Logger).map(|()| log::set_max_level(log::LevelFilter::Info)).unwrap();
}



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
    pub fn spit_line(
        &self,
        lines: CodeSpan,
        codes: &Module,
        pad: usize
    ) {
        let code_lines = codes.code.lines().collect::<Vec<&'_ str>>();
        println!(" :: 指定ファイルのコード出力\n");

        if lines.is_oneliner() {
            let line = lines.start_usize();
            println!("   :: {} |: {}", line, code_lines.get(line - 1).unwrap_or(&"~~~~~~~~~~~~~~"));
        } else {
            let (start, end) = if lines.length() > pad + 1 {
                (lines.start_usize(), std::cmp::min(lines.end_usize() + 1, codes.line))
            } else {
                (lines.start_usize().saturating_sub(pad), std::cmp::min(lines.end_usize() + pad + 1, codes.line))
            };
            for line in start .. end {
                println!("   :: {} |: {}", line, code_lines.get(line-1).unwrap_or(&"~~~~~~~~~~~~~~"));
            }
        }
    }
}

impl log::Log for IsekaiLogger {
    fn enabled(&self, meta: &log::Metadata) -> bool {
        meta.level() <= log::Level::Trace
    }

    fn log(&self, rec: &log::Record) {
        if !self.enabled(rec.metadata()) {
            println!("Disabled.");
        }

        if rec.target() == "internal" {
            println!("[Isekai :: {}] 内部エラー - {}", rec.level(), rec.args());
            return;
        }

        println!("{:-<50}", format!("[Isekai :: {}] {} ", rec.level(), rec.target()));

        println!("{}", rec.args());

        match (rec.file(), rec.line()) {
            (None, None) => {
                println!("エラーの位置が分かりません！");
            }
            (None, Some(line)) => {
                println!("{} 行目で発生していますが、発生元のファイルが提供されませんでした。", line);
            }
            (Some(file_name), None) => {
                println!("ファイル {} で発生していますが、原因の行目が提供されませんでした。", file_name);
            }
            (Some(file), Some(line)) => {
                println!("{} 行目 :: ファイル {}", line, file);
            }
        }
    }

    fn flush(&self) {}
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;

    #[test]
    fn log_file() {
        init_logger();
        trace!("Hello world.");
        assert!(true);
    }
}
