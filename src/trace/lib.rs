pub mod source;
pub use source::SourceFile;
pub mod macros;
pub mod position;

use position::CodeSpan;

use std::sync::atomic::AtomicBool;

static ERROR_REPORTED: AtomicBool = AtomicBool::new(false);

pub mod prelude
{
    pub use super::err_fatal;
    pub use super::position::CodeSpan;
    pub use super::source::SourceFile;
    pub use super::{error_reported, report, spit_line};
}

pub fn error_reported() -> bool
{
    ERROR_REPORTED.load(std::sync::atomic::Ordering::Relaxed)
}

fn mark_by_red(string: &str, col_start: usize, col_len: usize) -> String
{
    let mut chars = string.chars();

    let left = chars.by_ref().take(col_start - 1).collect::<String>();
    let problematic_pos = chars.by_ref().take(col_len).collect::<String>();
    let rest = chars.collect::<String>();

    let middle = format!("\x1b[31m{}\x1b[0m", problematic_pos);
    format!("{}{}{}", left, middle, rest)
}

pub fn spit_line(file: &SourceFile, lines: &CodeSpan)
{
    let code_lines = file.code.lines().collect::<Vec<&'_ str>>();
    println!("{} - ファイル {}", lines, file.filename);

    let (col_start, col_len) = lines.cols();
    if lines.is_oneliner()
    {
        let line = lines.rows().0;
        let index = line.saturating_sub(1);
        let line_text = code_lines.get(index);

        let decorated_text = if let Some(text) = line_text
        {
            mark_by_red(text, col_start, col_len)
        }
        else
        {
            String::from("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        };

        println!(" {:>5} |:| {}", line, &decorated_text);
    }
    else
    {
        let (start, len) = lines.rows();
        let end = start + len;

        for line in start..end
        {
            let index = line.saturating_sub(1);
            println!(
                " {:>5} |:| \x1b[31m{}\x1b[0m",
                line,
                code_lines
                    .get(index)
                    .unwrap_or(&"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            );
        }
    }
}

pub fn report(title: &str, message: &str)
{
    if title == "internal"
    {
        println!("{:-<50}", "\x1b[31m[Isekai]\x1b[0m :: 内部エラー ");
        println!("{}", message);
        return;
    }

    println!("{:-<50}", format!("\x1b[31m[Isekai]\x1b[0m :: {} ", title));
    println!("{}", message);
    ERROR_REPORTED.store(true, std::sync::atomic::Ordering::Relaxed);
}
