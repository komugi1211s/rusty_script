pub mod source;
pub use source::SourceFile;
pub mod macros;
pub mod position;

use position::CodeSpan;

fn mark_by_red(string: &str, col_start: usize, col_len: usize) -> String {
    let mut chars = string.chars();

    let left = chars.by_ref().take(col_start).collect::<String>();
    let problematic_pos = chars.by_ref().take(col_len).collect::<String>();
    let rest = chars.collect::<String>();

    format!("{}\x1b[31m{}\x1b[39m{}", left, problematic_pos, rest)
}

pub fn spit_line(lines: &CodeSpan, file: &SourceFile) {
    let code_lines = file.code.lines().collect::<Vec<&'_ str>>();
    println!("{} 行目 :: ファイル {}", lines.row_start, file.filename);

    let (col_start, col_len) = lines.cols();
    if lines.is_oneliner() {
        let line = lines.rows().0.saturating_sub(1);
        let line_text = code_lines.get(line);

        let decorated_text = if let Some(text) = line_text {
            mark_by_red(text, col_start, col_len)
        } else {
            String::from("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        };

        println!(
            "   :: {} |: {}",
            line,
            &decorated_text
        );
    } else {
        let (start, len) = lines.rows();
        let end = start + len;

        for line in start..end {
            let line = line.saturating_sub(1);
            println!(
                "   :: {} |: {}",
                line,
                code_lines.get(line).unwrap_or(&"~~~~~~~~~~~~~~")
            );
        }
    }
}

pub fn report(
    title: &str,
    message: &str,
) {
    if title == "internal" {
        println!("[Isekai :: 内部エラー] {}", message);
        return;
    }

    println!(
        "{:-<50}",
        format!("[Isekai :: {}]", title)
    );
    println!("{}", message);
}

