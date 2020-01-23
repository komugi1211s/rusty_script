use std::fs;
use super::CodeSpan;

#[derive(Clone, Debug)]
pub struct Module {
    pub filename: String,
    pub code: String,
}

impl Module {
    pub fn open(name: &str) -> Result<Self, ()> {

        let source = match fs::read_to_string(name) {
            Ok(n) => n,
            Err(_) => return Err(()),
        };

        Ok(Module {
            filename: name.to_string(),
            code: source,
        })
    }

    pub fn new(name: &str, code: &str) -> Self {
        Module {
            filename: name.to_string(),
            code: code.to_string(),
        }
    }

    pub fn kv_from_span<'a>(&'a self, span: CodeSpan) -> Vec<(usize, &'a str)> {
        assert!(!span.is_invalid());

        let file_lines: Vec<&'a str> = self.code.lines().collect();
        assert!(span.end_usize() < file_lines.len());

        if span.is_oneliner() {
            let line = span.start_usize();
            return vec![(line, file_lines[line])];
        }

        let length = span.end_usize() - span.start_usize();
        let mut vector = Vec::with_capacity(length + 1);
        for i in span.start_usize() .. span.end_usize() {
            vector.push((i, file_lines[i]));
        }
        vector
    }
}


