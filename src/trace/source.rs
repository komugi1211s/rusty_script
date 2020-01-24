use std::fs;
use std::io::{ Read, BufRead, BufReader };
use super::CodeSpan;
use std::cmp;

#[derive(Clone, Debug)]
pub struct Module {
    pub filename: String,
    pub code: String,
    pub line: usize,
}

impl Module {
    pub fn open(name: &str) -> Result<Self, ()> {

        let file = match fs::File::open(name) {
            Ok(x) => x,
            Err(_) => return Err(()),
        };

        let mut reader = BufReader::new(file);
        let mut string = String::new();
        let mut line = 0;

        loop {
            match reader.read_line(&mut string) {
                Ok(n) if n > 0 => line += 1,
                _ => break,
            }
        }

        Ok(Module {
            filename: name.to_string(),
            code: string,
            line: line
        })
    }

    pub fn new(name: &str, code: &str) -> Self {
        Module {
            filename: name.to_string(),
            code: code.to_string(),
            line: code.lines().collect::<Vec<&'_ str>>().len(),
        }
    }
}


