use std::fs;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug, PartialEq)]
pub struct SourceFile
{
    pub filename: String,
    pub code: String,
    pub line: usize,
}

impl SourceFile
{
    pub fn open(name: &str) -> Result<Self, ()>
    {
        let file = match fs::File::open(name)
        {
            Ok(x) => x,
            Err(_) => return Err(()),
        };

        let mut reader = BufReader::new(file);
        let mut string = String::new();
        let mut line = 0;

        loop
        {
            match reader.read_line(&mut string)
            {
                Ok(n) if n > 0 => line += 1,
                _ => break,
            }
        }

        Ok(SourceFile {
            filename: name.to_string(),
            code: string,
            line: line,
        })
    }

    pub fn new(name: &str, code: &str) -> Self
    {
        SourceFile {
            filename: name.to_string(),
            code: code.to_string(),
            line: code.lines().collect::<Vec<&'_ str>>().len(),
        }
    }

    // TODO - @DumbCode: every time user uses repl, it allocates new string and maybe it'll become
    // more slow -- require measurement, and fix if necessary.
    pub fn repl(code: &str) -> Self
    {
        Self::new("-- repl --", code)
    }
}
