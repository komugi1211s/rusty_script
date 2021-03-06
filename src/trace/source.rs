use std::fs;
use std::io::{BufRead, BufReader, self};

#[derive(Clone, Debug, PartialEq)]
pub struct SourceFile
{
    pub filename: String,
    pub code: String,
    pub line: usize,
}

impl SourceFile
{
    pub fn open(name: &str) -> io::Result<Self>
    {
        let file = fs::File::open(name)?;

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
}
