pub mod core;
pub mod report;
pub mod types;
// pub mod nativefunc;

pub mod token;
pub mod tokenizer;

pub mod parse;
pub mod parser;

// pub mod evaluate;
pub mod builtin_functions;
pub mod bytecode;
pub mod vm;


pub mod utils {
    pub fn str_to_u16(string: &str) -> u16 {
        string.as_bytes().iter().map(|x| *x as u16).sum()
    }
}
