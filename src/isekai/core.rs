use super::token::{ Token };
use super::tokenizer::{ Tokenizer, SyntaxError };
use super::parser::Parser;
// use super::evaluate::Interpreter;
use super::bytecode::{ BytecodeGenerator };
use super::vm::{ VirtualMachine };
// use self::error::*;
use std::time::{ Instant };

use std::fs::File;
use std::io::{
    prelude::*,
    BufReader,
};

pub fn start(code: &str) -> Result<(), SyntaxError>
{
    let start = Instant::now();
    let _tokens: Vec<Token> = Tokenizer::new(code).scan()?;
    println!("Tokenizer took: \x1b[32m{} ms\x1b[39m", start.elapsed().as_millis());

    let mut _parser: Parser = Parser::new(_tokens);

    let parser_time = Instant::now();
    let result = _parser.parse();
    println!("Parser took: \x1b[32m{} ms\x1b[39m", parser_time.elapsed().as_millis());
    // println!("parsed_result: \x1B{:#?}", result);

    let codegen = BytecodeGenerator::new();
    let bytecode = codegen.traverse_ast(result).unwrap();
    let disassembled = bytecode.disassemble_all();

    {
        let mut file = File::create("dump").expect("Dump File failed to create.");
        for i in disassembled
        {
            writeln!(file, "{}", i).expect("Hey?");
        }
        file.flush().expect("File Flushing Failed.");
    }

    let mut vm = VirtualMachine::new(bytecode);
    vm.run();


    /*
    let mut interpreter = Interpreter::new();
    let interp_time = Instant::now();
    for i in &result 
    {
        interpreter.visit(i);
    }
    println!("Interpreter took: {} ns", interp_time.elapsed().as_nanos());

    println!("TOTAL took: {} ms", start.elapsed().as_nanos());
    */
    Ok(())
}
