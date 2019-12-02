use super::token::{ Token };
use super::tokenizer::{ Tokenizer, SyntaxError };
use super::parser::Parser;
// use super::evaluate::Interpreter;
use super::bytecode::{ VirtualMachine };
// use self::error::*;
use std::time::{ Instant };

pub fn start(code: &str) -> Result<(), SyntaxError>
{
    let start = Instant::now();
    let _tokens: Vec<Token> = Tokenizer::new(code).scan()?;
    println!("Tokenizer took: \x1b[32m{} ns\x1b[39m", start.elapsed().as_nanos());

    let mut _parser: Parser = Parser::new(_tokens);

    let parser_time = Instant::now();
    let result = _parser.parse();
    println!("Parser took: \x1b[32m{} ns\x1b[39m", parser_time.elapsed().as_nanos());
    // println!("parsed_result: \x1B{:#?}", result);


    let bytecode_vm = VirtualMachine::new();
    let mut bytecode_vm = bytecode_vm.traverse_ast(result).unwrap();
    bytecode_vm.code.disassemble_all();
    bytecode_vm.run();


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
