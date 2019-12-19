use super::parser::Parser;
use super::token::Token;
use super::tokenizer::Tokenizer;
// use super::evaluate::Interpreter;
use super::bytecode::{ BytecodeGenerator, disassemble_all };
use super::report::{ ErrorReporter };
use super::vm::VirtualMachine;
// use self::error::*;
use std::time::Instant;
use std::io::prelude::*;
use std::fs::File;


pub fn start(code: &str) -> Result<(), ()> {
    let reporter = ErrorReporter::from_lineiter(code.lines());
    let start = Instant::now();

    let _tokens: Vec<Token> = match Tokenizer::new(code).scan() {
        Ok(n) => n,
        Err(err) => { reporter.report_error(err); return Err(()); },
    };
    println!(
        "Tokenizer took: \x1b[32m{} micros\x1b[39m",
        start.elapsed().as_micros()
    );
    let mut _parser: Parser = Parser::new(&_tokens);
    let parser_time = Instant::now();
    let result = match _parser.parse() {
        Ok(n) => n,
        Err(err) => { reporter.report_error(err); return Err(()); },
    };
    println!(
        "Parser took: \x1b[32m{} micros\x1b[39m",
        parser_time.elapsed().as_micros()
    );

    let codegen = BytecodeGenerator::new();
    let codegen_time = Instant::now();
    let chunk = codegen.traverse_ast(result).unwrap();
    println!(
        "Codegen took: \x1b[32m{} micros\x1b[39m",
        codegen_time.elapsed().as_micros()
    );
    println!(
        "Total Time: \x1b[32m{} micros\x1b[39m",
        start.elapsed().as_micros()
    );

    let disassembled = disassemble_all(&chunk.code);

    {
        let mut file = File::create("dump").expect("Dump File failed to create.");
        for i in disassembled {
            writeln!(file, "{}", i).expect("Hey?");
        }
        file.flush().expect("File Flushing Failed.");
    }
    let mut vm = VirtualMachine::new(chunk);
    vm.run();

    /*
    let mut interpreter = Interpreter::new();
    let interp_time = Instant::now();
    {
        interpreter.visit(i);
    }
    println!("Interpreter took: {} ns", interp_time.elapsed().as_nanos());

    println!("TOTAL took: {} ms", start.elapsed().as_nanos());
    */
    Ok(())
}
