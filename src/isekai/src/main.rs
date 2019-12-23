//#![feature(test)]
//extern crate test;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use syntax_ast::tokenizer::Tokenizer;
use syntax_ast::parser::Parser;
// use super::evaluate::Interpreter;
use super::bytecode::{ BytecodeGenerator, disassemble_all };
use super::report::{ ErrorReporter };
use super::vm::VirtualMachine;
// use self::error::*;
use std::time::Instant;
use std::io::prelude::*;
use std::fs::File;

#[cfg(target_os = "macos")]
fn exit_process(success: bool) -> ! {
    ::std::process::exit(if success { 0 } else { 1 });
}

#[cfg(target_os = "linux")]
fn exit_process(success: bool) -> ! {
    ::std::process::exit(if success { 0 } else { 1 });
}

#[cfg(target_os = "windows")]
fn exit_process(success: bool) -> ! {
    ::std::process::exit(if success { 0x0000 } else { 0x0100 });
}


pub fn start(code: &str) -> Result<(), ()> {
    let reporter = ErrorReporter::from_lineiter(code.lines());
    let start = Instant::now();

    let _tokens = match Tokenizer::new(code).scan() {
        Ok(n) => n,
        Err(err) => { reporter.report_error(err); return Err(()); },
    };
    println!(
        "Tokenizer took: \x1b[32m{} micros\x1b[39m",
        start.elapsed().as_micros()
    );
    let mut _parser = Parser::new(&_tokens);
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

fn run_file(path: &String) -> bool {
    let mut strings = String::new();
    let mut f = match File::open(path) {
        Ok(n) => n,
        _ => {
            println!("ファイルを開けませんでした。");
            return false;
        }
    };

    f.read_to_string(&mut strings)
        .expect("ファイルの読み込みに失敗しました。");
    start(strings.as_str()).is_ok()
}

fn main() {
    let arguments: Vec<String> = env::args().collect();

    if arguments.len() <= 1 {
        // もし引数が無かったら
        println!("usage: isekai [filename].kai");
        exit_process(true);
    }
    exit_process(run_file(arguments.get(1).unwrap()))
}

/*
fn main() {
    use isekai::_byte_test::fntest;
    fntest();
}
*/

/*
#[cfg(test)]
mod tests
{
    use super::*;
    use test::Bencher;

    #[bench]
    fn benchmark(b: &mut Bencher)
    {
        let stri = "
                int: loop_i = 0;
                while loop_i <= 10000 {
                  loop_i = loop_i + 1;
                }
        ";

        b.iter(move ||
        {
            let n = test::black_box(1);

            for _ in 0..n {
                isekai::core::start(stri);
            }
        });
    }
}
*/
