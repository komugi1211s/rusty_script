//#![feature(test)]
//extern crate test;

use std::env;
use std::io::prelude::*;
use std::fs::{ self, File };

use std::time::Instant;

use syntax_ast::parser::Parser;
use syntax_ast::tokenizer::Tokenizer;
use compiler::{ bytecode, vm };

use trace::{ error, err_internal, source::Module };

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn exit_process(success: bool) -> ! {
    ::std::process::exit(if success { 0 } else { 1 });
}

type Stage = u8;
use std::cell::RefCell;

/*
fn dump_chunk(chunk: &ByteChunk) {
    let disassembled = disassemble_all(&chunk.code);
    let mut file = File::create("dump").expect("Dump File failed to create.");
    for i in disassembled {
        writeln!(file, "{}", i).expect("Hey?");
    }
    file.flush().expect("File Flushing Failed.");
}
*/

struct TimeCount {
    messages: RefCell<Vec<String>>
}

impl TimeCount {
    fn time<T, U>(&self, step: &str, fun: impl FnOnce() -> Result<T, U>) -> Result<T, U> {
        let start = Instant::now();
        let result = fun();
        let elapsed = start.elapsed();
        let time = {
            elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9
        };

        let elapsed_msg = if result.is_ok() {
            format!(
                " {0:<12} Finished :: \x1b[32m{1} secs\x1b[39m",
                step,
                time
            )
        } else {
            format!(
                " {0:<12} Error :: \x1b[31m{1} secs\x1b[39m",
                step,
                time
            )
        };
        self.messages.borrow_mut().push(elapsed_msg);

        result
    }

    fn report(&self) {
        for x in self.messages.borrow().iter() {
            println!("{}", x);
        }
    }
}

fn time_it<T, U>(step: &str, fun: impl FnOnce() -> Result<T, U>) -> Result<T, U> {
    let start = Instant::now();
    let result = fun();
    let elapsed = start.elapsed();
    let time = {
        elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9
    };

    if result.is_ok() {
        println!(
            " {0:<12} Finished :: \x1b[32m{1} secs\x1b[39m",
            step,
            time
        );
    } else {
        println!(
            " {0:<12} Error :: \x1b[31m{1} secs\x1b[39m",
            step,
            time
        );
    }

    result
}

/*
fn run_module(
    core: Module,
    stage: Stage
) -> Result<(), ()> {
    let mut tokenizer  = Tokenizer::new();
    let mut parser     = Parser::new();
    let mut codegen    = BytecodeGenerator::new();
    let mut vmachine   = VirtualMachine::new();


    let tokens = tokenizer.from_module(core).scan().unwrap();
    let parsed = parser.tokens(tokens).parse().unwrap();
    let chunk = codegen.traverse_ast(parsed).unwrap();
}

*/

pub fn start(module: Module, stage: u8) -> Result<(), ()> {
    let mut timer = TimeCount { messages: RefCell::new(Vec::with_capacity(4)) };

    let binary = timer.time(
        "Total",
        || {
            let tokens  = timer.time("Tokenizer", || Tokenizer::new().scan(&module))?;
            let parsed  = timer.time("Parser",    || Parser::new(&module, &tokens).parse())?;
            let binary  = timer.time("CodeGen",   || bytecode::generate_bytecode(&module, &parsed))?;
            Ok(())
        }
    )?;

    timer.report();

    let mut vm = vm::VirtualMachine::new();
    vm::start_vm(&mut vm, &module, &binary);
    Ok(())
}

fn form_repl_line(s: &str) -> Option<&str> {
    if s.len() == 0 {
        None
    } else {
        Some(s)
    }
}

fn interpret() -> bool {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("Isekai :: > ");
        match readline {
            Ok(line) => {
                if let Some(l) = form_repl_line(&line) {
                    if l == "!exit" {
                        break;
                    }
                    let mods = Module::repl(l);
                    start(mods, 0);
                }
            }
            _ => break,
        }
    }

    true
}

fn run_file(path: &str, stage: Option<&String>) -> bool {
    let module = match Module::open(path) {
        Ok(n) => n,
        Err(_) => {
            err_internal!("ファイルが読み込めませんでした。");
            return false;
        },
    };
    let mut stage_u8: u8 = 0;
    if let Some(stage_) = stage {
        stage_u8 = stage_.as_str().parse::<u8>().unwrap_or(0);
    }

    start(module, stage_u8).is_ok()
}

fn main() {
    trace::init_logger();
    let arguments: Vec<String> = env::args().collect();

    if arguments.len() <= 1 {
        // もし引数が無かったら
        println!("usage: isekai [filename].kai");
        exit_process(interpret());
    }
    exit_process(run_file(arguments.get(1).unwrap(), arguments.get(2)))
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
