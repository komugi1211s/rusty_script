//#![feature(test)]
//extern crate test;

use std::{
    env,
    fs::{self, File},
    io::prelude::*,
    time::Instant,
};

// Phase 1 & 2 - Tokenizing, Parsing
use syntax_ast::{ast::ASTree, parser::Parser, tokenizer::Tokenizer};

// Phase 3  - semantic analysis
use semantic;

// Phase 4  - bytecode gen | llvm gen(TODO)
use compiler::{bytecode, ir::print_ir_vec};

// Phase 5a - VM
use compiler::vm;
use trace::prelude::*;

fn exit_process(success: bool) -> !
{
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

struct TimeCount
{
    messages: RefCell<Vec<String>>,
}

impl TimeCount
{
    fn time<T, U>(&self, step: &str, fun: impl FnOnce() -> Result<T, U>) -> Result<T, U>
    {
        let start = Instant::now();
        let result = fun();
        let elapsed = start.elapsed();
        let time = { elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9 };

        let elapsed_msg = if result.is_ok() && !error_reported()
        {
            format!(" {0:<12} Finished :: \x1b[32m{1} secs\x1b[0m", step, time)
        }
        else
        {
            format!(" {0:<12} Error :: \x1b[31m{1} secs\x1b[0m", step, time)
        };
        self.messages.borrow_mut().push(elapsed_msg);

        result
    }

    fn report(&self)
    {
        println!("");
        for x in self.messages.borrow().iter()
        {
            println!("{}", x);
        }
    }
}

pub fn start(module: SourceFile, stage: u8) -> Result<(), ()>
{
    let mut timer = TimeCount {
        messages: RefCell::new(Vec::with_capacity(5)),
    };
    // 実際の所謂「文字の数」ではなくバイト列の長さを確認している
    // 今回の場合は最低でもこれだけを確保しておきたいという長さなのでこれで問題ない
    let byte_length = module.code.as_str().len();

    let binary = timer.time("Total", || {
        let tokens = timer.time(
            "Tokenizer",
            || Tokenizer::new(byte_length).scan(&module)
        )?;

        let mut ast = timer.time(
            "Parser", 
            || Parser::new(&module, &tokens).parse()
        )?;


        timer.time(
            "Semantic",
            || semantic::analysis(&mut ast)
        )?;

        let binary = timer.time(
            "CodeGen",
            || bytecode::generate_bytecode(&ast)
        )?;

        Ok(binary)
    });

    // print_ir_vec(&binary.code);
    timer.report();
    let binary = binary?;

    let mut vm = vm::VirtualMachine::new();
    vm::start_vm(&mut vm, &module, &binary);
    Ok(())
}

fn form_repl_line(s: &str) -> Option<&str>
{
    if s.len() == 0
    {
        None
    }
    else
    {
        Some(s)
    }
}

fn interpret() -> bool
{
    true
}

fn run_file(path: &str, stage: Option<&String>) -> bool
{
    let module = match SourceFile::open(path)
    {
        Ok(n) => n,
        Err(_) =>
        {
            report("internal", "ファイルが読み込めませんでした。");
            return false;
        }
    };
    let mut stage_u8: u8 = 0;
    if let Some(stage_) = stage
    {
        stage_u8 = stage_.as_str().parse::<u8>().unwrap_or(0);
    }

    start(module, stage_u8).is_ok()
}

fn main()
{
    let arguments: Vec<String> = env::args().collect();

    if arguments.len() <= 1
    {
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
