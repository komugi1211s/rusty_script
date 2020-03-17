//#![feature(test)]
//extern crate test;

#[macro_use]
extern crate bitflags;

use std::{
    env,
    time::Instant,
    cell::RefCell,
};

mod ast;
mod parser;
mod tokenizer;

mod semantic;
mod bytecode;
mod ir;
mod vm;
mod trace;
mod types;

use ast::ASTree;
use parser::Parser;
use tokenizer::Tokenizer;
use trace::prelude::*;

fn exit_process(success: bool) -> !
{
    ::std::process::exit(if success { 0 } else { 1 });
}

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

pub fn start(module: SourceFile) -> Result<(), ()>
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

fn run_file(path: &str)
{
    
    let module = match SourceFile::open(path)
    {
        Ok(n) => n,
        Err(_) =>
        {
            report("internal", "ファイルが読み込めませんでした。");
            return;
        }
    };

    start(module);
}

fn main()
{
    let arguments: Vec<String> = env::args().collect();

    if arguments.len() <= 1
    {
        // もし引数が無かったら
        println!("usage: isekai [filename].kai");
    }
    else 
    {
        let file_name = arguments.get(1).unwrap();
        run_file(file_name);
    }
}
