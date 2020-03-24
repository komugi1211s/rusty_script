//#![feature(test)]
//extern crate test;

#[macro_use] extern crate bitflags;

mod trace;
mod ast;

mod parser;
mod tokenizer;

mod types;
mod semantic;

mod bytecode;
mod ir;
mod vm;

use std::{
    env,
    time::Instant,
    cell::RefCell,
    collections::HashMap,
    path::{ PathBuf, Path },
};

use ast::ASTree;
use parser::Parser;
use tokenizer::Tokenizer;
use trace::prelude::*;

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

pub(crate) struct Global 
{
    pub module_paths: Vec<PathBuf>,
    pub modules: Vec<SourceFile>,
}

impl Global
{
    fn new() -> Self
    {
        let mut vector = Vec::with_capacity(16);
        let root = expect!(env::current_dir(),
                           "現在のディレクトリが取得できませんでした。");
        vector.push(root);

        Self 
        {
            module_paths: vector,
            modules: Vec::with_capacity(32),
        }
    }

    pub fn open_and_add_module(&mut self, file_name: &str) -> Result<usize, ()> 
    {
        let filename_path = Path::new(file_name).with_extension("kai");

        for module_dir in self.module_paths.iter() 
        {
            let target_file_path = module_dir.join(&filename_path);
            if target_file_path.exists() 
            {
                let filepath_str = expect!(target_file_path.to_str().ok_or(()), "ファイルパスに許されない文字列が含まれています。");
                let file = expect!(SourceFile::open(filepath_str), "ファイルを開けませんでした。");

                let next_idx = self.modules.len();
                self.modules.push(file);
                return Ok(next_idx);
            }
        }

        Err(())
    } 

    pub fn lookup_by_name(&self, name: &str) -> Option<usize>
    {
        for (idx, module) in self.modules.iter().enumerate()
        {
            if name == module.filename
            {
                return Some(idx);
            }
        }
        None
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

fn main()
{
    let arguments: Vec<String> = env::args().collect();
    let mut globals = Global::new();
    let mut vm = vm::VirtualMachine::new();

    if arguments.len() <= 1 
    { 
        // TODO: Interpreter Support
        println!("usage: isekai [filename].kai");
        return;
    }
    let root = globals.open_and_add_module(&arguments[1]).unwrap();
    let root_file = &globals.modules[root];

    let tokens  = Tokenizer::new(root_file.code.len()).scan(root_file).unwrap();
    let mut ast = Parser::new(root_file, &tokens).parse().unwrap();
    semantic::analysis(&mut ast).unwrap();
    let bc      = bytecode::generate_bytecode(&ast).unwrap();
    vm::start_vm(&mut vm, &root_file, &bc);
}
