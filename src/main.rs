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

mod global {
    use std::{
        env,
        collections::HashMap,
        sync::{ Arc, Mutex },
        path::{ PathBuf, Path },
    };

    use super::{
        ast::ASTree,
        semantic::{ SymbolTable, SymTable },
        trace::prelude::*,
    };

    pub struct Global 
    {
        pub module_paths: Vec<PathBuf>,
        pub modules: Vec<SourceFile>,
        pub symtable: SymTable,
    }

    impl Global
    {
        pub fn new() -> Self
        {
            let mut vector = Vec::with_capacity(16);
            let root = expect!(env::current_dir(),
                               "現在のディレクトリが取得できませんでした。");
            vector.push(root);

            Self 
            {
                module_paths: vector,
                modules: Vec::with_capacity(32),
                symtable: SymTable {
                    symbol: HashMap::with_capacity(255),
                    locals: Vec::with_capacity(64),
                }
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
}

use std::{
    env,
    time::Instant,
    sync::{ Arc, Mutex },
    cell::RefCell,
    collections::HashMap,
    path::{ PathBuf, Path },
};

use ast::ASTree;
use parser::Parser;
use tokenizer::Tokenizer;
use semantic::{ SymbolTable, SymTable };
use trace::prelude::*;
use global::Global;

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

    let tokens   = {
        Tokenizer::new(root_file.code.len()).scan(root_file).unwrap()
    };

    let mut ast  = {
        Parser::new(root_file, &tokens).parse().unwrap()
    };

    let symtable = {
        semantic::analysis(&mut globals.symtable, &mut ast).unwrap()
    };

    let bc       = bytecode::generate_bytecode(&globals, &ast).unwrap();
    vm::start_vm(&mut vm, &root_file, &bc);
}
