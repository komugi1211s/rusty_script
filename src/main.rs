//#![feature(test)]
//extern crate test;

#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
// extern crate llvm_sys;

mod trace;
mod ast;

mod parser;
mod tokenizer;

mod types;
mod semantic;

mod bytecode;
mod ir;
mod vm;

// mod llvm;

mod global {
    #[allow(unused_imports)]
    use std::{
        env,
        collections::HashMap,
        sync::{ Arc, Mutex },
        path::{ PathBuf, Path },
    };

    #[allow(unused_imports)]
    use super::{
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
                symtable: HashMap::with_capacity(255),
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

        /*
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
        */
    }
}


use parser::Parser;
use tokenizer::Tokenizer;

use trace::prelude::*;
use global::Global;
use std::{
    env,
};

fn main()
{
    let arguments: Vec<String> = env::args().collect();
    let mut globals = Global::new();
    let mut vm = vm::VirtualMachine::new();

    if arguments.len() <= 1
    {
        println!("usage: isekai [filename].kai");
        return;
    }

    let typecheck_only = arguments.get(2) == Some(&("check".into())); // TODO - @Cleanup

    if let Ok(file_index) = globals.open_and_add_module(&arguments[1]) {
        let root_file = &globals.modules[file_index];
        let tokens   = {
            match Tokenizer::new(root_file.code.len()).scan(root_file)
            {
                Ok(x) => x,
                Err(()) =>
                {
                    println!("コンパイルに失敗しました: Tokenizer エラー");
                    return;
                }
            }
        };

        let mut ast  = {
            match Parser::new(root_file, &tokens).parse()
            {
                Ok(x) => x,
                Err(()) =>
                {
                    println!("コンパイルに失敗しました: Parser エラー");
                    return;
                }
            }
        };

        {
            match semantic::analysis(&mut globals.symtable, &mut ast)
            {
                Ok(x) => x,
                Err(()) =>
                {
                    println!("コンパイルに失敗しました: Semantic Analysis エラー");
                    return;
                }
            }
        }

        if !typecheck_only
        {
            if false {
                println!("LLVM starting.");
                // llvm::llvm_dump(&globals, &ast);
            }
            else
            {
                let bc       = {
                    match bytecode::generate_bytecode(&globals, &ast)
                    {
                        Ok(x) => x,
                        Err(()) =>
                        {
                            println!("コンパイルに失敗しました: Bytecode Generator エラー");
                            return;
                        }
                    }
                };

                // if ::std::cfg!(debug_assertions)
                {
                    use std::fs::File;
                    use std::io::Write;
                    let mut file = File::create("dump").expect("failed to create a dump file.");
                    for (idx, i) in bc.code.iter().enumerate()
                    {
                        writeln!(file, "{} {:?}", idx, i).expect("Hey?");
                    }
                    file.flush().expect("File Flushing Failed.");
                }

                vm::start_vm(&mut vm, &root_file, &bc);
            }
        }
        else
        {
            println!("Typecheck Done.");
        }
    }

}
