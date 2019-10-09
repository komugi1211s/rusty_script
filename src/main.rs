use std::env;
use std::fs::File;
use std::io::prelude::*;

use isekai_language::isekai;

macro_rules! ise_print {
    ($x: expr) => {
        println!("isekai: {}", $x);
    }
}

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

fn run_file(path: &String) -> Result<(), ()>{
    let mut strings = String::new();
    let mut f = match File::open(path) {
        Ok(n) => n,
        _ => {
            ise_print!("ファイルの読み込みに失敗しました。");
            exit_process(false);
        }
    };

    f.read_to_string(&mut strings).expect("ファイルの読み込みに失敗しました。");
    println!("{}", &strings);
    match isekai::core::start(&strings) {
        Ok(_) => Ok(()),
        Err(e) => {
            println!("エラーが発生しました。: {:?}", e);
            Err(())
        },
    }
}

fn main() {
    let arguments: Vec<String> = env::args().collect();
    
    if arguments.len() <= 1 { // もし引数が無かったら
        ise_print!("usage: isekai [filename].kai");
        exit_process(true);
    }
    exit_process(match run_file(&arguments[1]) {
        Ok(_) => true,
        Err(_) => {
            println!("パースに失敗。");
            false
        },
    })
}
