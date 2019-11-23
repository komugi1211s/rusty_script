use std::env;
use std::fs::File;
use std::io::prelude::*;

use isekai_language::isekai;

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

fn run_file(path: String) -> bool {
    let mut strings = String::new();
    let mut f = match File::open(path) {
        Ok(n) => n,
        _ => {
            println!("ファイルを開けませんでした。");
            return false;
        }
    };

    f.read_to_string(&mut strings).expect("ファイルの読み込みに失敗しました。");
    match isekai::core::start(strings.as_str()) {
        Ok(_) => true,
        Err(e) => {
            println!("エラーが発生しました。: {}", e.to_string());
            false
        },
    }
}

fn main() {
    let arguments: Vec<String> = env::args().collect();
    
    if arguments.len() <= 1 { // もし引数が無かったら
        println!("usage: isekai [filename].kai");
        exit_process(true);
    }
    exit_process(run_file(arguments[1].clone()))
}
