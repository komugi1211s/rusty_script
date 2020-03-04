#[macro_use]
extern crate bitflags;

// pub mod builtin_functions;
// pub mod bytecode;
// pub mod typecheck;
// pub mod vm;
// pub mod context;
//
// pub mod utils {
//     pub fn str_to_u16(string: &str) -> u16 {
//         string.as_bytes().iter().map(|x| *x as u16).sum()
//     }
// }
//
pub mod bytecode;
// pub mod typecheck;
// pub mod llvm;
pub mod ir;
pub mod vm;
