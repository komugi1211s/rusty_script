use trace::{ SourceFile, err_fatal, code_line };
use syntax_ast::ast::*;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
};


// TODO: Actually implement.
