
use trace::SourceFile;
use syntax_ast::ast::ASTree;

pub struct CompiledCode {
}

pub fn generate_bytecode(module: &SourceFile, ast: &ASTree) -> Result<(), ()> {
    Ok(())
}
