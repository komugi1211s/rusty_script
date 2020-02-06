 
use trace::{
    SourceFile,
    err_fatal,
    code_line,
};

use syntax_ast::ast::{ ASTree, AstNode, Statement, Expr, StmtId, ExprId, Literal, LiteralKind, Operator };

use super::ir::IRCode;
use super::context::{ CodeGen, BranchMode, ConditionalBranch, Context };
use super::typecheck::{ TypeArena, TypeContext };

use types::{ Value, Type };

#[derive(Debug)]
pub struct CompiledCode {
    pub code: Vec<IRCode>,
    pub consts: Constants,
}

#[derive(Debug)]
pub struct Constants {
    pub values: Vec<Value>,
}

impl Constants {
    fn new() -> Self {
	let mut _self = Self {
	    values: Vec::with_capacity(255) // TODO: change capacity
	};
	
	_self.values.push(Value::Null);
	_self.values.push(Value::Boolean(true));
	_self.values.push(Value::Boolean(false));
	
	_self
    }

    fn add_const(&mut self, value: Value) -> usize {
	let current = self.values.len();
	self.values.push(value);
	current
    }
}

pub struct Env<'a> {
    source: &'a SourceFile,
    defn:   TypeArena,
    consts: Constants,
}

impl<'a> Env<'a> {
    fn new(source: &'a SourceFile) -> Self {
        Env {
	    source,
            defn:   TypeArena::new(),
            consts: Constants::new(),
        }
    }
}

pub fn generate_bytecode(module: &SourceFile, ast: &ASTree) -> Result<CompiledCode, ()> {
    let mut ctx = Context::new();
    let mut env = Env::new(module);

    for node in ast.ast.iter() {
        traverse_statement(&mut env, &mut ctx, ast, node.stmt_id);
    }

    Ok(CompiledCode {
        code: ctx.eject(),
	consts: env.consts,
    })
}


fn traverse_statement(
    env: &mut Env,
    ctx: &mut impl CodeGen,
    ast: &ASTree,
    statement_id: StmtId,
) {
    let statement = ast.get_stmt(statement_id);

    use Statement::*;
    match statement {
        Expression(expr_id) => traverse_expression(env, ctx, ast, *expr_id),

        Print(expr_id)      => {
            traverse_expression(env, ctx, ast, *expr_id);
            ctx.emit_op(IRCode::DebugPrint);
        }

        _ => unimplemented!(),
    };
}

fn traverse_expression(
    env: &mut Env,
    ctx: &mut impl CodeGen,
    ast: &ASTree,
    expression_id: ExprId,
) {
    let expression = ast.get_expr(expression_id);

    use Expr::*;
    match expression {
 	Binary(lhs, rhs, oper) => {
            traverse_expression(env, ctx, ast, *lhs);
            traverse_expression(env, ctx, ast, *rhs);
            ctx.emit_from_oper(*oper);
        }

        Unary(rhs, oper) => {
            traverse_expression(env, ctx, ast, *rhs);
            ctx.emit_from_oper(*oper);
        }

        Literal(ref lit_data)  => emit_constants(env, ctx, lit_data),
        _ => unimplemented!(),
    };
}

fn emit_constants(
    env: &mut Env,
    ctx: &mut impl CodeGen,
    literal: &Literal
) {
    let lexeme: &str = literal.tok.lexeme.as_ref().expect("リテラルなのにトークンが空");

    match literal.kind {
	LiteralKind::Bool => {
	    match lexeme {
		// TODO - @DumbCode: Hardcoded Index.
		"true"  => ctx.emit_op(IRCode::Const8(1)),
		"false" => ctx.emit_op(IRCode::Const8(2)),
		_ => unreachable!(),
	    };
	}

	LiteralKind::Int => {
	    let value: i64 = match lexeme.parse() {
		Ok(n) => n,
		Err(_) => {
		    err_fatal!(
			src: env.source,
			span: literal.tok.span,
			title: "Broken Integer Literal",
			msg: "整数を期待しましたがパースに失敗しました。"
		    );

		    code_line!(src: env.source, span: literal.tok.span, pad: 2);
		    return;
		}
	    };
	    
	    let index = env.consts.add_const(Value::Int(value));
	    ctx.emit_op(IRCode::Const64(index as u32));
	}

	LiteralKind::Float => {
	    let value: f64 = match lexeme.parse() {
		Ok(n) => n,
		Err(_) => {
		    err_fatal!(
			src: env.source,
			span: literal.tok.span,
			title: "Broken Float Literal",
			msg: "実数を期待しましたがパースに失敗しました。"
		    );

		    code_line!(src: env.source, span: literal.tok.span, pad: 2);
		    return;
		}
	    };
	    
	    let index = env.consts.add_const(Value::Float(value));
	    ctx.emit_op(IRCode::Const64(index as u32));
	}
	_ => unimplemented!(),
    }
}
