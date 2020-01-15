use super::{BytecodeGenerator, OpCode};
use crate::typecheck::{astconv, check as typeck};

use trace::position::{CodeSpan, EMPTY_SPAN};
use types::{Type, TypeKind};

use syntax_ast::{ast::*, tokenizer::token::TokenType};

#[derive(Debug)]
pub struct ExpressionHandleResult {
    pub index: usize,
    pub span: CodeSpan,
    pub _type: Type,
}

impl BytecodeGenerator {
    pub(super) fn handle_expr(
        &mut self,
        ast: &ParsedResult,
        expr: &ExprId,
        span: CodeSpan,
    ) -> ExpressionHandleResult {
        let expr = ast.get_expr(*expr);
        let mut handled_result = ExpressionHandleResult {
            span,
            index: 0,
            _type: Type::default(),
        };

        match expr {
            Expr::Variable(name) => {
                let (exists, pos) = self.find_local(&name);

                // FIXME @DumbCode - up there you do "if exists {"
                // and here you're doing "if !exists {"
                // It's confusing, make it consistent
                if !exists {
                    // it could be a global variable;
                    let (exists_global, pos_global) = self.find_global(&name);
                    if !exists_global {
                        panic!("Undefined Variable");
                    }

                    let _type = self.global_define[pos_global].dtype.clone();
                    let opcode = self.get_load_opcode(&_type, true);
                    handled_result._type = _type.clone();
                    self.code.push_opcode(opcode, span);

                    let index = pos_global as u16;
                    handled_result.index =
                        self.code.push_operands(index.to_ne_bytes().to_vec(), span);
                    return handled_result;
                }

                let local_type = self.current_define[pos].dtype.clone();
                let opcode = self.get_load_opcode(&local_type, false);
                handled_result._type = local_type;
                self.code.push_opcode(opcode, span);

                let index = pos as u16;
                handled_result.index = self.code.push_operands(index.to_ne_bytes().to_vec(), span);
                handled_result
            }
            Expr::Literal(literal) => {
                self.handle_literal(&mut handled_result, literal, span);
                handled_result
            }
            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(ast, left, span);
                let right = self.handle_expr(ast, right, span);

                if left._type != right._type {
                    panic!();
                }

                handled_result._type = left._type;
                handled_result.index = match operator {
                    Operator::Add => self.code.push_opcode(OpCode::Add, span),
                    Operator::Sub => self.code.push_opcode(OpCode::Sub, span),
                    Operator::Mul => self.code.push_opcode(OpCode::Mul, span),
                    Operator::Div => self.code.push_opcode(OpCode::Div, span),
                    Operator::Mod => self.code.push_opcode(OpCode::Mod, span),

                    // PartialEq Series
                    Operator::NotEq => self.code.push_opcode(OpCode::NotEq, span),
                    Operator::EqEq => self.code.push_opcode(OpCode::EqEq, span),

                    // PartialOrd Series
                    Operator::LessEq => self.code.push_opcode(OpCode::LessEq, span),
                    Operator::MoreEq => self.code.push_opcode(OpCode::MoreEq, span),
                    Operator::Less => self.code.push_opcode(OpCode::Less, span),
                    Operator::More => self.code.push_opcode(OpCode::More, span),
                    _ => unreachable!(),
                };
                handled_result
            }
            Expr::Logical(left, right, operator) => {
                self.handle_expr(ast, left, span);
                self.handle_expr(ast, right, span);

                handled_result.index = match operator {
                    Operator::And => self.code.push_opcode(OpCode::And, span),
                    Operator::Or => self.code.push_opcode(OpCode::Or, span),
                    _ => unreachable!(),
                };
                handled_result._type = Type::boolean();
                handled_result
            }
            Expr::Unary(expr, operator) => {
                let another_result = self.handle_expr(ast, expr, span);
                handled_result.index = match operator {
                    Operator::Not => self.code.push_opcode(OpCode::Not, span),
                    Operator::Neg => self.code.push_opcode(OpCode::Neg, span),
                    _ => unreachable!(),
                };
                handled_result._type = another_result._type;
                handled_result
            }
            Expr::Grouping(group) => self.handle_expr(ast, group, span),
            Expr::Assign(name_id, expr) => {
                let (exists, position, is_global) = {
                    let name = ast.get_expr(name_id.clone());
                    if let Expr::Variable(name) = name {
                        let (lexists, lpos) = self.find_local(&name);
                        if lexists {
                            (lexists, lpos, false)
                        } else {
                            let (gexists, gpos) = self.find_global(&name);
                            if gexists {
                                (gexists, gpos, true)
                            } else {
                                (false, 0, false)
                            }
                        }
                    } else {
                        (false, 0, false)
                    }
                };

                if !exists {
                    panic!("Undeclared / defined variable!");
                }

                let expression_result = self.handle_expr(ast, expr, span);
                let expression_type = expression_result._type;
                let declared_type = {
                    if is_global {
                        &self.global_define[position].dtype
                    } else {
                        &self.current_define[position].dtype
                    }
                };

                if declared_type != &expression_type {
                    panic!(
                        "Type Mismatch when Assigning: {:?} != {:?}",
                        declared_type, &expression_type
                    );
                }

                let opcode = self.get_store_opcode(&expression_type, is_global);
                let operands = position as u16;
                self.code.push_opcode(opcode, span);
                handled_result._type = expression_type;
                handled_result.index = self
                    .code
                    .push_operands(operands.to_ne_bytes().to_vec(), span);
                handled_result
            }
            Expr::FunctionCall(func_name, arguments) => {
                if let Expr::Variable(ref name) = ast.get_expr(*func_name) {
                    let (exists, position) = self.find_global(&name);
                    if !exists {
                        panic!("Undefined Function.");
                    }

                    let arg_count = self.function_table[&position].arg_count;
                    let arg_types = self.function_table[&position].arg_types.clone();
                    let return_type = self.function_table[&position].return_type.clone();
                    if arg_count != arguments.len() {
                        panic!(
                            "Too Few / much arguments provided. require {} arg(s)",
                            arg_count
                        );
                    }

                    if 0 < arg_count {
                        let mut arguments = arguments.clone();
                        arguments.reverse();

                        let argument_end = arg_count - 1;
                        for (i, arg) in arguments.iter().enumerate() {
                            let handled_type = self.handle_expr(ast, arg, span)._type;
                            let current_type = &arg_types[argument_end - i];
                            if current_type != &handled_type {
                                panic!("Type Mismatch when calling a function!! at line {}, {:?} != {:?}", span, current_type, handled_type);
                            }
                        }
                    }
                    self.code.push_opcode(OpCode::Call, span);
                    let jump_here = self
                        .code
                        .push_operands((position as u16).to_ne_bytes().to_vec(), span);
                    handled_result._type = return_type.clone();
                    handled_result.index = jump_here;
                }
                handled_result
            }
            _ => unreachable!(),
        }
    }

    fn handle_literal(&mut self, out: &mut ExpressionHandleResult, lit: &Literal, span: CodeSpan) {
        let _type = astconv::literal_to_type(lit);
        if _type.kind == TypeKind::Null {
            out._type = _type;
            out.index = self.code.write_null(span);
            return;
        }
        let opcode = type_to_const_opcode(&_type);
        let value = literal_to_byte_array(lit).unwrap();
        let length = value.len();

        let start_index = if opcode == OpCode::ConstDyn {
            let x = self.const_table.push_data(length.to_ne_bytes().to_vec());
            self.const_table.push_data(value);
            x
        } else {
            self.const_table.push_data(value)
        };

        self.const_table.types.insert(start_index, _type.clone());
        self.code.push_opcode(opcode, span);
        let index = self
            .code
            .push_operands(start_index.to_ne_bytes().to_vec(), span);
        out._type = _type;
        out.index = index;
    }
}

fn type_to_const_opcode(ty: &Type) -> OpCode {
    match ty.kind {
        TypeKind::Int | TypeKind::Float => OpCode::Const64,
        TypeKind::Str => OpCode::ConstDyn,
        TypeKind::Boolean => OpCode::Const8,
        _ => unreachable!(),
    }
}

fn literal_to_byte_array(lit: &Literal) -> Option<Vec<u8>> {
    let string_ = lit.tok.lexeme.as_ref().unwrap();
    match lit.kind {
        LiteralKind::Int => {
            let item = string_.parse::<i64>().unwrap();
            Some(i64::to_ne_bytes(item).to_vec())
        }
        LiteralKind::Str => Some(string_.as_bytes().to_vec()),
        LiteralKind::Float => {
            let item = string_.parse::<f64>().unwrap();
            Some(u64::to_ne_bytes(f64::to_bits(item)).to_vec())
        }
        LiteralKind::Bool => match string_.as_str() {
            "true" => Some(vec![1]),
            "false" => Some(vec![0]),
            _ => unreachable!(),
        },
        LiteralKind::Null => None,
    }
}