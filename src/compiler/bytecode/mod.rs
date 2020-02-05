use syntax_ast::{ast::*, tokenizer::token::TokenType};

use crate::typecheck::{Solve, TypeArena, TypeContext, astconv, check};
use crate::vm::VirtualMachine;
use trace::position::{CodeSpan, EMPTY_SPAN};
use types::{Type, TypeKind};

use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::mem;

mod expr;
mod stmt;

use expr::ExpressionHandleResult;
use stmt::{StatementHandleResult, StatementInfo}; 
use super::opcode::{ OpCode };

const USIZE_LENGTH: usize = mem::size_of::<usize>();

#[derive(Default, Debug)]
pub struct Code {
    pub bytes: Vec<u8>,
    pub span: Vec<CodeSpan>,
}

#[derive(Default, Debug)]
pub struct ConstantTable {
    pub bytes: Vec<u8>,
    pub types: HashMap<usize, Type>,
}

impl Code {
    fn merge(&mut self, other: Code) -> usize {
        let current_length = self.current_length();
        self.bytes.extend(other.bytes);
        self.span.extend(other.span);
        current_length
    }

    pub fn push_operand(&mut self, operand: u8, span: CodeSpan) -> usize {
        self.bytes.push(operand);
        self.span.push(span);
        self.bytes.len()
    }

    pub fn current_length(&self) -> usize {
        self.bytes.len()
    }

    pub fn push_opcode(&mut self, code: OpCode, span: CodeSpan) -> usize {
        self.push_operand(code as u8, span)
    }

    pub fn rewrite_operand(&mut self, byte: u8, index: usize) -> usize {
        if self.bytes.len() < index {
            panic!("Rewrite opcode out of range");
        }

        self.bytes[index] = byte;
        self.bytes.len()
    }

    pub fn rewrite_operands(&mut self, bytes: Vec<u8>, index: usize) -> usize {
        for i in 0..bytes.len() {
            self.rewrite_operand(bytes[i], index + i);
        }
        self.bytes.len()
    }

    pub fn rewrite_opcode(&mut self, code: OpCode, index: usize) -> usize {
        self.rewrite_operand(code as u8, index)
    }

    pub fn push_operands(&mut self, operands: Vec<u8>, span: CodeSpan) -> usize {
        for i in &operands {
            self.push_operand(*i, span);
        }
        self.bytes.len()
    }

    pub fn write_null(&mut self, span: CodeSpan) -> usize {
        self.push_opcode(OpCode::PushPtr, span);
        self.push_operands(0usize.to_ne_bytes().to_vec(), span);
        self.bytes.len()
    }
}

impl ConstantTable {
    pub fn push_data<T>(&mut self, data: T) -> usize
    where
        T: IntoIterator<Item = u8>,
    {
        let start_at = self.bytes.len();
        for i in data.into_iter() {
            self.bytes.push(i);
        }
        start_at
    }

    pub fn assert_datarange(&self, index: usize) {
        if self.bytes.len() <= index {
            panic!("Index Out of Range");
        }
    }

    pub fn read_data_8(&self, index: usize) -> (u8, &Type) {
        self.assert_datarange(index);
        (self.bytes[index], self.types.get(&index).unwrap())
    }

    pub fn read_data_16(&self, index: usize) -> ([u8; 2], &Type) {
        self.assert_datarange(index);
        let first = self.bytes[index];
        let second = self.bytes[index + 1];
        ([first, second], self.types.get(&index).unwrap())
    }

    pub fn read_data_32(&self, index: usize) -> ([u8; 4], &Type) {
        self.assert_datarange(index);
        let f = self.bytes[index];
        let s = self.bytes[index + 1];
        let t = self.bytes[index + 2];
        let fo = self.bytes[index + 3];
        ([f, s, t, fo], self.types.get(&index).unwrap())
    }

    pub fn read_data_64(&self, index: usize) -> ([u8; 8], &Type) {
        self.assert_datarange(index);
        let mut data: [u8; 8] = [0; 8];

        for i in 0..8 {
            data[i] = self.bytes[index + i];
        }
        (data, self.types.get(&index).unwrap())
    }

    pub fn read_data_dyn(&self, index: usize) -> (Vec<u8>, &Type) {
        self.assert_datarange(index);
        let mut data: Vec<u8> = Vec::new();
        let length: usize = {
            let mut size: [u8; USIZE_LENGTH] = [0; USIZE_LENGTH];
            for i in 0..USIZE_LENGTH {
                size[i] = self.bytes[index + i];
            }
            usize::from_ne_bytes(size)
        };
        for i in 0..length {
            data.push(self.bytes[index + USIZE_LENGTH + i]);
        }
        (data, self.types.get(&index).unwrap())
    }
}

pub fn disassemble_all(code: &Code) -> Vec<String> {
    disassemble(code, 0, code.bytes.len())
}

pub fn disassemble(code: &Code, start: usize, max_len: usize) -> Vec<String> {
    let mut vector: Vec<String> = Vec::new();
    vector.push(" ============ DISASSEMBLED ============ \n".to_string());
    let mut current = start;
    let max = max_len;
    while current < max {
        let opbyte = code.bytes[current];
        let mut opcode = format!("{:?}", OpCode::from_u8(opbyte).unwrap_or(OpCode::Interrupt));
        opcode.make_ascii_uppercase();
        let padding = OpCode::from_u8(opbyte).unwrap_or(OpCode::Interrupt).len();
        if 0 < padding {
            current += 1;

            let operand = get_disassemble_operand(code, current, padding);
            let formatted = format!("{:<16} {}", opcode, operand);
            let in_byte = format!("{:02X} {}", opbyte, operand);
            vector.push(format!(
                " | {:04X} | {:<48} | {}",
                current, formatted, in_byte
            ));
            current += padding;
        } else {
            let in_byte = format!("{:02X}", opbyte);
            let formatted = format!("{:<16}", opcode);
            vector.push(format!(
                " | {:04X} | {:<48} | {}",
                current, formatted, in_byte
            ));
            current += 1;
        }
    }

    vector.push("\n ========== DISASSEMBLE DONE ========== \n".to_string());
    vector.push(" ============= BYTE CODES ============= \n".to_string());
    let bytecode_hex: Vec<String> = (&code.bytes[start..max])
        .iter()
        .map(|x| format!("{:02x}", x))
        .collect();
    vector.push(bytecode_hex.join(" "));
    vector.push("\n =========== BYTE CODES DONE ========== \n".to_string());
    vector
}

fn get_disassemble_operand(code: &Code, current: usize, padding: usize) -> String {
    let operand = &code.bytes[(current)..(current + padding)];
    let formatted: Vec<String> = operand.iter().map(|x| format!("{:02X}", x)).collect();
    formatted.join(" ")
}


// TODO - @Cleanup
pub struct FuncInfo {
    pub name: String,
    pub position: usize,
    pub arg_count: usize,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
    pub is_native: bool,
    pub native_pointer: Option<fn(&mut VirtualMachine)>,
}

impl FuncInfo {
    fn new(
        name: &str,
        position: usize,
        arg_count: usize,
        arg_types: Vec<Type>,
        return_type: Type,
        is_native: bool,
    ) -> Self {
        Self {
            name: name.to_string(),
            position,
            arg_count,
            arg_types,
            return_type,
            is_native,
            native_pointer: None,
        }
    }

    pub fn native(
        name: &str,
        arg_count: usize,
        arg_types: Vec<Type>,
        return_type: Type,
        nativefunc: fn(&mut VirtualMachine),
    ) -> Self {
        Self {
            name: name.to_string(),
            position: 0,
            arg_count,
            arg_types,
            return_type,
            is_native: true,
            native_pointer: Some(nativefunc),
        }
    }
}

use std::fmt;
impl fmt::Debug for FuncInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_native {
            write!(f, "<NativeFunc {:?}>", self.return_type)
        } else {
            write!(f, "<Func {:?}>", self.return_type)
        }
    }
}

impl fmt::Display for FuncInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_native {
            write!(f, "<NativeFunc {:?}>", self.return_type)
        } else {
            write!(f, "<Func {:?}>", self.return_type)
        }
    }
}

#[derive(Debug)]
pub struct ByteChunk {
    pub entry_point: usize,
    pub code     : Code,
    pub constants: ConstantTable,
    pub functions: HashMap<usize, FuncInfo>,
}


#[derive(Debug)]
pub struct BytecodeGenerator {
    // Things that we return
    pub code: Code,
    pub const_table: ConstantTable,

    // To keep track of definitions
    pub defs: TypeArena,
    pub function_table: HashMap<usize, FuncInfo>,

    pub last_loop_start: usize,
    depth: u16,
    pub break_call: Vec<usize>,
}

pub struct CodeState {
    pub constants: ConstantTable,
    pub types:     TypeArena,
    pub depth:     u16,
}


impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {
            code: Code::default(),
            const_table: ConstantTable::default(),
            last_loop_start: usize::max_value(),
            defs: TypeArena::new(),
            function_table: HashMap::new(),
            depth: 0,
            break_call: Vec::new(),
        }
    }

    pub fn traverse_ast(mut self, ast: ASTree) -> Result<ByteChunk, ()> {
        // use super::builtin_functions::apply_native_functions;
        // apply_native_functions(&mut self);

        for function in &ast.functions {
            self.prepare_function(&ast, function);
        }
        let entry_point = self.code.current_length();
        for node in ast.ast.iter() {
            self.handle_astnode(&ast, node);
        }

        let bytechunk = ByteChunk {
            entry_point,
            code:      self.code,
            constants: self.const_table,
            functions: self.function_table,
        };
        Ok(bytechunk)
    }

    fn prepare_function(&mut self, ast: &ASTree, function: &FunctionData) {
        let decl_info = &function.it;
        let block_id = function.block_id;

        // 実際に使うことはないが、特定の関数が必要とするため用意
        let mut placeholder = stmt::StatementHandleResult {
            span: EMPTY_SPAN,
            index: 0,
            info: StatementInfo::Nothing,
        };

        // 関数自体の戻り値をグローバルに定義しておく
        let ret_ty = astconv::annotation_to_type(&decl_info.dectype);
        let idx = if let Some(x) = ret_ty.as_ref() {
            self.defs.add_global(TypeContext::Solved(x.clone()), &decl_info.name)
        } else {
            self.defs.add_global(self.defs.new_existential(), &decl_info.name)
        }.unwrap();

        // 現在のコードセクションとローカルセクションを保存し、
        // コールフレームを作るためローカル変数のインデックスをリセットした上で
        // 変数を引数として処理する
        let main_code = mem::replace(&mut self.code, Code::default());
        let function_starts_at = main_code.current_length();

        // depth を加算する事でローカル領域として処理を進める
        self.enter_local();
        let arg_count = function.args.len();
        let mut argument_types = Vec::new();
        for arg_decl_info in function.args.iter() {
            let arg_type = astconv::annotation_to_type(&arg_decl_info.dectype).unwrap();
            argument_types.push(arg_type);
            self.handle_declaration_data(ast, &mut placeholder, arg_decl_info);
        }

        // コードセクションも上記と同様に処理する
        let block = ast.get_stmt(block_id);
        if let Statement::Block(ref blk) = block {
            for statement in blk.statements.iter() {
                let result = self.handle_stmt(ast, statement, EMPTY_SPAN);
                match result.info {
                    StatementInfo::Return(dtype) => {
                        if self.defs.global[idx].is_not_solved() {
                            if dtype.is_some() {
                                self.defs.determine_global(idx, dtype.unwrap());
                            }
                        } else {
                            if self.defs.global[idx].dtype.inner_ref() != dtype.as_ref() {
                                panic!("Type Mismatch");
                            }
                        }
                    }
                    StatementInfo::Continue(..) => {
                        panic!("You cannot use return within function scope.")
                    }
                    StatementInfo::Break(..) => {
                        panic!("You cannot use break within function scope.")
                    }
                    _ => (),
                }
            }
        } else {
            panic!();
        }

        if self.defs.global[idx].is_not_solved() {
            if decl_info.is_inferred() {
                self.code.write_null(EMPTY_SPAN);
                self.code.push_opcode(OpCode::Return, EMPTY_SPAN);
                self.defs.determine_global(idx, Type::null());
            } else {
                panic!("Return type specified but nothing returned");
            }
        }

        let func_info = FuncInfo::new(
            &decl_info.name,
            function_starts_at,
            arg_count,
            argument_types,
            self.defs.global[idx].dtype.inner_ref().unwrap().clone(),
            false,
        );

        self.function_table.insert(idx, func_info);
        self.leave_local();

        // 元のコードを復活させ、後付けでマージする
        let function_code = mem::replace(&mut self.code, main_code);
        self.code.merge(function_code);

        // 関数に関する情報を保存してテーブルに保存
        placeholder.index = self.code.current_length();
    }

    fn handle_astnode(&mut self, ast: &ASTree, node: &AstNode) -> StatementHandleResult {
        self.handle_stmt(ast, &node.stmt_id, node.span)
    }

    fn handle_declaration_data(
        &mut self,
        ast: &ASTree,
        out: &mut StatementHandleResult,
        decl: &DeclarationData,
    ) {

        let mut empty_expr = false;
        let expression = decl.expr.as_ref();
        let mut actual_type = Type::default();
        match expression {
            Some(expr) => {
                let expr_handle_result = self.handle_expr(ast, expr, out.span);
                // println!("Declaration_Expr: {:?}", &expr_handle_result);
                actual_type = expr_handle_result._type;
            }
            None => {
                empty_expr = true;
            }
        };

        let declared_type = if decl.is_annotated() {
            let annotation = astconv::annotation_to_type(&decl.dectype);
            match annotation {
                Some(x) => TypeContext::Solved(x),
                None => {
                    if let (true, addr) = self.defs.find_global(&decl.name) {
                        // TODO - @DumbCode; it's possible to copy the entire struct in here.
                        // It could happen way often and it's expensive as hell.
                        // it might be better to keep the position of the definition instead of whole data. 
                        
                        self.defs.global[addr].dtype.clone()
                    } else {
                        panic!();
                    }
                }
            }
        } else {
            if empty_expr {
                self.defs.new_existential()
            } else {
                TypeContext::Solved(actual_type)
            }
        };

        let dectype = match declared_type.inner_ref() {
            Some(x) => x.clone(),
            None => panic!("I expected this to be solved, but instead got: {:?}", declared_type)
        };

        // TODO: Dumb Code
        let def_position = {
            let def_result = if self.is_local() {
                self.defs.add_local(declared_type, &decl.name, self.depth)
            } else {
                self.defs.add_global(declared_type, &decl.name)
            };

            match def_result {
                Ok(n) => n,
                Err(p) => panic!("Variable Declared TWICE!"),
            }
        };

        if empty_expr {
            out.info = StatementInfo::Declaration {
                is_initialized: empty_expr,
                def: (def_position, !self.is_local()),
            };
            return;
        }

        if decl.kind == DeclKind::Argument {
            out.index = self.code.current_length();
            out.info = StatementInfo::Declaration {
                is_initialized: empty_expr,
                def: (def_position, !self.is_local()),
            };
            return;
        }

        let opcode = self.get_store_opcode(&dectype, !self.is_local());
        let operands = def_position as u16;

        self.code.push_opcode(opcode, out.span);
        out.index = self
            .code
            .push_operands(operands.to_ne_bytes().to_vec(), out.span);

        out.info = StatementInfo::Declaration {
            is_initialized: empty_expr,
            def: (def_position, !self.is_local()),
        };
    }

    fn enter_local(&mut self) -> usize {
        self.depth += 1;
        self.depth as usize
    }

    fn leave_local(&mut self) -> usize {
        self.depth -= 1;
        self.defs.ditch_out_of_scope(self.depth);
        self.depth as usize
    }

    fn is_local(&self) -> bool {
        self.depth > 0
    }

    fn get_load_opcode(&self, _type: &Type, is_global: bool) -> OpCode {
        self.get_type_based_opcode(_type, is_global, true)
    }

    fn get_store_opcode(&self, _type: &Type, is_global: bool) -> OpCode {
        self.get_type_based_opcode(_type, is_global, false)
    }

    fn get_type_based_opcode(&self, _type: &Type, is_global: bool, is_load: bool) -> OpCode {
        match _type.kind {
            TypeKind::Int if is_global && !is_load => OpCode::GIStore,
            TypeKind::Float if is_global && !is_load => OpCode::GFStore,
            TypeKind::Boolean if is_global && !is_load => OpCode::GBStore,
            TypeKind::Str if is_global && !is_load => OpCode::GSStore,
            TypeKind::Int if is_global && is_load => OpCode::GILoad,
            TypeKind::Float if is_global && is_load => OpCode::GFLoad,
            TypeKind::Boolean if is_global && is_load => OpCode::GBLoad,
            TypeKind::Str if is_global && is_load => OpCode::GSLoad,

            TypeKind::Int if !is_global && !is_load => OpCode::IStore,
            TypeKind::Float if !is_global && !is_load => OpCode::FStore,
            TypeKind::Boolean if !is_global && !is_load => OpCode::BStore,
            TypeKind::Str if !is_global && !is_load => OpCode::SStore,
            TypeKind::Int if !is_global && is_load => OpCode::ILoad,
            TypeKind::Float if !is_global && is_load => OpCode::FLoad,
            TypeKind::Boolean if !is_global && is_load => OpCode::BLoad,
            TypeKind::Str if !is_global && is_load => OpCode::SLoad,

            TypeKind::Compound { ref field } => {
                if field.len() == 2 && field[1].is_null() {
                    self.get_type_based_opcode(&field[0], is_global, is_load)
                } else {
                    panic!();
                }
            }
            _ => panic!(
                "Unsupported opcode for here: opcode: {:?}, is_global: {}, is_load: {}",
                _type, is_global, is_load
            ),

        }
    }

    fn fetch_declared(&self, name: &str) -> Option<(usize, bool)> {
        let (exists_in_local, lpos) = self.defs.find_local(&name, self.depth);
        if exists_in_local {
            return Some((lpos, false));
        }

        let (exists_globally, gpos) = self.defs.find_global(&name);

        if exists_globally {
            return Some((gpos, true));
        }
        None
    }
}
