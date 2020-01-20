use syntax_ast::{ast::*, tokenizer::token::TokenType};

use crate::typecheck::{self, astconv, tcheck};
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

#[derive(Debug, Clone, Hash, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // Const       = 0b00010000,
    Const8 = 0b0001_0001,   // usize分の大きさのオペランドを取る
    Const16 = 0b0001_0010,  // usize分の大きさのオペランドを取る
    Const32 = 0b0001_0011,  // usize分の大きさのオペランドを取る
    Const64 = 0b0001_0100,  // usize分の大きさのオペランドを取る
    ConstDyn = 0b0001_0111, // usize分の大きさのオペランドを取る

    // Operational = 0b00100000,
    Return = 0b0010_0000,
    Push = 0b0010_0010,
    PushPtr = 0b0001_1000, // usize分の大きさのオペランドを取る
    Pop = 0b0010_0011,
    BlockIn = 0b0010_0110,
    BlockOut = 0b0010_0111,

    // Arithmitic = 0b00110000,
    Add = 0b0011_0001,
    Sub = 0b0011_0010,
    Mul = 0b0011_0011,
    Div = 0b0011_0100,
    Mod = 0b0011_0101,
    Not = 0b0011_0111,

    // Logical     = 0b01000000,
    EqEq = 0b0100_0001,
    NotEq = 0b0100_0010,
    LessEq = 0b0100_0011,
    MoreEq = 0b0100_0100,
    Less = 0b0100_0101,
    More = 0b0100_0111,
    And = 0b0100_1000,
    Or = 0b0100_1001,
    Neg = 0b0100_1010,

    // Branching   = 0b01010000,
    Jump = 0b0101_0000,
    JumpIfFalse = 0b0101_0010,
    Call = 0b0101_0011,

    // Globals = 0b01100000,
    GILoad = 0b0110_0001,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GIStore = 0b0110_0010, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFLoad = 0b0110_0011,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFStore = 0b0110_0100, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSLoad = 0b0110_0101,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSStore = 0b0110_0110, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBLoad = 0b0110_0111,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBStore = 0b0110_1000, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る

    // Locals      = 0b01110000,
    ILoad = 0b0111_0001,  // index(u16 - u8 + u8) の2つのオペランドを取る
    IStore = 0b0111_0010, // index(u16 - u8 + u8) の2つのオペランドを取る
    FLoad = 0b0111_0011,  // index(u16 - u8 + u8) の2つのオペランドを取る
    FStore = 0b0111_0100, // index(u16 - u8 + u8) の2つのオペランドを取る
    SLoad = 0b0111_0101,  // index(u16 - u8 + u8) の2つのオペランドを取る
    SStore = 0b0111_0110, // index(u16 - u8 + u8) の2つのオペランドを取る
    BLoad = 0b0111_0111,  // index(u16 - u8 + u8) の2つのオペランドを取る
    BStore = 0b0111_1000, // index(u16 - u8 + u8) の2つのオペランドを取る

    // System     = 0b11110000,
    Interrupt = 0b1111_1111,
    DebugPrint = 0b1111_0001,
}

#[cfg(target_pointer_width = "32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width = "64")]
const USIZE_LENGTH: usize = 8;

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
        let padding = disassemble_pad(OpCode::from_u8(opbyte).unwrap_or(OpCode::Interrupt));
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

fn disassemble_pad(opcode: OpCode) -> usize {
    match opcode {
        OpCode::Const8 | OpCode::Const16 | OpCode::Const32 | OpCode::Const64 | OpCode::ConstDyn => {
            USIZE_LENGTH
        }

        OpCode::JumpIfFalse | OpCode::Jump => USIZE_LENGTH,
        OpCode::Call => 2,
        OpCode::PushPtr | OpCode::Push => USIZE_LENGTH,

        OpCode::GILoad | OpCode::GFLoad | OpCode::GSLoad | OpCode::GBLoad => 2,
        OpCode::GIStore | OpCode::GFStore | OpCode::GSStore | OpCode::GBStore => 2,

        OpCode::ILoad | OpCode::FLoad | OpCode::SLoad | OpCode::BLoad => 2,
        OpCode::IStore | OpCode::FStore | OpCode::SStore | OpCode::BStore => 2,
        _ => 0,
    }
}

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
    pub code: Code,
    pub constants: ConstantTable,
    pub functions: HashMap<usize, FuncInfo>,
}

#[derive(Debug)]
pub struct BytecodeGenerator {
    // Things that we return
    pub code: Code,
    pub const_table: ConstantTable,

    // To keep track of definitions
    pub defs: typecheck::TypeArena,
    pub function_table: HashMap<usize, FuncInfo>,

    // Keep track of block nesting
    pub last_loop_start: usize,
    depth: u16
    pub break_call: Vec<usize>,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {
            code: Code::default(),
            const_table: ConstantTable::default(),
            last_loop_start: usize::max_value(),
            defs: typecheck::TypeArena::new(),
            function_table: HashMap::new(),
            depth: 0,
            break_call: Vec::new(),
        }
    }

    pub fn traverse_ast(mut self, ast: ParsedResult) -> Result<ByteChunk, ()> {
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


    fn prepare_function(&mut self, ast: &ParsedResult, function: &FunctionData) {
        let decl_info = &function.it;
        let block_id = function.block_id;

        // 実際に使うことはないが、特定の関数が必要とするため用意
        let mut placeholder = stmt::StatementHandleResult {
            span: EMPTY_SPAN,
            index: 0,
            info: StatementInfo::Nothing,
        };

        // 引数のデータをコピーして数を保存

        // 関数自体の戻り値をグローバルに定義しておく
        let ret_ty = astconv::annotation_to_type(&decl_info.dectype);
        let idx = self.add_global(ret_ty.as_ref().unwrap().clone(), &decl_info.name);

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

        let func_info = FuncInfo::new(
            &decl_info.name,
            function_starts_at,
            arg_count,
            argument_types,
            ret_ty.clone().unwrap(),
            false,
        );
        self.function_table.insert(idx, func_info);

        let mut returned_type: Option<Type> = None;

        // コードセクションも上記と同様に処理する
        let block = ast.get_stmt(block_id);
        if let Statement::Block(ref blk) = block {
            for statement in blk.statements.iter() {
                let result = self.handle_stmt(ast, statement, EMPTY_SPAN);
                match result.info {
                    StatementInfo::Return(dtype) => {
                        if decl_info.is_inferred() {
                            if returned_type.is_none() {
                                returned_type = dtype;
                            } else if returned_type.as_ref() != dtype.as_ref() {
                                panic!("multiple return detected but the type is inconsistent: first return and what I expected was {:?}, then later got {:?}", returned_type, dtype);
                            }
                        } else {
                            if ret_ty.as_ref() != dtype.as_ref() {
                                panic!(
                                    "Return type does not match! expected {:?}, got {:?}",
                                    decl_info.dectype, dtype
                                );
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

        if returned_type.is_none() {
            if decl_info.dectype == ParsedType::pUnknown {
                self.code.write_null(EMPTY_SPAN);
                self.code.push_opcode(OpCode::Return, EMPTY_SPAN);
            } else {
                panic!("Return type specified but nothing returned");
            }
        }

        self.leave_local();

        // 元のコードを復活させ、後付けでマージする
        let function_code = mem::replace(&mut self.code, main_code);
        self.code.merge(function_code);

        // 関数に関する情報を保存してテーブルに保存
        placeholder.index = self.code.current_length();
    }

    fn handle_astnode(&mut self, ast: &ParsedResult, node: &AstNode) -> StatementHandleResult {
        self.handle_stmt(ast, &node.stmt_id, node.span)
    }

    fn handle_declaration_data(
        &mut self,
        ast: &ParsedResult,
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
            TypeContext::Annotated(astconv::annotation_to_type(&decl.dectype))
        } else {
            if !empty_expr {
                TypeContext::Solved(actual_type)
            } else {
                // Kind of unreachable.
                TypeContext::Existential(decl.name.clone())
            }
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
                Err(p) => panic!("Variable Declared TWICE!");
            }
        }

        // FIXME @Cleanup + @DumbCode - This can be super simplified
        if self.is_local() {
            if decl.kind == DeclKind::Argument {
                out.index = self.code.current_length();
            } else {
                let opcode = self.get_store_opcode(&final_type, false);
                let operands = def_position as u16;

                self.code.push_opcode(opcode, out.span);
                out.index = self
                    .code
                    .push_operands(operands.to_ne_bytes().to_vec(), out.span);
            }
        } else {
            let opcode = self.get_store_opcode(&final_type, true);
            let operands = def_position as u16;

            self.code.push_opcode(opcode, out.span);
            out.index = self
                .code
                .push_operands(operands.to_ne_bytes().to_vec(), out.span);
        }
        out.info = StatementInfo::Declaration {
            is_initialized: empty_expr,
            dtype: final_type,
        };
    }

    fn enter_local(&mut self) -> usize {
        self.depth += 1;
        self.depth
    }

    fn leave_local(&mut self) -> usize {
        self.depth -= 1;
        self.defs.ditch_out_of_scope(self.depth);
        self.depth
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
            _ => panic!(
                "Unsupported opcode for here: opcode: {:?}, is_global: {}, is_load: {}",
                _type, is_global, is_load
            ),
        }
    }
}
