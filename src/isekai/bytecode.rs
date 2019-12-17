use super::parse::{DeclarationData, Expr, FunctionData, ParsedResult, Statement, StatementNode};
use super::token::TokenType;
use super::types::{ toVmByte, Constant, OpCode, Type };
use super::vm::{ VirtualMachine };
use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::mem;

#[cfg(target_pointer_width = "32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width = "64")]
const USIZE_LENGTH: usize = 8;

#[derive(Default, Debug)]
pub struct Code {
    pub bytes: Vec<u8>,
    pub line: Vec<usize>,
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
        self.line.extend(other.line);
        current_length
    }

    pub fn push_operand(&mut self, operand: u8, line: usize) -> usize {
        self.bytes.push(operand);
        self.line.push(line);
        self.bytes.len()
    }

    pub fn current_length(&self) -> usize {
        self.bytes.len()
    }

    pub fn push_opcode(&mut self, code: OpCode, line: usize) -> usize {
        self.push_operand(code as u8, line)
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

    pub fn push_operands(&mut self, operands: Vec<u8>, line: usize) -> usize {
        for i in &operands {
            self.push_operand(*i, line);
        }
        self.bytes.len()
    }

    pub fn write_null(&mut self, line: usize) -> usize {
        self.push_opcode(OpCode::PushPtr, line);
        self.push_operands(0usize.to_vm_byte(), line);
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
type Local = (Type, u16);

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
        let padding =
            disassemble_pad(OpCode::from_u8(opbyte).unwrap_or(OpCode::Interrupt));
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
        OpCode::Const8
        | OpCode::Const16
        | OpCode::Const32
        | OpCode::Const64
        | OpCode::ConstDyn => USIZE_LENGTH,

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
    pub name: u16,
    pub position: usize,
    pub arg_count: usize,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
    pub is_native: bool,
    pub native_pointer: Option<fn(&mut VirtualMachine)>,
}

impl FuncInfo
{
    fn new(name: u16, position: usize, arg_count: usize, arg_types: Vec<Type>, return_type: Type, is_native: bool) -> Self
    {
        Self {
            name,
            position,
            arg_count,
            arg_types,
            return_type,
            is_native,
            native_pointer: None,
        }
    }

    pub fn native(name: u16, arg_count: usize, arg_types: Vec<Type>, return_type: Type, nativefunc: fn(&mut VirtualMachine)) -> Self {
        Self {
            name,
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
pub struct BytecodeGenerator {
    // Things that we return
    pub code: Code,
    pub const_table: ConstantTable,

    // To keep track of definitions
    pub current_define: Vec<Local>,
    pub global_type: HashMap<u16, Type>,
    pub function_table: HashMap<u16, FuncInfo>,

    // Keep track of block nesting
    pub last_loop_start: usize,
    current_block: usize,
    pub break_call: Vec<usize>,
}

#[derive(Debug)]
struct StatementHandleResult {
    index: usize,
    line: usize,
    info: StatementInfo
}

#[derive(Debug)]
enum StatementInfo {
    Nothing,
    Break(usize),
    Continue(usize),
    Declaration { is_initialized: bool, dtype: Type },
    Return(Type),
}

#[derive(Debug)]
struct ExpressionHandleResult {
    index: usize,
    line: usize,
    _type: Type,
}

#[derive(Debug)]
pub struct ByteChunk {
    pub entry_point: usize,
    pub code: Code,
    pub constants: ConstantTable,
    pub functions: HashMap<u16, FuncInfo>,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {
            code: Code::default(),
            const_table: ConstantTable::default(),
            global_type: HashMap::new(),
            function_table: HashMap::new(),
            current_define: Vec::new(),
            last_loop_start: usize::max_value(),
            current_block: 0,
            break_call: Vec::new(),
        }
    }

    pub fn traverse_ast(mut self, ast: ParsedResult) -> Result<ByteChunk, ()> {
        use super::builtin_functions::apply_native_functions;
        apply_native_functions(&mut self);

        for function in ast.functions {
            self.prepare_function(function);
        }
        let entry_point = self.code.current_length();
        for i in ast.statements {
            self.handle_stmt(i.value, i.line);
        }
        self.const_table.push_data(usize::max_value().to_vm_byte());
        println!("{:04X}", entry_point);

        Ok(ByteChunk {
            entry_point,
            code: self.code,
            constants: self.const_table,
            functions: self.function_table,
        })
    }

    fn prepare_function(
        &mut self,
        function: FunctionData,
    ) {
        let decl_info = function.it;
        let body_block = function.block;
        // 実際に使うことはないが、特定の関数が必要とするため用意
        let mut placeholder = StatementHandleResult {
            line: 0,
            index: 0,
            info: StatementInfo::Nothing,
        };

        // 引数のデータをコピーして数を保存
        let _arguments = function.args;
        let arg_count = _arguments.len();

        // 関数自体の戻り値をグローバルに定義しておく
        self.global_type.insert(decl_info.name_u16, decl_info._type);
        
        // 現在のコードセクションとローカルセクションを保存し、
        // コールフレームを作るためローカル変数のインデックスをリセットした上で
        // 変数を引数として処理する
        let main_code = mem::replace(&mut self.code, Code::default());
        let main_local = mem::replace(&mut self.current_define, Vec::new());

        // current_block を加算する事でローカル領域として処理を進める
        self.current_block += 1;
        let mut argument_types = Vec::new();
        for arg_decl_info in _arguments {
            println!("{:?}", &arg_decl_info);
            let arg_type = arg_decl_info._type;
            argument_types.push(arg_type);
            self.handle_declaration_data(&mut placeholder, arg_decl_info);
        }

        let return_type = { 
            let mut x = decl_info._type;
            x.remove(Type::Func);
            x
        };
        
        let function_starts_at = main_code.current_length();
        let func_info = FuncInfo::new(decl_info.name_u16, function_starts_at, arg_count, argument_types, return_type, false);
        self.function_table.insert(decl_info.name_u16, func_info);

        let mut actually_returned = false;
        let mut first_return_type = Type::Any;

        // コードセクションも上記と同様に処理する
        for statement in body_block.statements {
            let result = self.handle_stmt(statement, 0);
            match result.info {
                StatementInfo::Return(dtype) => {
                    if !return_type.is_any() {
                        if !return_type.contains(dtype) {
                            panic!("Return type does not match! expected {:?}, got {:?}", return_type, dtype);
                        }
                        actually_returned = true;
                    } else {
                        if first_return_type == Type::Any {
                            first_return_type = dtype.clone();
                        } else if first_return_type != dtype {
                            panic!("multiple return detected but the type is inconsistent: first return and what I expected was {:?}, then later got {:?}", first_return_type, dtype);
                        }
                        if !return_type.contains(Type::Null) {
                            actually_returned = true;
                        }
                    }
                }
                StatementInfo::Continue(..) => panic!("You cannot use return within function scope."),
                StatementInfo::Break(..) => panic!("You cannot use break within function scope."),
                _ => (),
            }
        }

        if !actually_returned {
            if return_type.is_any() {
                self.handle_stmt(Statement::Return(None), 0);
            } else {
                panic!("Return type specified but nothing returned");
            }
        }

        self.current_block -= 1;

        // 元のコードを復活させ、後付けでマージする
        let _function_code = mem::replace(&mut self.code, main_code);
        let _function_local = mem::replace(&mut self.current_define, main_local);
        let function_index = self.code.merge(_function_code);

        // 関数に関する情報を保存してテーブルに保存
        placeholder.index = self.code.current_length();
    }

    pub fn write_const(&mut self, constant: Constant, line: usize) -> usize {
        let opcode = constant.code;
        let value = constant.value;
        let length = value.len();

        let start_index = if opcode == OpCode::ConstDyn {
            let x = self.const_table.push_data(length.to_vm_byte());
            self.const_table.push_data(value);
            x
        } else {
            self.const_table.push_data(value)
        };

        self.const_table
            .types
            .insert(start_index, constant.ctype.clone());

        self.code.push_opcode(opcode, line);
        self.code.push_operands(start_index.to_vm_byte(), line);
        start_index
    }

    fn handle_stmt(&mut self, data: Statement, line: usize) -> StatementHandleResult {
        let mut handled_result = StatementHandleResult { line, index: 0, info: StatementInfo::Nothing };

        match data {
            Statement::Expression(expr) => {
                handled_result.index = self.handle_expr(expr, line).index;
                handled_result
            }
            Statement::Decralation(declaration_info) => {
                self.handle_declaration_data(&mut handled_result, declaration_info);
                handled_result
            }

            Statement::Print(expr) => {
                self.handle_expr(expr, line);
                handled_result.index = self.code.push_opcode(OpCode::DebugPrint, line);
                handled_result
            }

            Statement::If(expr, if_block, optional_else_block) => {
                self.handle_expr(expr, line);

                // ジャンプ用のインデックスを作っておく
                let jump_opcode_index = self.code.push_opcode(OpCode::JumpIfFalse, line);
                self.code
                    .push_operands(usize::max_value().to_vm_byte(), line);

                // Ifの終わりにまでJumpする為のIndexが要る
                let end_of_if_block = self.handle_stmt(*if_block, line).index;

                // jump opcodeがある位置のオペランドを、Ifブロックの終了アドレスで上書き
                self.code
                    .rewrite_operands(end_of_if_block.to_vm_byte(), jump_opcode_index);

                if let Some(else_block) = optional_else_block {
                    //
                    // Elseがあるので、Elseを避けるためのJump命令をIf命令の最後に叩き込む
                    let jump_block = self.code.push_opcode(OpCode::Jump, line);
                    let after_operand = self
                        .code
                        .push_operands(usize::max_value().to_vm_byte(), line);

                    // Ifブロックの終了アドレスがあった部分を、Else避けJump分を加味して調整
                    self.code
                        .rewrite_operands(after_operand.to_vm_byte(), jump_opcode_index);

                    // Ifブロックが丁度終わる位置のJumpオペランドを、Elseブロックの終了アドレスで上書き
                    let end_of_else_block = self.handle_stmt(*else_block, line).index;
                    self.code
                        .rewrite_operands(end_of_else_block.to_vm_byte(), jump_block);
                }

                handled_result.index = self.code.current_length();
                handled_result
            }

            Statement::Block(block_data) => {
                // self.code.push_opcode(OpCode::BlockIn, line);
                self.current_block += 1;
                for i in block_data.statements {
                    match self.handle_stmt(i, line).info {
                        StatementInfo::Break(usize) => {
                            self.break_call.push(usize);
                        },
                        StatementInfo::Continue(..) => panic!("Continue does not supported"),
                        _ => (),
                    };
                }
                self.current_block -= 1;
                // handled_result.index = self.code.push_opcode(OpCode::BlockOut, line);
                handled_result.index = self.code.current_length();
                handled_result
            }

            Statement::While(expr, while_block) => {
                /*
                 どうやらアセンブラではWhileループは以下のように書かれるらしい:

                 Jump to Loop1
                 InsideLoop:
                   Loop Inside
                   Done, goes through to loop1

                 loop1:
                   Check if condition is true
                   when True, Jump to InsideLoop

                 なので、取り敢えずExprを判定する前に先にWhileBlockを処理しないとならない
                 Breakはloop1の後に、ContinueはInsideLoopの頭にジャンプする命令になる
                 https://stackoverflow.com/questions/28665528/while-do-while-for-loops-in-assembly-language-emu8086
                */

                let _before_loop = self.code.current_length();
                let jump_conditional = self.code.push_opcode(OpCode::Jump, line);
                let after_jump_conditional = self
                    .code
                    .push_operands(usize::max_value().to_vm_byte(), line);

                // この時点でContinue命令は after_jump_conditional に飛ぶようになる
                let previous_loop_start = self.last_loop_start;
                self.last_loop_start = after_jump_conditional;

                self.handle_stmt(*while_block, line);

                let before_expr = self.code.current_length();
                self.handle_expr(expr, line);
                self.code.push_opcode(OpCode::Not, line);

                self.code.push_opcode(OpCode::JumpIfFalse, line);
                let end_of_loop = self
                    .code
                    .push_operands(after_jump_conditional.to_vm_byte(), line);

                for i in self.break_call.drain(..) {
                    self.code.rewrite_operands(end_of_loop.to_vm_byte(), i);
                }
                self.code
                    .rewrite_operands(before_expr.to_vm_byte(), jump_conditional);
                self.last_loop_start = previous_loop_start;
                handled_result.index = end_of_loop;
                handled_result
            }

            Statement::Break => {
                if self.last_loop_start != usize::max_value() {
                    let break_index = self.code.push_opcode(OpCode::Jump, line);
                    self.code
                        .push_operands(usize::max_value().to_vm_byte(), line);
                    handled_result.info = StatementInfo::Break(break_index);
                }
                handled_result.index = self.code.current_length();
                handled_result
            }

            Statement::Continue => {
                if self.last_loop_start != usize::max_value() {
                    let continue_index = self.code.push_opcode(OpCode::Jump, line);
                    self.code
                        .push_operands(self.last_loop_start.to_vm_byte(), line);
                    handled_result.info = StatementInfo::Continue(continue_index);
                }
                handled_result.index = self.code.current_length();
                handled_result
            }
            Statement::Function(func_data) => {
                // self.handle_function_data(&mut handled_result, func_data, line);
                handled_result
            }
            Statement::Return(opt_expr) => {
                let result_type = if let Some(expr) = opt_expr {
                    let result_expr = self.handle_expr(expr, line);
                    result_expr._type
                }
                else {
                    self.code.write_null(line);
                    Type::Null
                };
                handled_result.index = self.code.push_opcode(OpCode::Return, line);
                handled_result.info = StatementInfo::Return(result_type);
                handled_result
            }
            _ => unreachable!(),
        }
    }

    fn handle_declaration_data(&mut self, out: &mut StatementHandleResult, info: DeclarationData) {
        let mut value_index = 0;
        let mut actual_type = Type::Null;

        let name = info.name_u16;
        let mut declared_type = info._type;
        let mut empty_expr = false;

        match info.expr {
            Some(expression) => {
                let expr_handle_result = self.handle_expr(expression, out.line);
                // println!("Declaration_Expr: {:?}", &expr_handle_result);
                value_index = expr_handle_result.index;
                actual_type = expr_handle_result._type;
            }
            None => {
                empty_expr = true;
                if !info.is_argument {
                    value_index = self.code.write_null(out.line)
                }
            }
        };

        if actual_type == Type::Null && !info.is_argument {
            if declared_type.is_any() {
                panic!("You cannot initialize Any type with null");
            }
            if !declared_type.is_nullable() {
                panic!("an attempt to initialize non-nullable variable with null");
            }
        } else if declared_type.is_any() {
            declared_type = actual_type
        
        // FIXME @Broken - this is error prone, function that returns int can be overwritten by single
        // primitive int value
        } else if !declared_type.contains(actual_type) {
            if !info.is_argument && actual_type == Type::Null {
                panic!("Type mismatch! {:?} != {:?}", declared_type, actual_type);
            }
        }

        if self.current_block > 0 {
            let is_exist = self.current_define.iter().position(|&x| x.1 == name);

            if is_exist.is_some() {
                panic!("Same Variable Declared TWICE.");
            }

            self.current_define.push((declared_type, name));
            if info.is_argument {
                out.index = self.code.current_length();
            } else {
                let position = self.current_define.len() - 1;
                let opcode = self.get_store_opcode(declared_type, false);
                let operands = position as u16;

                self.code.push_opcode(opcode, out.line);
                out.index = self.code.push_operands(operands.to_vm_byte(), out.line);
            }
        } else {
            self.global_type.insert(name, declared_type);
            declared_type.remove(Type::Null);
            let opcode = self.get_store_opcode(declared_type, true);
            self.code.push_opcode(opcode, out.line);
            out.index = self.code.push_operands(name.to_vm_byte(), out.line);
        }
        out.info = StatementInfo::Declaration { is_initialized: empty_expr, dtype: declared_type };
    }

    fn get_load_opcode(&self, _type: Type, is_global: bool) -> OpCode
    {
        self.get_type_based_opcode(_type, is_global, true)
    }

    fn get_store_opcode(&self, _type: Type, is_global: bool) -> OpCode
    {
        self.get_type_based_opcode(_type, is_global, false)
    }

    fn get_type_based_opcode(&self, _type: Type, is_global: bool, is_load: bool) -> OpCode {
        match _type {
            Type::Int     if is_global && !is_load  => OpCode::GIStore,
            Type::Float   if is_global && !is_load  => OpCode::GFStore,
            Type::Boolean if is_global && !is_load  => OpCode::GBStore,
            Type::Str     if is_global && !is_load  => OpCode::GSStore,
            Type::Int     if is_global && is_load   => OpCode::GILoad,
            Type::Float   if is_global && is_load   => OpCode::GFLoad,
            Type::Boolean if is_global && is_load   => OpCode::GBLoad,
            Type::Str     if is_global && is_load   => OpCode::GSLoad,

            Type::Int     if !is_global && !is_load => OpCode::IStore,
            Type::Float   if !is_global && !is_load => OpCode::FStore,
            Type::Boolean if !is_global && !is_load => OpCode::BStore,
            Type::Str     if !is_global && !is_load => OpCode::SStore,
            Type::Int     if !is_global && is_load  => OpCode::ILoad,
            Type::Float   if !is_global && is_load  => OpCode::FLoad,
            Type::Boolean if !is_global && is_load  => OpCode::BLoad,
            Type::Str     if !is_global && is_load  => OpCode::SLoad,
            _ => panic!("Unsupported opcode for here: opcode: {:?}, is_global: {}, is_load: {}", _type, is_global, is_load),
        }
    }

    fn handle_constant_data(
        &mut self,
        out: &mut ExpressionHandleResult,
        constant: Constant,
        line: usize,
    ) {
        let _type = constant.ctype.clone();
        let opcode = constant.code;
        let value = constant.value;
        let length = value.len();

        let start_index = if opcode == OpCode::ConstDyn {
            let x = self.const_table.push_data(length.to_vm_byte());
            self.const_table.push_data(value);
            x
        } else {
            self.const_table.push_data(value)
        };

        self.const_table.types.insert(start_index, _type);
        self.code.push_opcode(opcode, line);
        let index = self.code.push_operands(start_index.to_vm_byte(), line);
        out._type = _type;
        out.index = index;
    }

    fn handle_expr(&mut self, expr: Expr, line: usize) -> ExpressionHandleResult {
        let mut handled_result = ExpressionHandleResult {
            line,
            index: 0,
            _type: Type::Null,
        };
        match expr {
            Expr::Variable(name) => {
                // let data = self.variable.get(name).unwrap();
                // TODO: handle it without making a clone.
                let is_exist = self.current_define.iter().position(|&x| x.1 == name);

                if is_exist.is_none() {
                    // it could be a global variable;
                    if !self.global_type.contains_key(&name) {
                        panic!("Undefined Variable");
                    }

                    let _type = self.global_type.get(&name).unwrap().clone();
                    let opcode = self.get_load_opcode(_type, true);
                    handled_result._type = _type;
                    self.code.push_opcode(opcode, line);
                    handled_result.index = self.code.push_operands(name.to_vm_byte(), line);
                    return handled_result;
                }

                let mut index = 0;
                let (_type, _) = {
                    index = is_exist.unwrap();
                    self.current_define.get(index).unwrap().clone()
                };
                let index = index as u16;
                handled_result._type = _type;
                let opcode = self.get_load_opcode(_type, false);
                self.code.push_opcode(opcode, line);
                handled_result.index = self.code.push_operands(index.to_vm_byte(), line);
                handled_result
            }
            Expr::Literal(literal) => {
                // TODO: handle it without making a clone.
                // self.stack.push(literal);
                self.handle_constant_data(&mut handled_result, literal, line);
                handled_result
            }

            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(*left, line);
                let right = self.handle_expr(*right, line);

                handled_result._type =
                    Type::type_after_binary(&left._type, &right._type, &operator.tokentype)
                        .unwrap();
                handled_result.index = match operator.tokentype {
                    TokenType::Plus => self.code.push_opcode(OpCode::Add, line),
                    TokenType::Minus => self.code.push_opcode(OpCode::Sub, line),
                    TokenType::Asterisk => self.code.push_opcode(OpCode::Mul, line),
                    TokenType::Slash => self.code.push_opcode(OpCode::Div, line),
                    TokenType::Percent => self.code.push_opcode(OpCode::Mod, line),

                    // PartialEq Series
                    TokenType::NotEqual => self.code.push_opcode(OpCode::NotEq, line),
                    TokenType::EqualEqual => self.code.push_opcode(OpCode::EqEq, line),

                    // PartialOrd Series
                    TokenType::LessEqual => self.code.push_opcode(OpCode::LessEq, line),
                    TokenType::MoreEqual => self.code.push_opcode(OpCode::MoreEq, line),
                    TokenType::Less => self.code.push_opcode(OpCode::Less, line),
                    TokenType::More => self.code.push_opcode(OpCode::More, line),
                    _ => unreachable!(),
                };
                handled_result
            }
            Expr::Logical(left, right, operator) => {
                self.handle_expr(*left, line);
                self.handle_expr(*right, line);

                handled_result.index = match operator.tokentype {
                    TokenType::And => self.code.push_opcode(OpCode::And, line),
                    TokenType::Or => self.code.push_opcode(OpCode::Or, line),
                    _ => unreachable!(),
                };
                handled_result._type = Type::Boolean;
                handled_result
            }
            Expr::Unary(expr, operator) => {
                let another_result = self.handle_expr(*expr, line);
                handled_result._type =
                    Type::type_after_unary(&another_result._type, &operator.tokentype).unwrap();
                handled_result.index = match operator.tokentype {
                    TokenType::Bang => self.code.push_opcode(OpCode::Not, line),
                    TokenType::Minus => self.code.push_opcode(OpCode::Neg, line),
                    _ => unreachable!(),
                };
                handled_result._type = another_result._type;
                handled_result
            }
            Expr::Grouping(group) => self.handle_expr(*group, line),
            Expr::Assign(name, expr) => {
                let another_result = self.handle_expr(*expr, line);
                let is_exist = self.current_define.iter().position(|&x| x.1 == name);

                if is_exist.is_none() {
                    if !self.global_type.contains_key(&name) {
                        panic!("Undefined Variable");
                    }

                    let _type = self.global_type.get(&name).unwrap().clone();
                    let actual_type = &another_result._type;
                    if &_type != actual_type {
                        panic!("Type mismatch! {:?} != {:?}", _type, actual_type);
                    }
                    let opcode = match another_result._type {
                        Type::Boolean => OpCode::GBStore,
                        Type::Int => OpCode::GIStore,
                        Type::Str => OpCode::GSStore,
                        Type::Float => OpCode::GFStore,
                        _ => unreachable!(),
                    };
                    self.code.push_opcode(opcode, line);
                    handled_result._type = _type;
                    handled_result.index = self.code.push_operands(name.to_vm_byte(), line);
                } else {
                    let mut index = 0;
                    let (declared_type, _) = {
                        index = is_exist.unwrap();
                        self.current_define.get(index).unwrap()
                    }.clone();

                    if !&declared_type.contains(another_result._type) {
                        panic!(
                            "Type Mismatch when Assigning: {:?} != {:?}",
                            declared_type, another_result._type
                        );
                    }

                    let opcode = match another_result._type {
                        Type::Boolean => OpCode::BStore,
                        Type::Int => OpCode::IStore,
                        Type::Str => OpCode::SStore,
                        Type::Float => OpCode::FStore,
                        _ => unreachable!(),
                    };
                    self.code.push_opcode(opcode, line);
                    handled_result._type = declared_type.clone();
                    handled_result.index =
                        self.code.push_operands((index as u16).to_vm_byte(), line);
                }
                handled_result
            }
            Expr::FunctionCall(func_name, _open_paren, mut arguments) => {
                if let Expr::Variable(name) = *func_name {
                    if !self.function_table.contains_key(&name) {
                        panic!("Undefined Function.");
                    }

                    let arg_count = self.function_table[&name].arg_count;
                    let arg_types = self.function_table[&name].arg_types.clone();
                    let return_type = self.function_table[&name].return_type;
                    if arg_count != arguments.len()
                    {
                        panic!("Too Few / much arguments provided. require {} arg(s)", arg_count);
                    }
                    if 0 < arg_count {
                        let ARR_END = arg_count - 1;
                        arguments.reverse();
                        for (i, arg) in arguments.drain(..).enumerate() {
                            let handled_type = self.handle_expr(arg, line);
                            let current_type = arg_types[ARR_END - i];
                            if !current_type.contains(handled_type._type) {
                                panic!("Type Mismatch when calling a function!! at line {}, {:?} != {:?}", line, current_type, handled_type._type);
                            }
                            println!("Added ({:?}) = ({:?})", current_type, handled_type._type);
                        }
                    }
                    self.code.push_opcode(OpCode::Call, line);
                    let jump_here = self.code.push_operands(name.to_vm_byte(), line);
                    handled_result._type = return_type; 
                    handled_result._type.remove(Type::Func); 
                    handled_result.index = jump_here;
                }
                handled_result
            }
            _ => unreachable!(),
        }
    }
}
