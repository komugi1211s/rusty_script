use syntax_ast::{
    ast_data::{DeclarationData, Expr, FunctionData, ParsedResult, Statement, StatementNode, Literal, LiteralKind },
    token::TokenType,
};

use types::types::{ Type, TypeKind, TypeOption };
use crate::vm::{ VirtualMachine };
use crate::typecheck;

use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::mem;

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
    Neg = 0b0011_1000,

    // Logical     = 0b01000000,
    EqEq = 0b0100_0001,
    NotEq = 0b0100_0010,
    LessEq = 0b0100_0011,
    MoreEq = 0b0100_0100,
    Less = 0b0100_0101,
    More = 0b0100_0111,
    And = 0b0100_1000,
    Or = 0b0100_1001,

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

fn literal_to_builtin_value(lit: &Literal) -> Vec<u8> {
    match lit.kind {
        LiteralKind::Int => {
            let item = lit.tok.lexeme.parse::<i64>().unwrap();
            i64::to_ne_bytes(item).to_vec()
        }
        LiteralKind::Str => {
            lit.tok.lexeme.as_bytes().collect::<Vec<u8>>()
        }
        LiteralKind::Float => {
            let item = lit.tok.lexeme.parse::<f64>().unwrap();
            u64::to_ne_bytes(f64::to_bits(item)).to_vec()
        }
        LiteralKind::Bool => {
            match lit.tok {
                "true" => vec![1],
                "false" => vec![0],
            }
        }
        LiteralKind::Null => vec![]
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
        self.push_operands(0usize.to_ne_bytes().to_vec(), line);
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
    pub name: String,
    pub position: usize,
    pub arg_count: usize,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
    pub is_native: bool,
    pub native_pointer: Option<fn(&mut VirtualMachine)>,
}

impl FuncInfo
{
    fn new(name: &str, position: usize, arg_count: usize, arg_types: Vec<Type>, return_type: Type, is_native: bool) -> Self
    {
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

    pub fn native(name: &str, arg_count: usize, arg_types: Vec<Type>, return_type: Type, nativefunc: fn(&mut VirtualMachine)) -> Self {
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
    pub functions: HashMap<usize, FuncInfo>,
}

type Definition = (Type, String); 
#[derive(Debug)]
pub struct BytecodeGenerator {
    // Things that we return
    pub code: Code,
    pub const_table: ConstantTable,

    // To keep track of definitions
    pub current_define: Vec<Definition>,
    pub global_define : Vec<Definition>,
    pub function_table: HashMap<usize, FuncInfo>,

    // Keep track of block nesting
    pub last_loop_start: usize,
    current_block: usize,
    pub break_call: Vec<usize>,
}


impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {
            code: Code::default(),
            const_table: ConstantTable::default(),
            last_loop_start: usize::max_value(),
            current_define: Vec::new(),
            global_define : Vec::new(),
            function_table: HashMap::new(),
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
        self.const_table.push_data(usize::max_value().to_ne_bytes().to_vec());
        let bytechunk = ByteChunk {
            entry_point,
            code: self.code,
            constants: self.const_table,
            functions: self.function_table,
        };
        Ok(bytechunk)
    }

    fn find_local(&self, name: &String) -> (bool, usize) {
        let is_exist = self.current_define.iter().position(|x| &x.1 == name);
        (is_exist.is_some(), is_exist.unwrap_or(0))
    }

    fn find_global(&self, name: &String) -> (bool, usize) {
        let is_exist = self.global_define.iter().position(|x| &x.1 == name);
        (is_exist.is_some(), is_exist.unwrap_or(0))
    }

    fn prepare_function(
        &mut self,
        function: FunctionData,
    ) {
        let decl_info = function.it;
        let body_block = function.block;
        let return_type = { 
            let mut x = decl_info._type;
            x.option.remove(TypeOption::Func);
            x
        };
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
        self.global_define.push((decl_info._type, decl_info.name.clone()));
        let idx = self.global_define.len() - 1;
        
        // 現在のコードセクションとローカルセクションを保存し、
        // コールフレームを作るためローカル変数のインデックスをリセットした上で
        // 変数を引数として処理する
        let main_code = mem::replace(&mut self.code, Code::default());
        let main_local = mem::replace(&mut self.current_define, Vec::new());

        // current_block を加算する事でローカル領域として処理を進める
        self.current_block += 1;
        let mut argument_types = Vec::new();
        for arg_decl_info in _arguments {
            let arg_type = arg_decl_info._type;
            argument_types.push(arg_type);
            self.handle_declaration_data(&mut placeholder, arg_decl_info);
        }
        
        let function_starts_at = main_code.current_length();
        let func_info = FuncInfo::new(&decl_info.name, function_starts_at, arg_count, argument_types, return_type, false);
        self.function_table.insert(idx, func_info);

        let mut actually_returned = false;
        let mut first_return_type = Type::default();

        // コードセクションも上記と同様に処理する
        for statement in body_block.statements {
            let result = self.handle_stmt(statement, 0);
            match result.info {
                StatementInfo::Return(dtype) => {
                    if decl_info.is_inferred {
                        if first_return_type == Type::default() {
                            first_return_type = dtype;

                        } else if first_return_type != dtype {
                            panic!("multiple return detected but the type is inconsistent: first return and what I expected was {:?}, then later got {:?}", first_return_type, dtype);
                        }
                        actually_returned = true;
                    } else {
                        if return_type != dtype {
                            panic!("Return type does not match! expected {:?}, got {:?}", decl_info._type, dtype);
                        }
                        actually_returned = true;
                    }
                }
                StatementInfo::Continue(..) => panic!("You cannot use return within function scope."),
                StatementInfo::Break(..) => panic!("You cannot use break within function scope."),
                _ => (),
            }
        }

        if !actually_returned {
            if decl_info.is_inferred {
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
                    .push_operands(usize::max_value().to_ne_bytes().to_vec(), line);

                // Ifの終わりにまでJumpする為のIndexが要る
                let end_of_if_block = self.handle_stmt(*if_block, line).index;

                // jump opcodeがある位置のオペランドを、Ifブロックの終了アドレスで上書き
                self.code
                    .rewrite_operands(end_of_if_block.to_ne_bytes().to_vec(), jump_opcode_index);

                if let Some(else_block) = optional_else_block {
                    //
                    // Elseがあるので、Elseを避けるためのJump命令をIf命令の最後に叩き込む
                    let jump_block = self.code.push_opcode(OpCode::Jump, line);
                    let after_operand = self
                        .code
                        .push_operands(usize::max_value().to_ne_bytes().to_vec(), line);

                    // Ifブロックの終了アドレスがあった部分を、Else避けJump分を加味して調整
                    self.code
                        .rewrite_operands(after_operand.to_ne_bytes().to_vec(), jump_opcode_index);

                    // Ifブロックが丁度終わる位置のJumpオペランドを、Elseブロックの終了アドレスで上書き
                    let end_of_else_block = self.handle_stmt(*else_block, line).index;
                    self.code
                        .rewrite_operands(end_of_else_block.to_ne_bytes().to_vec(), jump_block);
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
                    .push_operands(usize::max_value().to_ne_bytes().to_vec(), line);

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
                    .push_operands(after_jump_conditional.to_ne_bytes().to_vec(), line);

                for i in self.break_call.drain(..) {
                    self.code.rewrite_operands(end_of_loop.to_ne_bytes().to_vec(), i);
                }
                self.code
                    .rewrite_operands(before_expr.to_ne_bytes().to_vec(), jump_conditional);
                self.last_loop_start = previous_loop_start;
                handled_result.index = end_of_loop;
                handled_result
            }

            Statement::Break => {
                if self.last_loop_start != usize::max_value() {
                    let break_index = self.code.push_opcode(OpCode::Jump, line);
                    self.code
                        .push_operands(usize::max_value().to_ne_bytes().to_vec(), line);
                    handled_result.info = StatementInfo::Break(break_index);
                }
                handled_result.index = self.code.current_length();
                handled_result
            }

            Statement::Continue => {
                if self.last_loop_start != usize::max_value() {
                    let continue_index = self.code.push_opcode(OpCode::Jump, line);
                    self.code
                        .push_operands(self.last_loop_start.to_ne_bytes().to_vec(), line);
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
                    Type::default()
                };
                handled_result.index = self.code.push_opcode(OpCode::Return, line);
                handled_result.info = StatementInfo::Return(result_type);
                handled_result
            }
            _ => unreachable!(),
        }
    }

    fn handle_declaration_data(&mut self, out: &mut StatementHandleResult, mut info: DeclarationData) {
        let mut value_index = 0;
        let mut actual_type = Type::default();

        let name = info.name.clone();
        let mut declared_type = info._type;
        let mut empty_expr = false;
        let expression = info.expr.take();

        match expression {
            Some(expr) => {
                let expr_handle_result = self.handle_expr(expr, out.line);
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

        fn decide_actual_type(decl: &DeclarationData, given_type: &Type) -> Type {
            let given_type: Type = given_type.clone();
            let given_type_is_null = given_type == Type::default();
            if decl.is_inferred {
                if given_type_is_null {
                    panic!("You cannot initialize Any type with null");
                }

                if decl.is_nullable {
                    panic!("You cannot initialize Any type with null");
                }

                return given_type;
            } else {
                if given_type_is_null {
                    if decl.is_nullable {
                        // Initializing Nullable variable with null.
                        // Fall Through.
                    } else if decl.is_argument {
                        // It's not really initializing it. not much of a problem.
                        // Fall through.
                    } else {
                        panic!("an attempt to initialize non-nullable variable with null");
                    }

                    return decl._type;
                } else {
                    if decl._type != given_type {
                        panic!("Type mismatch! {:?} != {:?}", decl._type, given_type);
                    }

                    return given_type;
                }
            }
        }

        declared_type = decide_actual_type(&info, &actual_type);

        // FIXME @Cleanup + @DumbCode - This can be super simplified
        if self.current_block > 0 {
            let (exists, pos) = self.find_local(&name);

            if exists {
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
                out.index = self.code.push_operands(operands.to_ne_bytes().to_vec(), out.line);
            }
        } else {
            let (exists, pos) = self.find_global(&name);

            if exists {
                panic!("Same Variable Declared TWICE.");
            }

            self.global_define.push((declared_type, name));
            let position = self.global_define.len() - 1;
            let opcode = self.get_store_opcode(declared_type, true);
            let operands = position as u16;

            self.code.push_opcode(opcode, out.line);
            out.index = self.code.push_operands(operands.to_ne_bytes().to_vec(), out.line);
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
        match _type.kind {
            TypeKind::Int     if is_global && !is_load  => OpCode::GIStore,
            TypeKind::Float   if is_global && !is_load  => OpCode::GFStore,
            TypeKind::Boolean if is_global && !is_load  => OpCode::GBStore,
            TypeKind::Str     if is_global && !is_load  => OpCode::GSStore,
            TypeKind::Int     if is_global && is_load   => OpCode::GILoad,
            TypeKind::Float   if is_global && is_load   => OpCode::GFLoad,
            TypeKind::Boolean if is_global && is_load   => OpCode::GBLoad,
            TypeKind::Str     if is_global && is_load   => OpCode::GSLoad,

            TypeKind::Int     if !is_global && !is_load => OpCode::IStore,
            TypeKind::Float   if !is_global && !is_load => OpCode::FStore,
            TypeKind::Boolean if !is_global && !is_load => OpCode::BStore,
            TypeKind::Str     if !is_global && !is_load => OpCode::SStore,
            TypeKind::Int     if !is_global && is_load  => OpCode::ILoad,
            TypeKind::Float   if !is_global && is_load  => OpCode::FLoad,
            TypeKind::Boolean if !is_global && is_load  => OpCode::BLoad,
            TypeKind::Str     if !is_global && is_load  => OpCode::SLoad,
            _ => panic!("Unsupported opcode for here: opcode: {:?}, is_global: {}, is_load: {}", _type, is_global, is_load),
        }
    }

    fn handle_literal(
        &mut self,
        out: &mut ExpressionHandleResult,
        lit: Literal,
        line: usize,
    ) {
        let _type = typecheck::literal_to_type(&lit);
        let opcode = type_to_const_opcode(&_type);
        let value = literal_to_builtin_value(&lit);
        let length = value.len();

        let start_index = if opcode == OpCode::ConstDyn {
            let x = self.const_table.push_data(length.to_ne_bytes().to_vec());
            self.const_table.push_data(value);
            x
        } else {
            self.const_table.push_data(value)
        };

        self.const_table.types.insert(start_index, _type);
        self.code.push_opcode(opcode, line);
        let index = self.code.push_operands(start_index.to_ne_bytes().to_vec(), line);
        out._type = _type;
        out.index = index;
    }

    fn handle_expr(&mut self, expr: Expr, line: usize) -> ExpressionHandleResult {
        let mut handled_result = ExpressionHandleResult {
            line,
            index: 0,
            _type: Type::default(),
        };

        match expr {
            Expr::Variable(name) => {
                // let data = self.variable.get(name).unwrap();
                // TODO: handle it without making a clone.
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

                    let _type = self.global_define[pos_global].0.clone();
                    let opcode = self.get_load_opcode(_type, true);
                    handled_result._type = _type;
                    self.code.push_opcode(opcode, line);

                    let index = pos_global as u16;
                    handled_result.index = self.code.push_operands(index.to_ne_bytes().to_vec(), line);
                    return handled_result;
                }

                let _type = self.current_define[pos].0.clone();
                let opcode = self.get_load_opcode(_type, false);
                handled_result._type = _type;
                self.code.push_opcode(opcode, line);

                let index = pos as u16;
                handled_result.index = self.code.push_operands(index.to_ne_bytes().to_vec(), line);
                handled_result
            }
            Expr::Literal(literal) => {
                self.handle_literal(&mut handled_result, literal, line);
                handled_result
            }

            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(*left, line);
                let right = self.handle_expr(*right, line);

                handled_result._type =
                    typecheck::type_after_binary(&left._type, &right._type, &operator.tokentype)
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
                handled_result._type = Type::boolean();
                handled_result
            }
            Expr::Unary(expr, operator) => {
                let another_result = self.handle_expr(*expr, line);
                handled_result._type =
                    typecheck::type_after_unary(&another_result._type, &operator.tokentype).unwrap();
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
                let (exists, position, is_global) = {
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
                };

                if !exists {
                    panic!("Undeclared / defined variable!");
                }

                let declared_type = {
                    if is_global {
                        self.global_define[position].0
                    } else {
                        self.current_define[position].0
                    }
                };

                let expression_result = self.handle_expr(*expr, line);
                let expression_type = expression_result._type;

                if declared_type != expression_type {
                    panic!(
                        "Type Mismatch when Assigning: {:?} != {:?}",
                        declared_type, expression_type 
                    );
                }

                let opcode = self.get_store_opcode(expression_type, is_global);
                let operands = position as u16;
                self.code.push_opcode(opcode, line);
                handled_result._type = expression_type;
                handled_result.index = self.code.push_operands(operands.to_ne_bytes().to_vec(), line);
                handled_result
            }
            Expr::FunctionCall(func_name, _open_paren, mut arguments) => {
                if let Expr::Variable(name) = *func_name {
                    
                    let (exists, position) = self.find_global(&name);
                    if !exists {
                        panic!("Undefined Function.");
                    }

                    let arg_count = self.function_table[&position].arg_count;
                    let arg_types = self.function_table[&position].arg_types.clone();
                    let return_type = self.function_table[&position].return_type;
                    if arg_count != arguments.len()
                    {
                        panic!("Too Few / much arguments provided. require {} arg(s)", arg_count);
                    }
                    if 0 < arg_count {
                        let argument_end = arg_count - 1;
                        arguments.reverse();
                        for (i, arg) in arguments.drain(..).enumerate() {
                            let handled_type = self.handle_expr(arg, line)._type;
                            let current_type = arg_types[argument_end - i];
                            if current_type != handled_type {
                                panic!("Type Mismatch when calling a function!! at line {}, {:?} != {:?}", line, current_type, handled_type);
                            }
                        }
                    }
                    self.code.push_opcode(OpCode::Call, line);
                    let jump_here = self.code.push_operands((position as u16).to_ne_bytes().to_vec(), line);
                    handled_result._type = return_type; 
                    handled_result.index = jump_here;
                }
                handled_result
            }
            _ => unreachable!(),
        }
    }
}
