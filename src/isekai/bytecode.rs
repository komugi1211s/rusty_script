
use super::types::{ Type, Value, Constant, OpCode, toVmByte };
use super::parse::{ Statement, Expr, ParserNode, DeclarationData, BlockData, FunctionData };
use super::token::{ Token, TokenType };
use std::mem;
use num_traits::FromPrimitive;
use std::collections::HashMap;

#[cfg(target_pointer_width="32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width="64")]
const USIZE_LENGTH: usize = 8;

#[derive(Default, Debug)]
pub struct ByteChunk
{
    pub data_chunk: Vec<u8>,
    // TODO: There should be more smart way to handle this
    pub _data_type: HashMap<usize, Type>,
    pub data_idx: usize,
    pub code_chunk: Vec<u8>,
    pub code_line: Vec<usize>,
    pub current_line: usize,
    pub code_idx: usize,
}

impl ByteChunk
{

    pub fn push_operand(&mut self, operand: u8, line: usize) -> usize
    {
        self.code_chunk.push(operand);
        self.code_line.push(line);
        self.code_idx += 1;
        self.code_idx
    }

    pub fn push_opcode(&mut self, code: OpCode, line: usize) -> usize
    {
        self.push_operand(code as u8, line)
    }

    pub fn rewrite_operand(&mut self, byte: u8, index: usize) -> usize
    {
        if self.code_idx < index
        {
            panic!("Rewrite opcode out of range");
        }

        self.code_chunk[index] = byte;
        self.code_idx
    }

    pub fn rewrite_operands(&mut self, bytes: Vec<u8>, index: usize) -> usize
    {
        for i in 0..bytes.len()
        {
            self.rewrite_operand(bytes[i], index + i);
        }
        self.code_idx
    }

    pub fn rewrite_opcode(&mut self, code: OpCode, index: usize) -> usize
    {
        self.rewrite_operand(code as u8, index)
    }

    pub fn push_operands(&mut self, operands: Vec<u8>, line: usize) -> usize
    {
        for i in &operands
        {
            self.push_operand(*i, line);
        }
        self.code_idx
    }

    pub fn write_const(&mut self, constant: Constant, line: usize) -> usize
    {
        let opcode = constant.code;
        let value = constant.value;
        let length = value.len();

        let start_index = if opcode == OpCode::ConstDyn
        {
            let x = self.push_data(length.to_vm_byte());
            self.push_data(value);
            x
        } else {
            self.push_data(value)
        };

        self.push_opcode(opcode, line);
        self.push_operands(start_index.to_vm_byte(), line);
        start_index
    }

    pub fn write_null(&mut self, line: usize) -> usize
    {
        self.push_opcode(OpCode::PushPtr, line);
        self.push_operands(0usize.to_vm_byte(), line);
        self.data_idx
    }

    pub fn push_data<T>(&mut self, data: T) -> usize
    where
        T: IntoIterator<Item=u8>
    {
        let start_at = self.data_idx;
        for i in data.into_iter()
        {
            self.data_chunk.push(i);
            self.data_idx += 1;
        }
        start_at
    }

    pub fn assert_datarange(&self, index: usize)
    {
        if self.data_idx <= index
        {
            panic!("Index Out of Range");
        }
    }

    pub fn read_data_8(&self, index: usize) -> u8
    {
        self.assert_datarange(index);
        self.data_chunk[index]
    }
    
    pub fn read_data_16(&self, index: usize) -> [u8; 2]
    {
        self.assert_datarange(index);
        let first = self.data_chunk[index];
        let second = self.data_chunk[index + 1];
        [first, second]
    }

    pub fn read_data_32(&self, index: usize) -> [u8; 4]
    {
        self.assert_datarange(index);
        let f = self.data_chunk[index];
        let s = self.data_chunk[index+1];
        let t = self.data_chunk[index+2];
        let fo = self.data_chunk[index+3];
        [f, s, t, fo]
    }
    
    pub fn read_data_64(&self, index: usize) -> [u8; 8]
    {
        self.assert_datarange(index);
        let mut data: [u8; 8] = [0; 8];
        
        for i in 0..8
        {
            data[i] = self.data_chunk[index+i];
        }
        data
    }

    pub fn read_data_dyn(&self, index: usize) -> Vec<u8>
    {
        self.assert_datarange(index);
        let mut data: Vec<u8> = Vec::new();
        let length: usize = {
            let mut size: [u8; USIZE_LENGTH] = [0; USIZE_LENGTH];
            for i in 0 .. USIZE_LENGTH
            {
                size[i] = self.data_chunk[index + i];
            }
            usize::from_ne_bytes(size)
        };
        for i in 0..length
        {
            data.push(self.data_chunk[index + USIZE_LENGTH + i]);
        }
        data
    }

    pub fn disassemble_all(&self) -> Vec<String>
    {
        self.disassemble(0, self.code_chunk.len())
    }

    pub fn disassemble(&self, start: usize, max_len: usize) -> Vec<String>
    {
        let mut vector: Vec<String> = Vec::new();
        vector.push(" ============ DISASSEMBLED ============ \n".to_string());
        let mut current = start;
        let max = max_len;
        while current < max
        {
            let opbyte = self.code_chunk[current];
            let mut opcode = format!("{:?}", OpCode::from_u8(opbyte).unwrap());
            opcode.make_ascii_uppercase();
            let padding = self.disassemble_pad(OpCode::from_u8(opbyte).unwrap());
            if 0 < padding {
                current += 1;

                let operand = self.get_disassemble_operand(current, padding);
                let formatted = format!("{:<16} {}", opcode, operand);
                let in_byte = format!("{:02X} {}", opbyte, operand);
                vector.push(format!(" | {:04X} | {:<48} | {}", current, formatted, in_byte)); 
                current += padding;
            } else {
                let in_byte = format!("{:02X}", opbyte);
                let formatted = format!("{:<16}", opcode);
                vector.push(format!(" | {:04X} | {:<48} | {}", current, formatted, in_byte));
                current += 1;
            }
        }

        vector.push("\n ========== DISASSEMBLE DONE ========== \n".to_string());
        vector.push(" ============= BYTE CODES ============= \n".to_string());
        let bytecode_hex: Vec<String> = (&self.code_chunk[start .. max]).iter()
                                        .map(|x| format!("{:02x}", x)).collect();
        vector.push(format!("{}", bytecode_hex.join(" ")));
        vector.push("\n =========== BYTE CODES DONE ========== \n".to_string());
        vector.push(" =============  BYTE DATA ============= \n".to_string());
        let bytedata_hex: Vec<String> = self.data_chunk.iter()
                                        .map(|x| format!("{:02x}", x)).collect();
        vector.push(format!("{}", bytedata_hex.join(" ")));
        vector.push("\n ============ BYTE DATA DONE ========== \n".to_string());
        vector
    }

    fn get_disassemble_operand(&self, current: usize, padding: usize) -> String
    {
        let operand = &self.code_chunk[(current) .. (current + padding)];
        let formatted: Vec<String> = operand.iter().map(|x| format!("{:02X}", x)).collect();
        formatted.join(" ")
    }

    fn disassemble_pad(&self, opcode: OpCode) -> usize
    {
        match opcode
        {
            OpCode::Const8  | 
            OpCode::Const16 | 
            OpCode::Const32 | 
            OpCode::Const64 |
            OpCode::ConstDyn => USIZE_LENGTH,


            OpCode::JumpIfFalse | OpCode::Jump => USIZE_LENGTH,
            OpCode::Call => 2,
            OpCode::PushPtr |
            OpCode::Push => USIZE_LENGTH,

            OpCode::GILoad  => 2,
            OpCode::GFLoad  => 2,
            OpCode::GSLoad  => 2,
            OpCode::GBLoad  => 2,
            OpCode::GIStore => 2,
            OpCode::GFStore => 2,
            OpCode::GSStore => 2,
            OpCode::GBStore => 2,

            OpCode::ILoad  => 2,
            OpCode::FLoad  => 2,
            OpCode::SLoad  => 2,
            OpCode::BLoad  => 2,
            OpCode::IStore => 2,
            OpCode::FStore => 2,
            OpCode::SStore => 2,
            OpCode::BStore => 2,
            _ => 0
        }
    }
}



type Local = (Type, u16);
#[derive(Debug)]
pub struct BytecodeGenerator
{
    pub chunk: ByteChunk,
    pub global_type: HashMap<u16, Type>,
    pub current_define: Vec<Local>,
    pub last_loop_start: usize,
    current_block: usize,
    pub break_call: Vec<usize>,
}

#[derive(Debug)]
struct StatementHandleResult
{
    index: usize,
    line: usize,
}

#[derive(Debug)]
struct ExpressionHandleResult
{
    index: usize,
    line: usize,
    _type: Type
}

impl BytecodeGenerator
{
    pub fn new() -> Self
    {
        Self {
            chunk: ByteChunk::default(),
            global_type: HashMap::new(),
            current_define: Vec::new(),
            last_loop_start: usize::max_value(),
            current_block: 0,
            break_call: Vec::new(),
        }
    }

    pub fn traverse_ast(mut self, ast: Vec<ParserNode>) -> Result<ByteChunk, ()>
    {
        for i in ast {
            self.handle_data(i);
        }
        self.chunk.push_data(usize::max_value().to_vm_byte());
        Ok(self.chunk)
    }

    fn handle_data(&mut self, data: ParserNode)
    {
        let line = data.line;
        self.handle_stmt(data.value, line);
    }

    fn handle_stmt(&mut self, data: Statement, line: usize) -> StatementHandleResult
    {
        let mut handled_result = StatementHandleResult {
            line: line,
            index: 0,
        };

        match data {
            Statement::Expression(expr) => { 
                handled_result.index = self.handle_expr(expr, line).index;
                handled_result
            },
            Statement::Decralation(declaration_info) =>
            {
                self.handle_declaration_data(&mut handled_result, declaration_info);
                handled_result
            },

            Statement::Print(expr) => {
                self.handle_expr(expr, line);
                handled_result.index = self.chunk.push_opcode(OpCode::DebugPrint, line);
                handled_result
            },

            Statement::If(expr, if_block, optional_else_block) => {
                self.handle_expr(expr, line);

                // ジャンプ用のインデックスを作っておく
                let jump_opcode_index = self.chunk.push_opcode(OpCode::JumpIfFalse, line);
                self.chunk.push_operands(usize::max_value().to_vm_byte(), line);

                // Ifの終わりにまでJumpする為のIndexが要る
                let end_of_if_block = self.handle_stmt(*if_block, line).index;

                // jump opcodeがある位置のオペランドを、Ifブロックの終了アドレスで上書き
                self.chunk.rewrite_operands(end_of_if_block.to_vm_byte(), jump_opcode_index);

                if let Some(else_block) = optional_else_block
                {
                    // 
                    // Elseがあるので、Elseを避けるためのJump命令をIf命令の最後に叩き込む
                    let jump_block = self.chunk.push_opcode(OpCode::Jump, line);
                    let after_operand = self.chunk.push_operands(usize::max_value().to_vm_byte(), line);

                    // Ifブロックの終了アドレスがあった部分を、Else避けJump分を加味して調整
                    self.chunk.rewrite_operands(after_operand.to_vm_byte(), jump_opcode_index);

                    // Ifブロックが丁度終わる位置のJumpオペランドを、Elseブロックの終了アドレスで上書き
                    let end_of_else_block = self.handle_stmt(*else_block, line).index;
                    self.chunk.rewrite_operands(end_of_else_block.to_vm_byte(), jump_block);
                }

                handled_result.index = self.chunk.code_idx;
                handled_result
            },

            Statement::Block(block_data) =>
            {
                self.chunk.push_opcode(OpCode::BlockIn, line);
                self.current_block += 1;
                for i in block_data.statements 
                {
                    self.handle_stmt(i, line);
                }
                self.current_block -= 1;
                handled_result.index = self.chunk.push_opcode(OpCode::BlockOut, line);
                handled_result
            },

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

                
                let before_loop = self.chunk.code_idx;
                let jump_conditional = self.chunk.push_opcode(OpCode::Jump, line);
                let after_jump_conditional = self.chunk.push_operands(usize::max_value().to_vm_byte(), line);

                // この時点でContinue命令は after_jump_conditional に飛ぶようになる
                let previous_loop_start = self.last_loop_start;
                self.last_loop_start = after_jump_conditional;

                self.handle_stmt(*while_block, line);

                let before_expr = self.chunk.code_idx;
                self.handle_expr(expr, line);
                self.chunk.push_opcode(OpCode::Not, line);

                self.chunk.push_opcode(OpCode::JumpIfFalse, line);
                let end_of_loop = self.chunk.push_operands(after_jump_conditional.to_vm_byte(), line);

                for i in self.break_call.drain(..)
                {
                    self.chunk.rewrite_operands(end_of_loop.to_vm_byte(), i);
                }
                self.chunk.rewrite_operands(before_expr.to_vm_byte(), jump_conditional);
                self.last_loop_start = previous_loop_start;
                handled_result.index = end_of_loop;
                handled_result
            },

            Statement::Break => {
                if self.last_loop_start != usize::max_value() {
                    let break_index = self.chunk.push_opcode(OpCode::Jump, line);
                    self.chunk.push_operands(usize::max_value().to_vm_byte(), line);
                    self.break_call.push(break_index);
                }
                handled_result.index = self.chunk.code_idx;
                handled_result
            },

            Statement::Continue => {
                if self.last_loop_start != usize::max_value() {
                    let break_index = self.chunk.push_opcode(OpCode::Jump, line);
                    self.chunk.push_operands(self.last_loop_start.to_vm_byte(), line);
                }
                handled_result.index = self.chunk.code_idx;
                handled_result
            },
            Statement::Function(func_data) => {
                self.handle_function_data(&mut handled_result, func_data);
                handled_result
            }
            _ => unreachable!(),
        }
    }

    fn handle_declaration_data(&mut self, out: &mut StatementHandleResult, info: DeclarationData)
    {
        let mut value_index = 0;
        let mut actual_type = Type::Null;

        let name = info.name_u16;
        let mut declared_type = info._type;
        let mut empty_expr = false;
        println!("Declaration: {:?}", &info);

        match info.expr {
            Some(expression) => { 
                let expr_handle_result = self.handle_expr(expression, out.line);
                println!("Declaration_Expr: {:?}", &expr_handle_result);
                value_index = expr_handle_result.index;
                actual_type = expr_handle_result._type;
            },
            None             => {
                empty_expr = true;
                value_index = self.chunk.write_null(out.line)
            },
        };

        if actual_type == Type::Null
        {
            if declared_type.is_any()
            {
                panic!("You cannot initialize Any type with null");
            }

            if !declared_type.is_nullable()
            {
                panic!("an attempt to initialize non-nullable variable with null");
            }
        }
        else
        {
            if declared_type.is_any() 
            {
                declared_type = actual_type
            }
            else if declared_type != actual_type
            {
                panic!("Type mismatch! {:?} != {:?}", declared_type, actual_type);
            }
        }

        // let index = self.chunk.write_const(name);
        if self.current_block > 0
        {
            let is_exist = self.current_define
                            .iter()
                            .position(|&x| x.1 == name);

            if is_exist.is_some() 
            { 
                panic!("Same Variable Declared TWICE.");
            }

            self.current_define.push((declared_type, name));
            let position = self.current_define.len() - 1;

            let opcode = match declared_type
            {
                Type::Int     => OpCode::IStore,
                Type::Float   => OpCode::FStore,
                Type::Boolean => OpCode::BStore,
                Type::Str     => OpCode::SStore,
                _ => panic!("Unsupported opcode for here"),
            };
            let operands = position as u16;

            self.chunk.push_opcode(opcode, out.line);
            out.index = self.chunk.push_operands(operands.to_vm_byte(), out.line);
        }
        else
        {
            self.global_type.insert(name, declared_type);
            let opcode = match declared_type
            {
                Type::Int     => OpCode::GIStore,
                Type::Float   => OpCode::GFStore,
                Type::Boolean => OpCode::GBStore,
                Type::Str     => OpCode::GSStore,
                _ => panic!("Unsupported opcode for here"),
            };
            self.chunk.push_opcode(opcode, out.line);
            out.index = self.chunk.push_operands(name.to_vm_byte(), out.line);
        }
    }

    fn handle_function_data(&mut self, out: &mut StatementHandleResult, data: FunctionData)
    {
        if 0 < self.current_block
        {
            panic!("You currently cannot create closure.");
        }
        let decl_info = data.it;
        let arguments = data.args;
        let body_block = data.block;
        self.global_type.insert(decl_info.name_u16, decl_info._type);
        self.chunk.push_opcode(OpCode::BlockIn, line);
        self.current_block += 1;
        for i in block_data.statements 
        {
            self.handle_stmt(i, line);
        }
        self.current_block -= 1;
        handled_result.index = self.chunk.push_opcode(OpCode::BlockOut, line);
        handled_result
    }

    fn handle_expr(&mut self, expr: Expr, line: usize) -> ExpressionHandleResult
    {
        let mut handled_result = ExpressionHandleResult {
            line: line,
            index: 0,
            _type: Type::Null
        };
        match expr
        {
            Expr::Variable(name) => {
                // let data = self.variable.get(name).unwrap();
                // TODO: handle it without making a clone.
                let is_exist = self.current_define
                                .iter()
                                .position(|&x| x.1 == name);

                if is_exist.is_none()
                {
                    // it could be a global variable;
                    if !self.global_type.contains_key(&name)
                    {
                        panic!("Undefined Variable");
                    }

                    let _type = self.global_type.get(&name).unwrap().clone();
                    let opcode = match _type
                    {
                        Type::Int     => OpCode::GILoad,
                        Type::Float   => OpCode::GFLoad,
                        Type::Str     => OpCode::GSLoad,
                        Type::Boolean => OpCode::GBLoad,
                        _ => unreachable!(),
                    };
                    handled_result._type = _type;
                    self.chunk.push_opcode(opcode, line);
                    handled_result.index = self.chunk.push_operands(name.to_vm_byte(), line);
                    return handled_result;
                }

                let mut index = 0;
                let (_type, _) = {
                    index = is_exist.unwrap();
                    self.current_define.get(index).unwrap().clone()
                };
                let index = index as u16;
                handled_result._type = _type;

                let opcode = match _type
                {
                    Type::Int     => OpCode::ILoad,
                    Type::Float   => OpCode::FLoad,
                    Type::Str     => OpCode::SLoad,
                    Type::Boolean => OpCode::BLoad,
                    _ => unreachable!(),
                };
                self.chunk.push_opcode(opcode, line);
                handled_result.index = self.chunk.push_operands(index.to_vm_byte(), line);
                handled_result
            },
            Expr::Literal(literal) => {
                // TODO: handle it without making a clone.
                // self.stack.push(literal);
                let _type = literal.ctype;
                let index = self.chunk.write_const(literal, line);
                self.chunk._data_type.insert(index, _type.clone());
                handled_result.index = index;
                handled_result._type = _type.clone();
                return handled_result;
            },

            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(*left, line);
                let right = self.handle_expr(*right, line);

                handled_result._type = Type::type_after_binary(&left._type, &right._type, &operator.tokentype).unwrap();
                handled_result.index = match operator.tokentype
                {
                    TokenType::Plus       => self.chunk.push_opcode(OpCode::Add, line),
                    TokenType::Minus      => self.chunk.push_opcode(OpCode::Sub, line),
                    TokenType::Asterisk   => self.chunk.push_opcode(OpCode::Mul, line),
                    TokenType::Slash      => self.chunk.push_opcode(OpCode::Div, line),
                    TokenType::Percent    => self.chunk.push_opcode(OpCode::Mod, line),

                    // PartialEq Series
                    TokenType::NotEqual   => self.chunk.push_opcode(OpCode::NotEq, line),
                    TokenType::EqualEqual => self.chunk.push_opcode(OpCode::EqEq, line),

                    // PartialOrd Series
                    TokenType::LessEqual  => self.chunk.push_opcode(OpCode::LessEq, line),
                    TokenType::MoreEqual  => self.chunk.push_opcode(OpCode::MoreEq, line),
                    TokenType::Less       => self.chunk.push_opcode(OpCode::Less, line),
                    TokenType::More       => self.chunk.push_opcode(OpCode::More, line),
                    _ => unreachable!(),
                };
                handled_result
            },
            Expr::Logical(left, right, operator) =>
            {
                self.handle_expr(*left, line);
                self.handle_expr(*right, line);

                handled_result.index = match operator.tokentype
                {
                    TokenType::And => self.chunk.push_opcode(OpCode::And, line),
                    TokenType::Or => self.chunk.push_opcode(OpCode::Or, line),
                    _ => unreachable!(),
                };
                handled_result._type = Type::Boolean;
                handled_result
            },
            Expr::Unary(expr, operator) =>
            {
                let another_result = self.handle_expr(*expr, line);
                handled_result._type = Type::type_after_unary(&another_result._type, &operator.tokentype).unwrap();
                handled_result.index = match operator.tokentype
                {
                    TokenType::Bang => self.chunk.push_opcode(OpCode::Not, line),
                    TokenType::Minus => self.chunk.push_opcode(OpCode::Neg, line),
                    _ => unreachable!(),
                };
                handled_result._type = another_result._type;
                handled_result
            },
            Expr::Grouping(group) => self.handle_expr(*group, line),
            Expr::Assign(name, expr) =>
            {
                let another_result = self.handle_expr(*expr, line);

                let is_exist = self.current_define
                                .iter()
                                .position(|&x| x.1 == name);

                if is_exist.is_none()
                {
                    if !self.global_type.contains_key(&name)
                    {
                        panic!("Undefined Variable");
                    }

                    let _type = self.global_type.get(&name).unwrap().clone();
                    let actual_type = &another_result._type;
                    if &_type != actual_type 
                    {
                        panic!("Type mismatch! {:?} != {:?}", _type, actual_type);
                    }
                    let opcode = match another_result._type
                    {
                        Type::Boolean => OpCode::GBStore,
                        Type::Int     => OpCode::GIStore,
                        Type::Str     => OpCode::GSStore,
                        Type::Float   => OpCode::GFStore,
                        _ => unreachable!(),
                    };
                    self.chunk.push_opcode(opcode, line);
                    handled_result._type = _type; 
                    handled_result.index = self.chunk.push_operands(name.to_vm_byte(), line);
                }

                let mut index = 0;
                let (declared_type, _) = {
                    index = is_exist.unwrap();
                    self.current_define.get(index).unwrap().clone()
                };

                if &declared_type != &another_result._type
                {
                    panic!("Type Mismatch when Assigning");
                }

                let opcode = match another_result._type
                {
                    Type::Boolean => OpCode::BStore,
                    Type::Int     => OpCode::IStore,
                    Type::Str     => OpCode::SStore,
                    Type::Float   => OpCode::FStore,
                    _ => unreachable!(),
                };
                self.chunk.push_opcode(opcode, line);
                handled_result._type = declared_type.clone();
                handled_result.index = self.chunk.push_operands(index.to_vm_byte(), line);
                handled_result
            },
            Expr::FunctionCall(func_name, open_paren, mut arguments) => {
                if let Expr::Variable(name) = *func_name {
                    let address_section = self.chunk.push_opcode(OpCode::PushPtr, line);
                    self.chunk.push_operands(usize::max_value().to_vm_byte(), line);
                    arguments.reverse();
                    for arg in arguments
                    {
                        self.handle_expr(arg, line);
                    }
                    self.chunk.push_opcode(OpCode::Call, line);
                    let jump_here = self.chunk.push_operands(name.to_vm_byte(), line);
                    self.chunk.push_opcode(OpCode::BlockOut, line);
                    self.chunk.rewrite_operands(jump_here.to_vm_byte(), address_section);

                    handled_result._type = *self.global_type.get(&name).unwrap_or(&Type::Int);
                    handled_result.index = jump_here;
                }
                handled_result
            }
            _ => unreachable!(),
        }
    }
}

