
use super::types::{ Type, Value, OpCode, toVmByte };
use super::parse::{ Statement, Expr, ParserNode, DeclarationData, BlockData };
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

    fn push_operand(&mut self, operand: u8, line: usize) -> usize
    {
        self.code_chunk.push(operand);
        self.code_line.push(line);
        self.code_idx += 1;
        self.code_idx
    }

    fn push_opcode(&mut self, code: OpCode, line: usize) -> usize
    {
        self.push_operand(code as u8, line)
    }

    fn rewrite_operand(&mut self, byte: u8, index: usize) -> usize
    {
        if self.code_idx < index
        {
            panic!("Rewrite opcode out of range");
        }

        self.code_chunk[index] = byte;
        self.code_idx
    }

    fn rewrite_operands(&mut self, bytes: Vec<u8>, index: usize) -> usize
    {
        for i in 0..bytes.len()
        {
            self.rewrite_operand(bytes[i], index + i);
        }
        self.code_idx
    }

    fn rewrite_opcode(&mut self, code: OpCode, index: usize) -> usize
    {
        self.rewrite_operand(code as u8, index)
    }

    fn push_operands(&mut self, operands: Vec<u8>, line: usize) -> usize
    {
        for i in &operands
        {
            self.push_operand(*i, line);
        }
        self.code_idx
    }

    fn write_const<T>(&mut self, value: T, line: usize) -> usize
        where
            T: toVmByte
    {
        let opcode = value.sufficient_opcode();
        let value = value.to_vm_byte();
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

    fn write_null(&mut self, line: usize) -> usize
    {
        self.push_opcode(OpCode::ConstPtr, line);
        self.push_operands(0usize.to_vm_byte(), line);
        self.data_idx
    }


    fn push_data<T>(&mut self, data: T) -> usize
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

    fn assert_datarange(&self, index: usize)
    {
        if self.data_idx <= index
        {
            panic!("Index Out of Range");
        }
    }

    fn read_data_8(&self, index: usize) -> u8
    {
        self.assert_datarange(index);
        self.data_chunk[index]
    }
    
    fn read_data_16(&self, index: usize) -> [u8; 2]
    {
        self.assert_datarange(index);
        let first = self.data_chunk[index];
        let second = self.data_chunk[index + 1];
        [first, second]
    }

    fn read_data_32(&self, index: usize) -> [u8; 4]
    {
        self.assert_datarange(index);
        let f = self.data_chunk[index];
        let s = self.data_chunk[index+1];
        let t = self.data_chunk[index+2];
        let fo = self.data_chunk[index+3];
        [f, s, t, fo]
    }
    
    fn read_data_64(&self, index: usize) -> [u8; 8]
    {
        self.assert_datarange(index);
        let mut data: [u8; 8] = [0; 8];
        
        for i in 0..8
        {
            data[i] = self.data_chunk[index+i];
        }
        data
    }

    fn read_data_dyn(&self, index: usize) -> Vec<u8>
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
            OpCode::ConstPtr |
            OpCode::ConstDyn => USIZE_LENGTH,

            OpCode::JumpIfFalse | OpCode::Jump => USIZE_LENGTH,

            OpCode::GlobalDefine => 3,
            OpCode::GlobalLoad   => 2,
            OpCode::GlobalStore  => 2,

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
                let mut value_index = 0;
                let mut actual_type = Type::Null;

                let name = declaration_info.name_u16;
                let mut declared_type = declaration_info._type;
                let mut empty_expr = false;
                println!("Declaration: {:?}", &declaration_info);

                match declaration_info.expr {
                    Some(expression) => { 
                        let expr_handle_result = self.handle_expr(expression, line);
                        println!("Declaration_Expr: {:?}", &expr_handle_result);
                        value_index = expr_handle_result.index;
                        actual_type = expr_handle_result._type;
                    },
                    None             => {
                        empty_expr = true;
                        value_index = self.chunk.write_null(line)
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

                let is_exist = self.current_define
                                .iter()
                                .position(|&x| x.1 == name);

                if is_exist.is_some() 
                { 
                    panic!("Same Variable Declared TWICE.");
                }

                self.current_define.push((declared_type, name));
                let position = self.current_define.len() - 1;
                // let index = self.chunk.write_const(name);
                if self.current_block > 0
                {
                    let opcode = match declared_type
                    {
                        Type::Int     => OpCode::IStore,
                        Type::Float   => OpCode::FStore,
                        Type::Boolean => OpCode::BStore,
                        Type::Str     => OpCode::SStore,
                        _ => panic!("Unsupported opcode for here"),
                    };
                    let operands = position as u16;

                    self.chunk.push_opcode(opcode, line);
                    handled_result.index = self.chunk.push_operands(operands.to_vm_byte(), line);
                }
                else
                {
                    self.chunk.push_opcode(OpCode::GlobalDefine, line);
                    let name_vector = declaration_info.name_u16.to_vm_byte();
                    let operands: Vec<u8> = vec![declared_type.bits(), name_vector[0], name_vector[1]];
                    handled_result.index = self.chunk.push_operands(operands, line);
                }
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
                let previous_vec = mem::replace(&mut self.current_define, Vec::new());
                self.current_block += 1;
                for i in block_data.statements 
                {
                    self.handle_stmt(i, line);
                }
                self.current_block -= 1;
                let recent_vec = mem::replace(&mut self.current_define, previous_vec);
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
            _ => unreachable!(),
        }
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
                if 0 < self.current_block
                {
                    let is_exist = self.current_define
                                    .iter()
                                    .position(|&x| x.1 == name);

                    if is_exist.is_none()
                    {
                        // panic!("Undefined Variable");
                        // it could be a global variable;
                        self.chunk.push_opcode(OpCode::GlobalLoad, line);
                        handled_result.index = self.chunk.push_operands(name.to_vm_byte(), line);
                        return handled_result;
                    }

                    let mut index = 0;
                    let (_type, _) = {
                        index = is_exist.unwrap();
                        self.current_define.get(index).unwrap()
                    };
                    let index = index as u16;

                    let opcode = match _type
                    {
                        &Type::Int     => OpCode::ILoad,
                        &Type::Float   => OpCode::FLoad,
                        &Type::Str     => OpCode::SLoad,
                        &Type::Boolean => OpCode::BLoad,
                        _ => unreachable!(),
                    };
                    self.chunk.push_opcode(opcode, line);
                    handled_result.index = self.chunk.push_operands(index.to_vm_byte(), line);
                }
                else
                {
                    self.chunk.push_opcode(OpCode::GlobalLoad, line);
                    handled_result.index = self.chunk.push_operands(name.to_vm_byte(), line);
                }
                handled_result
            },
            Expr::Literal(literal) => {
                // TODO: handle it without making a clone.
                // self.stack.push(literal);
                match literal
                {
                    Value::Boolean(boolean) => {
                        let index = self.chunk.write_const(boolean, line);
                        self.chunk._data_type.insert(index, Type::Boolean);
                        handled_result.index = index;
                        handled_result._type = Type::Boolean;
                    },
                    Value::Int(int) => {
                        let index = self.chunk.write_const(int, line);
                        self.chunk._data_type.insert(index, Type::Int);
                        handled_result.index = index;
                        handled_result._type = Type::Int;
                    },
                    Value::Float(float) => {
                        let index = self.chunk.write_const(float, line);
                        self.chunk._data_type.insert(index, Type::Float);
                        handled_result.index = index;
                        handled_result._type = Type::Float;
                    },
                    Value::Str(string) => {
                        let index = self.chunk.write_const(string, line);
                        self.chunk._data_type.insert(index, Type::Str);
                        handled_result.index = index;
                        handled_result._type = Type::Str;
                    },
                    Value::Null => {
                        let index = self.chunk.write_null(line);
                        handled_result.index = index;
                        handled_result._type = Type::Null;
                    },
                    _=> unreachable!(),
                };
                handled_result
            },
            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(*left, line);
                let right = self.handle_expr(*right, line);

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
                if 0 < self.current_block 
                {
                    let is_exist = self.current_define
                                    .iter()
                                    .position(|&x| x.1 == name);

                    if is_exist.is_none()
                    {
                        panic!("Undefined Variable");
                    }

                    let mut index = 0;
                    let (declared_type, _) = {
                        index = is_exist.unwrap();
                        self.current_define.get(index).unwrap()
                    };

                    if declared_type != &another_result._type
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
                    handled_result._type = *declared_type; 
                    handled_result.index = self.chunk.push_operand(index as u8, line);
                }
                else
                {
                    self.chunk.push_opcode(OpCode::GlobalStore, line);
                    handled_result.index = self.chunk.push_operands(name.to_vm_byte(), line);
                }
                handled_result
            },
            _ => unreachable!(),
        }
    }
}

type EnvHashMap = HashMap<u16, (Type, Value)>;
#[derive(Debug)]
pub struct Environment 
{
    pub values: Vec<EnvHashMap>,
    pub current: usize,
}

impl Environment 
{
    pub fn new() -> Self
    {
        Self {
            values: vec![HashMap::new()],
            current: 0,
        }
    }

    pub fn connect(&mut self, env: Environment) -> usize
    {
        for i in env.values
        {
            self.values.push(i);
            self.current += 1;
        }
        self.current
    }

    // pub fn define_function(&mut self, func: Value)
    // {
    //     if Value::Callable(x) = &func
    //     {
    //         if !x.retType.is_compatible(
    //     }
    // }
    //
    pub fn enter_block(&mut self) -> usize
    {
        self.values.push(HashMap::new());
        self.current += 1;
        self.current
    }

    pub fn leave_block(&mut self) -> (usize, EnvHashMap)
    {
        self.current -= 1;
        (self.current, self.values.pop().unwrap())
    }

    pub fn is_toplevel(&self) -> bool
    {
        self.current == 0
    }

    pub fn define(&mut self, name: u16, _type: Type, value: Value)
    {
        if !_type.is_compatible(&value)
        {
            unreachable!("Mismatched Type and Value: {:?}: {} = {:?}", _type, name, value.to_type());
        }
        let exist_key = self.exist(name);
        if let Some(key) = exist_key 
        {
            if key == self.current
            {
                let declared = self.values[key].get(&name).unwrap();
                if declared.0.is_compatible(&value)
                {
                    unreachable!("Variable Declared Twice - Use an assignment instead.");
                }
            }
        }
        self.values[self.current].insert(name, (_type, value));
    }

    pub fn assign(&mut self, name: u16, new_value: Value)
    {
        let exist_key = self.exist(name);
        if exist_key.is_none() {
            panic!("Undefined Variable {}", name);
        }
        let exist_key = exist_key.unwrap();

        let (c_type, v) = self.values[exist_key].get(&name).unwrap();
        let c_type = c_type.clone();
        if !c_type.is_compatible(&new_value)
        {
            unreachable!("Mismatched Type and Value: {:?}: {} = {:?}", c_type, name, new_value.to_type());
        }
            
        self.values[exist_key].insert(name, (c_type, new_value));
    }

    pub fn assign_global(&mut self, name: u16, new_value: Value)
    {
        if !self.values[0].contains_key(&name)
        {
            panic!("Undefined Variable {}", name);
        }

        let (c_type, value) = self.values[0].get(&name).unwrap();
        let c_type = c_type.clone();
        if !c_type.is_compatible(&new_value)
        {
            unreachable!("Mismatched Type and Value: {:?}: {} = {:?}", c_type, name, new_value.to_type());
        }
            
        self.values[0].insert(name, (c_type, new_value));
    }

    pub fn exist(&self, name: u16) -> Option<usize>
    {
        let mut counter = self.current;
        while !self.values[counter].contains_key(&name)
        {
            if counter <= 0
            {
                return None;
            }
            counter -= 1;
        }
        Some(counter)
    }

    pub fn get(&self, name: u16) -> &Value
    {
        let exist_key = self.exist(name);
        if exist_key.is_none() {
            panic!("Undefined Variable {}", name);
        }

        let (_, x) = self.values[exist_key.unwrap()].get(&name).unwrap();
        if x.is_same_type(&Value::Null)
        {
            unreachable!("Use of an uninitialized variable: {}", name);
        }
        x
    }

    pub fn get_global(&self, name: u16) -> &Value
    {
        if !self.values[0].contains_key(&name)
        {
            panic!("Undefined Variable {}", name);
        }

        let (_, x) = self.values[0].get(&name).unwrap();
        if x.is_same_type(&Value::Null)
        {
            unreachable!("Use of an uninitialized variable: {}", name);
        }
        x
    }
}

#[derive(Debug)]
pub struct VirtualMachine
{
    pub stack: Vec<Value>,
    pub stack_pointer: usize,
    pub code: ByteChunk,
    pub variable: Environment,
    pub last_op: OpCode,
}

impl VirtualMachine
{
    pub fn new(code: ByteChunk) -> Self
    {
        Self {
            stack: Vec::new(),
            stack_pointer: 0,
            code,
            variable: Environment::new(),
            last_op: OpCode::Interrupt,
        }
    }

    pub fn run(&mut self)
    {
        // Meta
        let mut stack_pointer_stack: Vec<usize> = Vec::new();
        let mut current: usize = 0;
        let max: usize = self.code.code_idx;
        while current < max 
        {
            let current_operation = OpCode::from_u8(self.code.code_chunk[current]).unwrap();
            let current_line = self.code.code_line[current];
            println!("OpCode: {:?}", current_operation);
            println!("Current Stack: {:?}", self.stack);
            println!("Current Environment: {:?}", self.variable);

            match current_operation 
            {
                OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div  => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::Add => a + b,
                        OpCode::Sub => a - b,
                        OpCode::Mul => a * b,
                        OpCode::Div => a / b,
                        _ => unreachable!(),
                    });
                    current += 1;
                },
                OpCode::Not | OpCode::Neg => {
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::Not => (!a).is_truthy().into(),
                        OpCode::Neg => -a,
                        _ => unreachable!(),
                    });
                    current += 1;
                },

                OpCode::EqEq | OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::EqEq => (a == b).into(),
                        OpCode::NotEq => (a != b).into(),
                        _ => unreachable!(),
                    });
                    current += 1;
                },

                OpCode::LessEq | OpCode::MoreEq | OpCode::Less | OpCode::More => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::LessEq => (a <= b).into(),
                        OpCode::MoreEq => (a >= b).into(),
                        OpCode::Less   => (a < b).into(),
                        OpCode::More   => (a > b).into(),
                        _ => unreachable!(),
                    });
                    current += 1;
                },
                OpCode::EqEq | OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::EqEq => (a >= b).into(),
                        OpCode::NotEq => (a <= b).into(),
                        _ => unreachable!(),
                    });
                    current += 1;
                },
                OpCode::Const8 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let value = self.code.read_data_8(index);
                    self.stack.push(Value::Boolean(value != 0));
                    current = new_end;
                },
                OpCode::ConstPtr => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    if index == 0 // Null Pointer
                    {
                        self.stack.push(Value::Null);
                    }
                    current = new_end;
                },
                OpCode::Const64 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let value = self.code.read_data_64(index);
                    // GET_TYPE
                    self.stack.push(match self.code._data_type.get(&index).unwrap()
                    {
                        &Type::Int => Value::Int(i64::from_ne_bytes(value)),
                        &Type::Float => Value::Float(f64::from_bits(u64::from_ne_bytes(value))),
                        _ => unreachable!(),
                    });
                    current = new_end;
                },
                OpCode::ConstDyn => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let value = self.code.read_data_dyn(index);

                    self.stack.push(match self.code._data_type.get(&index).unwrap() {
                        &Type::Str => Value::Str(String::from_utf8(value).unwrap()),
                        _ => unreachable!(),
                    });
                    current = new_end;
                },
                OpCode::JumpIfFalse => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let test_value = self.stack.pop().unwrap();

                    current = if test_value.is_truthy() { new_end } else { index };
                },

                OpCode::BStore | OpCode::IStore | OpCode::FStore | OpCode::SStore => {
                    current += 1;
                    let index = self.stack_pointer + self.code.code_chunk[current] as usize;
                    let value = self.stack.last().unwrap().clone();
                    self.stack[index] = value;
                    current += 1;
                },
                OpCode::BLoad | OpCode::ILoad | OpCode::FLoad | OpCode::SLoad  => {
                    current += 1;
                    let index = self.stack_pointer + self.code.code_chunk[current] as usize;
                    let data = self.stack[index].clone();
                    self.stack.push(data);
                    current += 1;
                },
                OpCode::Jump => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    current = index;
                },
                OpCode::BlockIn  => {
                    stack_pointer_stack.push(self.stack_pointer);
                    self.stack_pointer = self.stack.len();
                    current += 1;
                },
                OpCode::BlockOut => {
                    self.stack.truncate(self.stack_pointer);
                    let index = stack_pointer_stack.pop().unwrap();
                    self.stack_pointer = index;
                    current += 1;
                },
                OpCode::GlobalDefine => {
                    // println!("Current: {:?}", self.stack);
                    let value = self.stack.pop().unwrap();

                    let type_operand = self.code.code_chunk[current + 1];
                    let actual_type = Type::from_bits(type_operand).unwrap();

                    let u16_one = self.code.code_chunk[current + 2];
                    let u16_two = self.code.code_chunk[current + 3];
                    let identifier = u16::from_ne_bytes([u16_one, u16_two]);
                    self.variable.define(identifier, actual_type, value);
                    current += 4;
                },
                OpCode::GlobalLoad => {
                    let operand_one = self.code.code_chunk[current + 1];
                    let operand_two = self.code.code_chunk[current + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);

                    let value = self.variable.get_global(identifier);
                    self.stack.push(value.clone());
                    current += 3;
                },
                OpCode::GlobalStore => {
                    let operand_one = self.code.code_chunk[current + 1];
                    let operand_two = self.code.code_chunk[current + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);

                    let value = self.variable.get(identifier);
                    self.stack.push(value.clone());
                    current += 3;
                },
                OpCode::DebugPrint => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a);
                    current += 1;
                },
                _ => {
                    println!("!!!!!!!!!!!!! PANIC !!!!!!!!!!!!!!!");
                    println!("OpCode Interrupt Detected at index {}", current);
                    println!("Current Stack Data: {:?}", self.stack);
                    println!("Current Disassemble here");
                    let max_len = if self.code.code_chunk.len() < current + 5 {
                        self.code.code_chunk.len()
                    }
                    else {
                        current + 5
                    };
                    for message in self.code.disassemble(current - 5, max_len)
                    {
                        println!("{}", message);
                    }
                    panic!();
                },
            }
            self.last_op = current_operation;
        }
    }

    pub fn consume_const_index(&mut self, start: usize) -> (/* result */usize, /* new_end */usize)
    {
        let start = start + 1;
        let mut new_end = start;
        let mut result: [u8; USIZE_LENGTH] = [0; USIZE_LENGTH];
        for i in 0..USIZE_LENGTH
        {
            result[i] = self.code.code_chunk[start + i];
            new_end += 1;
        }

        let result: usize = usize::from_ne_bytes(result);
        (result, new_end)
    }
}

