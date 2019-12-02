
use super::types::{ Type, Value, OpCode, toVmByte };
use super::parse::{ Statement, Expr, ParsedData };
use super::token::{ Token, TokenType };
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
    fn push_opcode(&mut self, code: OpCode, line: usize) -> usize
    {
        self.code_chunk.push(code as u8);
        self.code_line.push(line);
        self.code_idx += 1;
        self.code_idx
    }
    
    fn push_operand(&mut self, operand: u8, line: usize) -> usize
    {
        self.code_chunk.push(operand);
        self.code_line.push(line);
        self.code_idx += 1;
        self.code_idx
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

    pub fn disassemble_all(&self) 
    {
        self.disassemble(0, self.code_chunk.len());
    }

    pub fn disassemble(&self, start: usize, max_len: usize)
    {
        println!(" ============ DISASSEMBLED ============ ");
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
                let formatted = format!(" - \x1b[1m{:<12}\x1b[0m {}", opcode, operand);
                let in_byte = format!("\x1b[1m{:02X}\x1b[0m {}", opbyte, operand);
                println!(" | {:04} | {:<36} {}", current, in_byte, formatted); 
                current += padding;
            } else {
                let in_byte = format!("\x1b[1m{:02X}\x1b[0m", opbyte);
                let formatted = format!(" - \x1b[1m{:<12}\x1b[0m", opcode);
                println!(" | {:04} | {:<36} {}", current, in_byte, formatted);
                current += 1;
            }
        }

        println!(" ========== DISASSEMBLE DONE ========== ");
        println!();
        println!(" ============= BYTE CODES ============= ");
        println!();
        let bytecode_hex: Vec<String> = (&self.code_chunk[start .. max]).iter()
                                        .map(|x| format!("{:02x}", x)).collect();
        println!("{}", bytecode_hex.join(" "));
        println!();
        println!(" =========== BYTE CODES DONE ========== ");
        println!();
        println!(" =============  BYTE DATA ============= ");
        println!();
        let bytedata_hex: Vec<String> = self.data_chunk.iter()
                                        .map(|x| format!("{:02x}", x)).collect();
        println!("{}", bytedata_hex.join(" "));
        println!();
        println!(" ============ BYTE DATA DONE ========== ");
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
            OpCode::Define => 3,
            OpCode::Read => 2,
            OpCode::Write => 2,
            _ => 0
        }
    }
}


#[derive(Debug)]
pub struct VirtualMachine
{
    pub stack: Vec<Value>,
    pub code: ByteChunk,
    pub variable: HashMap<u16, (Type, Value)>,
    pub last_op: OpCode,
}

impl VirtualMachine
{
    pub fn new() -> Self
    {
        Self {
            stack: Vec::new(),
            code: ByteChunk::default(),
            variable: HashMap::new(),
            last_op: OpCode::Interrupt,
        }
    }

    pub fn traverse_ast(mut self, ast: Vec<ParsedData>) -> Result<Self, ()>
    {
        for i in ast {
            self.handle_stmt(i);
        }
        Ok(self)
    }

    fn handle_stmt(&mut self, data: ParsedData) -> usize
    {
        let line = data.line;
        match data.value {
            Statement::Expression(expr) => self.handle_expr(expr, line),
            Statement::Decralation(name, declared_type, value_expr) =>
            {
                let value_index = self.handle_expr(value_expr, line);
                // let index = self.code.write_const(name);
                self.code.push_opcode(OpCode::Define, line);

                let name_vector = name.to_vm_byte();
                let operands: Vec<u8> = vec![declared_type.bits(), name_vector[0], name_vector[1]];
                self.code.push_operands(operands, line)
            },

            Statement::Print(expr) => {
                self.handle_expr(expr, line);
                self.code.push_opcode(OpCode::DebugPrint, line)
            },
            Statement::If(expr, if_block, else_block) => {
                unimplemented!();
                0
            },

            Statement::While(expr, while_block) => {
                unimplemented!();
                0
            },

            Statement::Break => {
                unimplemented!();
                0
            },
            Statement::Continue => {
                unimplemented!();
                0
            },
            _ => unreachable!(),
        }
    }

    fn handle_expr(&mut self, expr: Expr, line: usize) -> usize
    {
        match expr
        {
            Expr::Variable(name) => {
                // let data = self.variable.get(name).unwrap();
                // TODO: handle it without making a clone.
                self.code.push_opcode(OpCode::Read, line);
                self.code.push_operands(name.to_vm_byte(), line)
            },
            Expr::Literal(literal) => {
                // TODO: handle it without making a clone.
                // self.stack.push(literal);
                match literal
                {
                    Value::Boolean(boolean) => {
                        let index = self.code.write_const(boolean, line);
                        self.code._data_type.insert(index, Type::Boolean);
                        index
                    },
                    Value::Int(int) => {
                        let index = self.code.write_const(int, line);
                         self.code._data_type.insert(index, Type::Int);
                        index
                    },
                    Value::Float(float) => {
                        let index = self.code.write_const(float, line);
                        self.code._data_type.insert(index, Type::Float);
                        index
                    },
                    Value::Str(string) => {
                        let index = self.code.write_const(string, line);
                        self.code._data_type.insert(index, Type::Str);
                        index
                    },
                    Value::Null => {
                        let index = self.code.write_null(line);
                        index
                    },
                    _=> unreachable!(),
                }
            },
            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(*left, line);
                let right = self.handle_expr(*right, line);

                match operator.tokentype
                {
                    TokenType::Plus       => self.code.push_opcode(OpCode::Add, line),
                    TokenType::Minus      => self.code.push_opcode(OpCode::Sub, line),
                    TokenType::Asterisk   => self.code.push_opcode(OpCode::Mul, line),
                    TokenType::Slash      => self.code.push_opcode(OpCode::Div, line),
                    TokenType::Percent    => self.code.push_opcode(OpCode::Mod, line),

                    // PartialEq Series
                    TokenType::NotEqual   => self.code.push_opcode(OpCode::NotEq, line),
                    TokenType::EqualEqual => self.code.push_opcode(OpCode::EqEq, line),

                    // PartialOrd Series
                    TokenType::LessEqual  => self.code.push_opcode(OpCode::LessEq, line),
                    TokenType::MoreEqual  => self.code.push_opcode(OpCode::MoreEq, line),
                    TokenType::Less       => self.code.push_opcode(OpCode::Less, line),
                    TokenType::More       => self.code.push_opcode(OpCode::More, line),
                    _ => unreachable!(),
                }
            },
            Expr::Logical(left, right, operator) =>
            {
                self.handle_expr(*left, line);
                self.handle_expr(*right, line);

                match operator.tokentype
                {
                    TokenType::And => self.code.push_opcode(OpCode::And, line),
                    TokenType::Or => self.code.push_opcode(OpCode::Or, line),
                    _ => unreachable!(),
                }
            },
            Expr::Unary(expr, operator) =>
            {
                self.handle_expr(*expr, line);
                match operator.tokentype
                {
                    TokenType::Bang => self.code.push_opcode(OpCode::Not, line),
                    TokenType::Minus => self.code.push_opcode(OpCode::Neg, line),
                    _ => unreachable!(),
                }
            },
            Expr::Grouping(group) => self.handle_expr(*group, line),
            Expr::Assign(name, expr) =>
            {
                self.handle_expr(*expr, line);
                self.code.push_opcode(OpCode::Write, line);
                self.code.push_operands(name.to_vm_byte(), line)
            },
            _ => unreachable!(),
        }
    }

    pub fn run(&mut self)
    {
        let mut current: usize = 0;
        let max: usize = self.code.code_idx;
        while current < max 
        {
            let current_operation = OpCode::from_u8(self.code.code_chunk[current]).unwrap();
            let current_line = self.code.code_line[current];
            println!("OpCode: {:?}", current_operation);
            println!("Current Stack: {:?}", self.stack);

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
                OpCode::Define => {
                    // println!("Current: {:?}", self.stack);
                    let value = self.stack.pop().unwrap();
                    let type_operand = self.code.code_chunk[current + 1];
                    let actual_type = Type::from_bits(type_operand).unwrap();

                    if !actual_type.is_compatible(&value) {
                        panic!("Type Mismatch!: {:?} to {:?}", actual_type, value.to_type());
                    }
                    if value == Value::Null && !actual_type.is_nullable() {
                        panic!("Tried to assign null to non-nullable variable: line {}", current_line);
                    }

                    let u16_one = self.code.code_chunk[current + 2];
                    let u16_two = self.code.code_chunk[current + 3];

                    let identifier = u16::from_ne_bytes([u16_one, u16_two]);
                    self.variable.insert(identifier, (actual_type, value));
                    current += 4;
                },
                OpCode::Write => {
                    let new_value = self.stack.pop().unwrap();
                    let operand_one = self.code.code_chunk[current + 1];
                    let operand_two = self.code.code_chunk[current + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);
                    if !self.variable.contains_key(&identifier)
                    {
                        panic!("Write of undeclared variable: Line {}", current_line);
                    }

                    let (current_type, current_value) = self.variable.get(&identifier).unwrap();
                    if !current_type.is_compatible(&new_value)
                    {
                        panic!("Type Mismatch when Writing: Line {}", current_line);
                    }

                    self.variable.insert(identifier, (current_type.clone(), new_value.clone()));
                    self.stack.push(new_value);
                    current += 3;
                },
                OpCode::Read => {
                    let operand_one = self.code.code_chunk[current + 1];
                    let operand_two = self.code.code_chunk[current + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);
                    if !self.variable.contains_key(&identifier)
                    {
                        panic!("Read of undeclared variable: Line {}", current_line);
                    }
                    let (_, value) = self.variable.get(&identifier).unwrap();
                    if value.to_type() == Type::Null
                    {
                        panic!("Use of uninitialized variable: Line {}", current_line);
                    }
                    self.stack.push(value.clone());
                    current += 3;
                },
                OpCode::DebugPrint => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a);
                    current += 1;
                },
                OpCode::Interrupt => {
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
                    self.code.disassemble(current - 5, max_len);
                    panic!();
                },
                _ => current += 1,
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

