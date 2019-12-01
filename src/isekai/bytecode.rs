
use super::types::{ Value, OpCode, toVmByte };
use super::parse::{ Statement, Expr };
use super::token::{ Token, TokenType };
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct ByteCode
{
    pub data_section: Vec<u8>,
    pub data_idx: usize,
    pub text_section: Vec<u8>,
    pub text_idx: usize,
}

impl ByteCode
{
    fn push_opcode(&mut self, code: OpCode) -> usize
    {
        self.text_section.push(code.into());
        self.text_idx += 1;
        self.text_idx
    }
    
    fn push_operand(&mut self, operand: u8) -> usize
    {
        self.text_section.push(operand);
        self.text_idx += 1;
        self.text_idx
    }

    fn push_operands(&mut self, operands: Vec<u8>) -> usize
    {
        for i in &operands
        {
            self.push_operand(*i);
        }
        self.text_idx
    }

    fn write_const<T>(&mut self, value: T) -> usize
        where
            T: toVmByte
    {
        let opcode = value.sufficient_opcode();
        let value = value.to_vm_byte();

        let start_index = self.push_data(value);

        self.push_opcode(opcode);
        return self.push_operands(start_index.to_vm_byte());
    }

    fn push_data<T>(&mut self, data: T) -> usize
    where
        T: IntoIterator<Item=u8>
    {
        let start_at = self.data_idx;
        for i in data.into_iter()
        {
            self.data_section.push(i);
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
        self.data_section[index]
    }
    
    fn read_data_16(&self, index: usize) -> [u8; 2]
    {
        self.assert_datarange(index);
        let first = self.data_section[index];
        let second = self.data_section[index + 1];
        [first, second]
    }

    fn read_data_32(&self, index: usize) -> [u8; 4]
    {
        self.assert_datarange(index);
        let f = self.data_section[index];
        let s = self.data_section[index+1];
        let t = self.data_section[index+2];
        let fo = self.data_section[index+3];
        [f, s, t, fo]
    }
    
    fn read_data_64(&self, index: usize) -> [u8; 8]
    {
        self.assert_datarange(index);
        let mut data: [u8; 8] = [0; 8];
        
        for i in 0..8
        {
            data[i] = self.data_section[index+i];
        }
        data
    }

    pub fn disassemble_all(&self) 
    {
        self.disassemble(0, self.text_section.len());
    }
    pub fn disassemble(&self, start: usize, max_len: usize)
    {
        println!(" ============ DISASSEMBLED ============ ");
        let mut current = start;
        let max = max_len;
        while current < max
        {
            let opbyte = self.text_section[current];
            let mut opcode = format!("{:?}", OpCode::from(opbyte));
            opcode.make_ascii_uppercase();
            let padding = self.disassemble_pad(OpCode::from(opbyte));
            if 1 < padding {
                current += 1;

                let operand = self.get_operand(current, padding);
                let operand: usize = usize::from_ne_bytes(operand);
                println!(" | {:04} | {:04X} {:04X} - {} {}", current, opbyte, operand, opcode, operand);

                current += padding;
            } else {

                println!(" | {:04} | {:04X} - {}", current, opbyte, opcode);
                current += 1;
            }
        }

        println!(" ========== DISASSEMBLE DONE ========== ");
        println!();
        println!(" ============= BYTE CODES ============= ");
        println!();
        let bytecode_hex: Vec<String> = (&self.text_section[start .. max-1]).iter()
                                        .map(|x| format!("{:02x}", x)).collect();
        println!("{}", bytecode_hex.join(" "));
        println!();
        println!(" =========== BYTE CODES DONE ========== ");
    }

    fn get_operand(&self, current: usize, padding: usize) -> [u8; 8]
    {
        let operand = &self.text_section[(current) .. (current + padding)];
        let mut x: [u8; 8] = [0; 8];
        for i in 0..8
        {
            x[i] = operand[i];
        }

        x
    }

    fn disassemble_pad(&self, opcode: OpCode) -> usize
    {
        match opcode
        {
            OpCode::Const8 => 8,
            OpCode::Const16 => 8,
            OpCode::Const32 => 8,
            OpCode::Const64 => 8,
            _ => 0
        }
    }
}


#[derive(Default, Debug)]
pub struct VirtualMachine
{
    pub stack: Vec<Value>,
    pub code: ByteCode,
    pub variable: HashMap<String, Value>,
}

impl VirtualMachine
{
    pub fn new() -> Self
    {
        Self {
            stack: Vec::new(),
            code: ByteCode::default(),
            variable: HashMap::new(),
        }
    }

    pub fn traverse_ast(mut self, ast: Vec<Statement>) -> Result<Self, ()>
    {
        for i in ast {
            self.handle_stmt(i);
        }
        Ok(self)
    }

    fn handle_stmt(&mut self, stmt: Statement)
    {
        match stmt {
            Statement::Expression(expr) => self.handle_expr(expr),
            Statement::Decralation(name, var_type, value_expr) =>
            {
                unimplemented!();
            },
            Statement::Print(expr) => {
                self.handle_expr(expr);
                self.code.push_opcode(OpCode::DebugPrint);
            },
            Statement::If(expr, if_block, else_block) => {
                unimplemented!();
            },

            Statement::While(expr, while_block) => {
                unimplemented!();
            },

            Statement::Break => {
                unimplemented!();
            },
            Statement::Continue => {
                unimplemented!();
            },
            _ => unreachable!(),
        };
    }

    fn handle_expr(&mut self, expr: Expr)
    {
        match expr
        {
            Expr::Variable(name) => {
                // let data = self.variable.get(name).unwrap();
                // TODO: handle it without making a clone.
                // self.stack.push(data);
                unimplemented!();
            },
            Expr::Literal(literal) => {
                // TODO: handle it without making a clone.
                // self.stack.push(literal);
                match literal
                {
                    Value::Boolean(boolean) => {
                        self.code.write_const(boolean);
                    },
                    Value::Int(int) => {
                        self.code.write_const(int);
                    },
                    Value::Float(float) => {
                        self.code.write_const(float);
                    },
                    Value::Str(string) => {
                        unimplemented!();
                    },
                    Value::Null => {
                        unimplemented!();
                    },
                    _=> unreachable!(),
                };
            },
            Expr::Binary(left, right, operator) => {
                let left = self.handle_expr(*left);
                let right = self.handle_expr(*right);

                match operator.tokentype
                {
                    TokenType::Plus      => self.code.push_opcode(OpCode::Add),
                    TokenType::Minus     => self.code.push_opcode(OpCode::Sub),
                    TokenType::Asterisk  => self.code.push_opcode(OpCode::Mul),
                    TokenType::Slash     => self.code.push_opcode(OpCode::Div),
                    TokenType::Percent   => self.code.push_opcode(OpCode::Mod),

                    // PartialEq Series
                    TokenType::NotEqual  => self.code.push_opcode(OpCode::NotEq),
                    TokenType::EqualEqual=> self.code.push_opcode(OpCode::EqEq),

                    // PartialOrd Series
                    TokenType::LessEqual => self.code.push_opcode(OpCode::LessEq),
                    TokenType::MoreEqual => self.code.push_opcode(OpCode::MoreEq),
                    TokenType::Less      => self.code.push_opcode(OpCode::Less),
                    TokenType::More      => self.code.push_opcode(OpCode::More),
                    _ => unreachable!(),
                };
            },
            Expr::Logical(left, right, operator) =>
            {
                self.handle_expr(*left);
                self.handle_expr(*right);

                match operator.tokentype
                {
                    TokenType::And => self.code.push_opcode(OpCode::And),
                    TokenType::Or => self.code.push_opcode(OpCode::Or),
                    _ => unreachable!(),
                };
            },
            Expr::Unary(expr, operator) =>
            {
                self.handle_expr(*expr);
                match operator.tokentype
                {
                    TokenType::Bang => self.code.push_opcode(OpCode::Not),
                    TokenType::Minus => self.code.push_opcode(OpCode::Neg),
                    _ => unreachable!(),
                };
            },
            Expr::Grouping(group) => self.handle_expr(*group),
            Expr::Assign(name, expr) =>
            {
                self.handle_expr(*expr);
            },
            _ => unreachable!(),
        };
    }

    pub fn run(&mut self)
    {
        let mut current: usize = 0;
        let max: usize = self.code.text_idx;
        while current < max 
        {
            let current_operation = OpCode::from(self.code.text_section[current]);

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
                    let value = self.code.data_section[index];
                    self.stack.push(Value::Boolean(value != 0));
                    current = new_end;
                },
                OpCode::Const64 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let value = self.code.read_data_64(index);
                    self.stack.push(Value::Int(i64::from_ne_bytes(value)));
                    current = new_end;
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
                    let max_len = if self.code.text_section.len() < current + 5 {
                        self.code.text_section.len()
                    }
                    else {
                        current + 5
                    };
                    self.code.disassemble(current - 5, max_len);
                    panic!();
                },
                _ => current += 1,
            }
        }
    }

    pub fn consume_const_index(&mut self, start: usize) -> (/* result */usize, /* new_end */usize)
    {
        let start = start + 1;
        let mut new_end = start;
        let mut result: [u8; 8] = [0; 8];
        for i in 0..8 
        {
            result[i] = self.code.text_section[start + i];
            new_end += 1;
        }

        let result: usize = usize::from_ne_bytes(result);
        (result, new_end)
    }
}


fn write_const<T>(stack: &mut ByteCode, value: T) -> usize
where 
    T: toVmByte
{
    let opcode = value.sufficient_opcode();
    let value = value.to_vm_byte();
    
    let start_index = stack.push_data(value);

    stack.push_opcode(opcode);
    return stack.push_operands(start_index.to_vm_byte());
}


fn write_add<T>(chunk: &mut ByteCode, a: T, b: T) -> usize
where 
    T: toVmByte
{
    write_const(chunk, a);
    write_const(chunk, b);
    chunk.push_opcode(OpCode::Add)
}

