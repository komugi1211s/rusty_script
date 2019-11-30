#[derive(Default, Debug)]
struct ByteCode
{
    data_section: Vec<u8>,
    data_idx: usize,
    text_section: Vec<u8>,
    text_idx: usize,
}

#[derive(Debug)]
enum OpCode
{
    Return,
    Const8,
    Const16,
    Const32,
    Const64,
    ConstPtr,
    
    Push,
    Pop,
    Interrupt,
    Add,
    DebugPrint,
}

impl From<u8> for OpCode
{
    fn from(item: u8) -> Self
    {
        match item
        {
            0x00 => Self::Return,
            0x01 => Self::Const8,
            0x02 => Self::Const16,
            0x04 => Self::Const32,
            0x08 => Self::Const64,
            0x0F => Self::ConstPtr,
            0x20 => Self::Push,
            0x21 => Self::Pop,
            0x10 => Self::Add,
            0xCF => Self::DebugPrint,
            _ => Self::Interrupt,
        }
    }
}

impl From<OpCode> for u8
{
    fn from(item: OpCode) -> u8
    {
        match item
        {
            OpCode::Return    => 0x00,
            OpCode::Const8    => 0x01,
            OpCode::Const16   => 0x02,
            OpCode::Const32   => 0x04,
            OpCode::Const64   => 0x08,
            OpCode::ConstPtr  => 0x0F,
            OpCode::Push      => 0x20,
            OpCode::Pop       => 0x21,
            OpCode::Add       => 0x10,
            OpCode::Interrupt => 0xCC,
            OpCode::DebugPrint=> 0xCF,
        }
    }
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

    fn push_data(&mut self, data: Vec<u8>) -> usize
    {
        let start_at = self.data_idx;
        for i in data.iter()
        {
            self.data_section.push(*i);
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
        let second = self.data_section[index+1];
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

    fn disassemble(&self) {
        println!(" ========== DISASSEMBLED ========== ");
        let mut current: usize = 0;
        let max = self.text_section.len();
        while current < max
        {
            let opcode = self.text_section[current];
            let padding = self.disassemble_pad(OpCode::from(opcode));
            if 1 < padding {
                current += 1;
                let operand = &self.text_section[(current) .. (current + padding)];
                let mut x: [u8; 8] = [0; 8];
                for i in 0..8
                {
                    x[i] = operand[i];
                }

                let operand: usize = usize::from_ne_bytes(x);
                println!(" | 0x{:x} 0x{:08x} - {:?} {}", opcode, operand, OpCode::from(opcode), operand);
                current += padding;
            } else {
                println!(" | {:x} - {:?}", opcode, OpCode::from(opcode));
                current += 1;
            }
        }
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

#[derive(Debug)]
enum Value
{
    int(i64),
    float(f64),
    string(String),
    boolean(bool)
}

use std::ops;
impl ops::Add<Value> for Value
{
    type Output = Value;
    fn add(self, other: Value) -> Value
    {
        match self
        {
            Self::int(i) => match other
            {
                Self::int(i2) => Self::int(i + i2),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Default, Debug)]
struct VirtualMachine
{
    stack: Vec<Value>,
    code: ByteCode
}

impl VirtualMachine
{
    fn run(&mut self)
    {
        let mut current: usize = 0;
        let max: usize = self.code.text_idx;
        while current < max 
        {
            match OpCode::from(self.code.text_section[current])
            {
                OpCode::Add => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();

                    self.stack.push(a + b);
                    current += 1;
                },
                OpCode::Const8 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let value = self.code.data_section[index];
                    self.stack.push(Value::boolean(value != 0));
                    current = new_end;
                },
                OpCode::Const64 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(current);
                    let value = self.code.read_data_64(index);
                    self.stack.push(Value::int(i64::from_ne_bytes(value)));
                    current = new_end;
                }
                OpCode::DebugPrint => {
                    let a = self.stack.pop().unwrap();
                    println!("{:?}", a);
                    current += 1;
                }
                _ => current += 1,
            }
        }
    }

    fn consume_const_index(&mut self, start: usize) -> (/* result */usize, /* new_end */usize)
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



trait toVmByte
{
    fn to_vm_byte(&self) -> Vec<u8>;
    fn sufficient_opcode(&self) -> OpCode;
}
impl toVmByte for i64 
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_ne_bytes().iter().cloned().collect()
    }
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const64
    }
}

impl toVmByte for f64
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_bits().to_ne_bytes().iter().cloned().collect()
    }
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const64
    }
}

impl toVmByte for String
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.clone().into_bytes()
    }
    
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::ConstPtr
    }
}

impl toVmByte for bool
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        vec![*self as u8]
    }
    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Const8
    }
}

impl toVmByte for usize
{
    fn to_vm_byte(&self) -> Vec<u8>
    {
        self.to_ne_bytes().iter().cloned().collect()
    }

    fn sufficient_opcode(&self) -> OpCode
    {
        OpCode::Interrupt
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

pub fn fntest() {
    let mut bcode: ByteCode = ByteCode::default();
    write_add(&mut bcode, 100i64, 15i64);
    bcode.push_opcode(OpCode::DebugPrint);

    bcode.disassemble();

    let mut vm = VirtualMachine {
        stack: Vec::new(),
        code: bcode,
    };

    vm.run();
}

