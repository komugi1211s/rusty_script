
struct ByteCode
{
    data_section: Vec<u8>,
    data_idx: usize,
    text_section: Vec<u8>,
    text_idx: usize,
}

enum OpCode
{
    Return,
    Const8,
    Const16,
    Const32,
    Const64,
    ConstPtr,
    Move,
    Seek,
    
    Push,
    Pop,
    Interrupt,
    Add,
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
            OpCode::Return => 0x00,
            OpCode::Const8 => 0x01,
            OpCode::Const16 => 0x02,
            OpCode::Const32 => 0x04,
            OpCode::Const64 => 0x08,
            OpCode::ConstPtr => 0x0F,
            OpCode::Push => 0x20,
            OpCode::Pop => 0x21,
            OpCode::Add => 0x10,
            OpCode::Interrupt => 0xCC,
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
        let data: [u8; 8] = [0; 8];
        
        for i in 0..8
        {
            data[i] = self.data_section[index+i];
        }
        data
    }
}

struct VirtualMachine
{
    stack: Vec<u8>,
    code: ByteCode
}

impl VirtualMachine
{
    fn run(&mut self)
    {
        loop {
            
        }
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

fn write_const<T>(stack: &mut ByteCode, value: T) -> usize
where 
    T: toVmByte
{
    let opcode = value.sufficient_opcode();
    let value = value.to_vm_byte();
    
    let start_index = stack.push_data(value);
    
    
    cur
}

fn write_add<T>(chunk: &mut ByteCode, a: T, b: T) -> usize
where
    T: toVmByte
{
0
}

