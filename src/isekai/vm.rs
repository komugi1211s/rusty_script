
use super::bytecode::{ ByteChunk };
use super::types::{ Value, Type, OpCode };
use num_traits::FromPrimitive;
use std::collections::HashMap;

type GlobalMap = HashMap<u16, Value>;

#[cfg(target_pointer_width="32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width="64")]
const USIZE_LENGTH: usize = 8;

#[derive(Debug)]
pub struct VirtualMachine
{
    pub stack: Vec<Value>,
    pub code_ip: usize,
    pub stack_pointer: usize,
    pub code: ByteChunk,
    pub globals: GlobalMap,
    pub last_op: OpCode,
}

impl VirtualMachine
{
    pub fn new(code: ByteChunk) -> Self
    {
        Self {
            stack: Vec::new(),
            code_ip: 0, 
            stack_pointer: 0,
            code,
            globals: GlobalMap::new(),
            last_op: OpCode::Interrupt,
        }
    }

    pub fn run(&mut self)
    {
        // Meta
        let mut stack_pointer_stack: Vec<usize> = Vec::new();
        let max: usize = self.code.code_idx;
        while self.code_ip < max 
        {
            let current_operation = OpCode::from_u8(self.code.code_chunk[self.code_ip]).unwrap();
            let current_line = self.code.code_line[self.code_ip];
            // println!("Current Stack: {:?}", self.stack);
            // println!("Current Environment: {:?}", self.globals);

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
                    self.code_ip += 1;
                },
                OpCode::Not | OpCode::Neg => {
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::Not => (!a).is_truthy().into(),
                        OpCode::Neg => -a,
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                },

                OpCode::EqEq | OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::EqEq => (a == b).into(),
                        OpCode::NotEq => (a != b).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
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
                    self.code_ip += 1;
                },
                OpCode::EqEq | OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::EqEq => (a >= b).into(),
                        OpCode::NotEq => (a <= b).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                },
                OpCode::Const8 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let value = self.code.read_data_8(index);
                    self.stack.push((value != 0).into());
                    self.code_ip = new_end;
                },
                OpCode::PushPtr => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    if index == 0 // Null Pointer
                    {
                        self.stack.push(Value::Null);
                    }
                    else
                    {
                        self.stack.push(Value::Pointer(index));
                    }
                    self.code_ip = new_end;
                },
                OpCode::Const64 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let value = self.code.read_data_64(index);
                    // GET_TYPE
                    self.stack.push(match self.code._data_type.get(&index).unwrap()
                    {
                        &Type::Int => i64::from_ne_bytes(value).into(),
                        &Type::Float => f64::from_bits(u64::from_ne_bytes(value)).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip = new_end;
                },
                OpCode::ConstDyn => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let value = self.code.read_data_dyn(index);

                    self.stack.push(match self.code._data_type.get(&index).unwrap() {
                        &Type::Str => Value::Str(String::from_utf8(value).unwrap()),
                        _ => unreachable!(),
                    });
                    self.code_ip = new_end;
                },
                OpCode::JumpIfFalse => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let test_value = self.stack.pop().unwrap();

                    self.code_ip = if test_value.is_truthy() { new_end } else { index };
                },

                OpCode::BStore | OpCode::IStore | OpCode::FStore | OpCode::SStore => {
                    self.code_ip += 1;
                    let indone = self.code.code_chunk[self.code_ip];
                    let indtwo = self.code.code_chunk[self.code_ip + 1];
                    let index = u16::from_ne_bytes([indone, indtwo]) as usize; 
                    let value = self.stack.last().unwrap().clone();
                    self.stack[index] = value;
                    self.code_ip += 2;
                },
                OpCode::BLoad | OpCode::ILoad | OpCode::FLoad | OpCode::SLoad  => {
                    self.code_ip += 1;
                    let indone = self.code.code_chunk[self.code_ip];
                    let indtwo = self.code.code_chunk[self.code_ip + 1];
                    let index = u16::from_ne_bytes([indone, indtwo]) as usize; 
                    let data = self.stack[index].clone();
                    self.stack.push(data);
                    self.code_ip += 2;
                },
                OpCode::Jump => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    self.code_ip = index;
                },
                OpCode::BlockIn  => {
                    stack_pointer_stack.push(self.stack_pointer);
                    self.stack_pointer = self.stack.len();
                    self.code_ip += 1;
                },
                OpCode::BlockOut => {
                    self.stack.truncate(self.stack_pointer);
                    let index = stack_pointer_stack.pop().unwrap();
                    self.stack_pointer = index;
                    self.code_ip += 1;
                },
                OpCode::GBLoad | OpCode::GILoad | OpCode::GFLoad | OpCode::GSLoad  => {
                    let operand_one = self.code.code_chunk[self.code_ip + 1];
                    let operand_two = self.code.code_chunk[self.code_ip + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);

                    let value = self.globals.get(&identifier).unwrap();
                    self.stack.push(value.clone());
                    self.code_ip += 3;
                },
                OpCode::GBStore | OpCode::GIStore | OpCode::GFStore | OpCode::GSStore => {
                    let operand_one = self.code.code_chunk[self.code_ip + 1];
                    let operand_two = self.code.code_chunk[self.code_ip + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);

                    let data = self.stack.pop().unwrap();
                    let value = self.globals.insert(identifier, data);
                    self.code_ip += 3;
                },
                OpCode::DebugPrint => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a);
                    self.code_ip += 1;
                },
                _ => {
                    println!("!!!!!!!!!!!!! PANIC !!!!!!!!!!!!!!!");
                    println!("OpCode Interrupt Detected at index {}", self.code_ip);
                    println!("Current Stack Data: {:?}", self.stack);
                    println!("Current Disassemble here");
                    let max_len = if self.code.code_chunk.len() < self.code_ip + 5 {
                        self.code.code_chunk.len()
                    }
                    else {
                        self.code_ip + 5
                    };
                    for message in self.code.disassemble(self.code_ip - 5, max_len)
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

