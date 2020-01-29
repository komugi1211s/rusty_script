use super::bytecode::{disassemble, ByteChunk};
use super::opcode::{OpCode};
use num_traits::FromPrimitive;
use std::collections::HashMap;
use types::{Type, TypeKind, Value};

#[cfg(target_pointer_width = "32")]
const USIZE_LENGTH: usize = 4;

#[cfg(target_pointer_width = "64")]
const USIZE_LENGTH: usize = 8;

type GlobalMap = HashMap<u16, Value>;
#[derive(Debug)]
pub struct VirtualMachine {
    pub chunk: ByteChunk,
    pub stack: Vec<Value>,
    pub code_ip: usize,
    pub stack_pointer: usize,
    pub globals: GlobalMap,
    pub last_op: OpCode,
}

impl VirtualMachine {
    pub fn new(chunk: ByteChunk) -> Self {
        let ep = chunk.entry_point;
        Self {
            chunk: chunk,
            stack: Vec::new(),
            code_ip: ep,
            stack_pointer: 0,
            globals: GlobalMap::new(),
            last_op: OpCode::Interrupt,
        }
    }

    pub fn run(&mut self) {
        let mut stack_frame: Vec<usize> = Vec::new();
        let mut call_frame: Vec<usize> = Vec::new();
        let max: usize = self.chunk.code.current_length();
        while self.code_ip < max {
            let current_operation = OpCode::from_u8(self.chunk.code.bytes[self.code_ip]);
            if current_operation.is_none() {
                panic!(
                    "Received {:04X} which is not an actual opcode, at line {}",
                    self.chunk.code.bytes[self.code_ip], self.chunk.code.span[self.code_ip]
                );
            }
            let _current_line = self.chunk.code.span[self.code_ip];
            // println!("Current Stack: {:?}", self.stack);
            // println!("Current Environment: {:?}", self.globals);
            let current_operation = current_operation.unwrap();

            match current_operation {
                OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div | OpCode::Mod => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::Add => a + b,
                        OpCode::Sub => a - b,
                        OpCode::Mul => a * b,
                        OpCode::Div => a / b,
                        OpCode::Mod => a % b,
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                }
                OpCode::Not | OpCode::Neg => {
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::Not => (!a).is_truthy().into(),
                        OpCode::Neg => -a,
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                }

                OpCode::EqEq | OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::EqEq => (a == b).into(),
                        OpCode::NotEq => (a != b).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                }

                OpCode::LessEq | OpCode::MoreEq | OpCode::Less | OpCode::More => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::LessEq => (a <= b).into(),
                        OpCode::MoreEq => (a >= b).into(),
                        OpCode::Less => (a < b).into(),
                        OpCode::More => (a > b).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                }
                OpCode::EqEq | OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match current_operation {
                        OpCode::EqEq => (a >= b).into(),
                        OpCode::NotEq => (a <= b).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip += 1;
                }
                OpCode::Const8 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let (value, _type) = self.chunk.constants.read_data_8(index);
                    self.stack.push((value != 0).into());
                    self.code_ip = new_end;
                }
                OpCode::PushPtr => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    if index == 0
                    // Null Pointer
                    {
                        self.stack.push(Value::Null);
                    } else {
                        self.stack.push(Value::Pointer(index));
                    }
                    self.code_ip = new_end;
                }
                OpCode::Const64 => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let (value, _type) = self.chunk.constants.read_data_64(index);
                    // GET_TYPE
                    self.stack.push(match _type.kind {
                        TypeKind::Int => i64::from_ne_bytes(value).into(),
                        TypeKind::Float => f64::from_bits(u64::from_ne_bytes(value)).into(),
                        _ => unreachable!(),
                    });
                    self.code_ip = new_end;
                }
                OpCode::ConstDyn => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let (value, _type) = self.chunk.constants.read_data_dyn(index);

                    self.stack.push(match _type.kind {
                        TypeKind::Str => Value::Str(String::from_utf8(value).unwrap()),
                        _ => unreachable!(),
                    });
                    self.code_ip = new_end;
                }
                OpCode::JumpIfFalse => {
                    let (index, new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    let test_value = self.stack.pop().unwrap();

                    self.code_ip = if test_value.is_truthy() {
                        new_end
                    } else {
                        index
                    };
                }
                OpCode::Jump => {
                    let (index, _new_end): (usize, usize) = self.consume_const_index(self.code_ip);
                    self.code_ip = index;
                }
                OpCode::BlockIn => {
                    stack_frame.push(self.stack_pointer);
                    self.stack_pointer = self.stack.len();
                    self.code_ip += 1;
                }
                OpCode::BlockOut => {
                    self.stack.truncate(self.stack_pointer);
                    let index = stack_frame.pop().unwrap();
                    self.stack_pointer = index;
                    self.code_ip += 1;
                }
                OpCode::BLoad | OpCode::ILoad | OpCode::FLoad | OpCode::SLoad => {
                    self.code_ip += 1;
                    let indone = self.chunk.code.bytes[self.code_ip];
                    let indtwo = self.chunk.code.bytes[self.code_ip + 1];
                    let index = u16::from_ne_bytes([indone, indtwo]) as usize;
                    let data = self.stack[self.stack_pointer + index].clone();
                    self.stack.push(data);
                    self.code_ip += 2;
                }
                OpCode::BStore | OpCode::IStore | OpCode::FStore | OpCode::SStore => {
                    self.code_ip += 1;
                    let indone = self.chunk.code.bytes[self.code_ip];
                    let indtwo = self.chunk.code.bytes[self.code_ip + 1];
                    let index = u16::from_ne_bytes([indone, indtwo]) as usize;
                    let value = self.stack.last().unwrap().clone();
                    self.stack[self.stack_pointer + index] = value;
                    self.code_ip += 2;
                }
                OpCode::GBLoad | OpCode::GILoad | OpCode::GFLoad | OpCode::GSLoad => {
                    let operand_one = self.chunk.code.bytes[self.code_ip + 1];
                    let operand_two = self.chunk.code.bytes[self.code_ip + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);

                    let value = self.globals.get(&identifier).unwrap();
                    self.stack.push(value.clone());
                    self.code_ip += 3;
                }
                OpCode::GBStore | OpCode::GIStore | OpCode::GFStore | OpCode::GSStore => {
                    let operand_one = self.chunk.code.bytes[self.code_ip + 1];
                    let operand_two = self.chunk.code.bytes[self.code_ip + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]);

                    let data = self.stack.pop().unwrap();
                    let _value = self.globals.insert(identifier, data);
                    self.code_ip += 3;
                }
                OpCode::DebugPrint => {
                    let a = self.stack.pop().unwrap();
                    println!("{}", a);
                    self.code_ip += 1;
                }
                OpCode::Call => {
                    let operand_one = self.chunk.code.bytes[self.code_ip + 1];
                    let operand_two = self.chunk.code.bytes[self.code_ip + 2];
                    let identifier = u16::from_ne_bytes([operand_one, operand_two]) as usize;

                    let func_info = &self.chunk.functions[&identifier];
                    if func_info.is_native {
                        (func_info.native_pointer.unwrap())(self);
                        self.code_ip += 3;
                    } else {
                        stack_frame.push(self.stack_pointer);
                        call_frame.push(self.code_ip + 2);
                        self.stack_pointer = self.stack.len() - func_info.arg_count;
                        self.code_ip = func_info.position;
                    }
                }
                OpCode::Return => {
                    let return_value = self.stack.pop().unwrap();
                    if stack_frame.is_empty() || call_frame.is_empty() {
                        break;
                    }
                    self.stack.truncate(self.stack_pointer);
                    let recover_stack = stack_frame.pop().unwrap();
                    self.stack_pointer = recover_stack;
                    self.stack.push(return_value);

                    let recover_ep = call_frame.pop().unwrap();

                    self.code_ip = recover_ep + 1;
                }
                _ => {
                    println!("!!!!!!!!!!!!! PANIC !!!!!!!!!!!!!!!");
                    println!("OpCode Interrupt Detected at index {}", self.code_ip);
                    println!("Current Stack Data: {:?}", self.stack);
                    println!("Current Disassemble here");
                    let max_len = if self.chunk.code.current_length() < self.code_ip + 5 {
                        self.chunk.code.current_length()
                    } else {
                        self.code_ip + 5
                    };
                    for message in disassemble(&self.chunk.code, self.code_ip - 5, max_len) {
                        println!("{}", message);
                    }
                    panic!();
                }
            }
            self.last_op = current_operation;
        }
    }

    pub fn consume_const_index(
        &mut self,
        start: usize,
    ) -> (/* result */ usize, /* new_end */ usize) {
        let start = start + 1;
        let mut new_end = start;
        let mut result: [u8; USIZE_LENGTH] = [0; USIZE_LENGTH];
        for i in 0..USIZE_LENGTH {
            result[i] = self.chunk.code.bytes[start + i];
            new_end += 1;
        }

        let result: usize = usize::from_ne_bytes(result);
        (result, new_end)
    }
}
