use num_traits::FromPrimitive;

#[derive(Debug, Clone, Copy, Hash, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // Const       = 0b00010000,
    Const8   = 0b0001_0001,   // usize分の大きさのオペランドを取る
    Const16  = 0b0001_0010,  // usize分の大きさのオペランドを取る
    Const32  = 0b0001_0011,  // usize分の大きさのオペランドを取る
    Const64  = 0b0001_0100,  // usize分の大きさのオペランドを取る
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

    // Logical     = 0b01000000,
    EqEq = 0b0100_0001,
    NotEq = 0b0100_0010,
    LessEq = 0b0100_0011,
    MoreEq = 0b0100_0100,
    Less = 0b0100_0101,
    More = 0b0100_0111,
    And = 0b0100_1000,
    Or = 0b0100_1001,
    Neg = 0b0100_1010,

    // Branching   = 0b01010000,
    Jump = 0b0101_0000,
    JT   = 0b0101_0010, // jumps if previous expr is true.
    JNT  = 0b0101_0011, // jumps if previous expr is false.
    Call = 0b0101_0100,

    // Globals = 0b01100000,
    GILoad =  0b0110_0001,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GIStore = 0b0110_0010, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFLoad =  0b0110_0011,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GFStore = 0b0110_0100, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSLoad =  0b0110_0101,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GSStore = 0b0110_0110, // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
    GBLoad =  0b0110_0111,  // 名前のu16(1) 名前のu16(2) の2つのオペランドを取る
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

    // System     = 0b11000000,
    Interrupt = 0b1100_1100,
    DebugPrint = 0b1111_0001,
}

impl OpCode {
    pub fn len(&self) -> usize {
        match self {
            OpCode::Const8 
            | OpCode::Const16 
            | OpCode::Const32 
            | OpCode::Const64 
            | OpCode::ConstDyn => std::mem::size_of::<usize>(),

            | OpCode::JT
            | OpCode::JNT
            | OpCode::Jump
            | OpCode::PushPtr 
            | OpCode::Push => std::mem::size_of::<usize>(),

            OpCode::GILoad  
            | OpCode::GFLoad 
            | OpCode::GSLoad 
            | OpCode::GBLoad => 2,

            OpCode::GIStore 
            | OpCode::GFStore 
            | OpCode::GSStore 
            | OpCode::GBStore => 2,

            OpCode::ILoad 
            | OpCode::FLoad 
            | OpCode::SLoad 
            | OpCode::BLoad => 2,

            OpCode::IStore 
            | OpCode::FStore 
            | OpCode::SStore 
            | OpCode::BStore => 2,

            OpCode::Call => 2,
            _ => 0,
        }
    }
}
