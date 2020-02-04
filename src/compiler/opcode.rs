use num_traits::FromPrimitive;

#[derive(Debug, Clone, Copy, Hash, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // Const       = 0b00010000,
    // Const系統は全て8バイトのオペランドを取る。
    // このオペランドはCONST値の収められた配列の絶対インデックスとして動作する
    Const8   = 0b0001_0001, 
    Const16  = 0b0001_0010, 
    Const32  = 0b0001_0011, 
    Const64  = 0b0001_0100, 
    ConstDyn = 0b0001_0111, 

    // Operational = 0b00100000,
    Return  = 0b0010_0000,
    Push    = 0b0010_0010,
    Pop     = 0b0010_0011,
    BlockIn = 0b0010_0110,
    BlockOut = 0b0010_0111,

    // スタック内の特定の位置へのポインタをオペランドとして持つ
    // ポインタは8バイト
    // FIXME - @DumbCode: 本来のポインタの動作の仕方と全く違うので
    // 修正されるべき
    PushPtr = 0b0001_1000, 

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
    // グローバル変数の変数名の数をコンパイル時にカウントし、それをオペランドとして指定する
    // オペランドは2バイト(u16)
    GILoad  = 0b0110_0001,  
    GIStore = 0b0110_0010, 
    GFLoad  = 0b0110_0011,  
    GFStore = 0b0110_0100, 
    GSLoad  = 0b0110_0101,  
    GSStore = 0b0110_0110, 
    GBLoad  = 0b0110_0111,  
    GBStore = 0b0110_1000, 

    // Locals      = 0b01110000,
    // コンパイル時のスタックポインタがオペランドとして入る
    // オペランドは2バイト(u16)
    ILoad = 0b0111_0001,  
    IStore = 0b0111_0010, 
    FLoad = 0b0111_0011,  
    FStore = 0b0111_0100, 
    SLoad = 0b0111_0101,  
    SStore = 0b0111_0110, 
    BLoad = 0b0111_0111,  
    BStore = 0b0111_1000, 

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
