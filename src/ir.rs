// use serde::{Deserialize, Serialize};

// オペランドを持つ類のものは全てu32のオペランドを持つ
// usizeとかu16とか使い分けたかったけど無理
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum IRCode
{
    Const8(u32),
    Const16(u32),
    Const32(u32),
    Const64(u32),
    ConstDyn(u32),
    Null,
    True,
    False,

    Return,
    Push,
    Pop,

    PushPtr(u32),

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Not,

    EqEq,
    NotEq,
    LessEq,
    MoreEq,
    Less,
    More,
    And,
    Or,
    Neg,

    Jump(u32),
    JT(u32),
    JNT(u32),
    Call(u32),

    GILoad(u32),
    GFLoad(u32),
    GSLoad(u32),
    GBLoad(u32),
    GIStore(u32),
    GFStore(u32),
    GSStore(u32),
    GBStore(u32),

    ILoad(u32),
    FLoad(u32),
    SLoad(u32),
    BLoad(u32),
    IStore(u32),
    FStore(u32),
    SStore(u32),
    BStore(u32),

    Load(u32),
    Store(u32),
    GLoad(u32),
    GStore(u32),

    Interrupt,
    DebugPrint,
}

