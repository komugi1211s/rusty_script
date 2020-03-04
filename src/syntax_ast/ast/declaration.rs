use super::{BlockData, ExprId};

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationData {
    pub kind: DeclKind,
    pub name: String,
    pub dectype: ParsedType,

    pub prefix: DeclPrefix,
    pub expr: Option<ExprId>,
}

impl DeclarationData {
    pub fn is_inferred(&self) -> bool {
        self.prefix.is_empty() && self.dectype == ParsedType::pUnknown
    }

    pub fn is_constant(&self) -> bool {
        self.prefix.contains(DeclPrefix::Const)
    }

    pub fn is_annotated(&self) -> bool {
        !self.is_inferred()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedType {
    pInt,
    pStr,
    pFloat,
    pBoolean,
    pArray(Box<ParsedType>, Option<u32>),
    pPointer(Box<ParsedType>),
    pOptional(Box<ParsedType>),
    pStruct(BlockData),
    pUserdef(String),
    pUnknown,
}

impl ParsedType {
    pub fn match_primitive(cand: &str) -> Option<Self> {
        match cand {
            "int"    => Some(Self::pInt),
            "string" => Some(Self::pStr),
            "float"  => Some(Self::pFloat),
            "bool"   => Some(Self::pBoolean),
            _ => None,
        }
    }
}

bitflags! {
    pub struct DeclPrefix: u16 {
        const Const  = 1 << 1;
        const Public = 1 << 2;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Variable,
    Argument,
    Struct,
}
