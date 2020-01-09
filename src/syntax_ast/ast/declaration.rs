

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationData {
    pub kind: DeclKind,
    pub name: String,
    pub dectype: ParsedType,

    pub prefix: DeclPrefix,
    pub suffix: DeclSuffix,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedType {
    pInt,
    pStr,
    pFloat,
    pBoolean,
    pResult(Box<ParsedType>),
    pOption(Box<ParsedType>),
    pArray(Box<ParsedType>, Option<u32>),
    pPointer(Box<ParsedType>),
    pStruct(BlockData),
    pUnknown,
}

impl ParsedType {
    pub fn match_primitive(cand: &str) -> Option<Self> {
        match cand {
            "int" => Some(Self::pInt),
            "string" => Some(Self::pStr),
            "float" => Some(Self::pFloat),
            "bool" => Some(Self::pBoolean),
            _ => None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclPrefix {
    Empty,
    Pointer,
    Constant,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclSuffix {
    Empty,
    Optional,
    Resulted,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Variable,
    Argument,
    Struct,
}
