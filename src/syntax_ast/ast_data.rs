use std::fmt;
use crate::token::{ Token };
use types::types::{ Type };

#[derive(Debug)]
pub struct ParsedResult {
    pub functions: Vec<FunctionData>,
    pub statements: Vec<StatementNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}


#[derive(Debug, Clone)]
pub struct StatementNode {
    pub span: CodeSpan,
    pub value: Statement,
}


impl StatementNode {
    pub fn new(stmt: Statement, span: CodeSpan) -> Self {
        Self { value: stmt, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionData {
    pub it: DeclarationData,
    pub args: Vec<DeclarationData>,
    pub block: BlockData,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationData {
    pub kind: DeclKind,
    pub name: String,
    pub dectype: Type,

    // TODO - @Improvement: const, nullable, inferred は共存できない（どれか１つだけ有効化出来る）
    // 専用のフラグかステータスを作るべきだと思う
    pub is_const: bool,
    pub is_nullable: bool,
    pub is_inferred: bool,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Variable,
    Argument,
    Struct,
    StructField,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockData {
    pub local_count: usize,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Box<Expr>, Operator),
    Logical(Box<Expr>, Box<Expr>, Operator),
    FunctionCall(Box<Expr>, Vec<Expr>),
    Assign(String, Box<Expr>),

    Literal(Literal),
    Grouping(Box<Expr>),
    Unary(Box<Expr>, Operator),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // DebugPrint
    Print(Expr),
    Return(Option<Expr>),
    Expression(Expr),
    // Defer(Expr),
    Decralation(DeclarationData),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Box<Statement>, Expr, Expr, Box<Statement>),
    Block(BlockData),
    Function(usize),
    Break,
    Continue,
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
    Mod,

    EqEq,
    NotEq,
    LessEq,
    MoreEq,
    Less,
    More,
    Not,
    Neg,

    And,
    Or,

    Asgn,
}

impl Operator {
    // NOTE: 新しくオペレータを追加した時エラーがでてほしいので全部手打ち
    pub fn is_arithmetic(&self) -> bool {
        // NOTE - @Improvement: Unaryのマイナスって計算式に入る？
        use Operator::*;
        match self {
            Add | Sub | Div | Mul | Mod => true,
            EqEq| NotEq | LessEq | MoreEq
            | Less | More | Neg | Not
            | And | Or | Asgn => false
        }
    }

    pub fn is_comparison(&self) -> bool {
        use Operator::*;
        match self {
            EqEq | NotEq | LessEq | MoreEq | Less | More => true,
            Add | Sub | Div | Mul | Mod
            | Neg | Not | And | Or | Asgn => false,
        }
    }

    pub fn is_logic(&self) -> bool {
        use Operator::*;
        match self {
            And | Or | Not => true,
            EqEq | NotEq | LessEq | MoreEq | Less | More
            | Add | Sub | Div | Mul | Mod
            | Neg | Asgn => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub tok: Token,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Bool,
    Str,
    Int,
    Float,
    Null,
}

impl Literal {
    pub fn new(kind: LiteralKind, tok: &Token) -> Self {
        Self {
            kind,
            tok: tok.clone(),
        }
    }

    pub fn new_null(tok: &Token) -> Self {
        Self::new(LiteralKind::Null, tok)
    }

    pub fn new_bool(tok: &Token) -> Self {
        Self::new(LiteralKind::Bool, tok)
    }

    pub fn new_str(tok: &Token) -> Self {
        Self::new(LiteralKind::Str, tok)
    }

    pub fn new_int(tok: &Token) -> Self {
        Self::new(LiteralKind::Int, tok)
    }

    pub fn new_float(tok: &Token) -> Self {
        Self::new(LiteralKind::Float, tok)
    }

    pub fn is_str(&self) -> bool {
        self.kind == LiteralKind::Str
    }

    pub fn is_int(&self) -> bool {
        self.kind == LiteralKind::Int
    }

    pub fn is_float(&self) -> bool {
        self.kind == LiteralKind::Float
    }

    pub fn is_bool(&self) -> bool {
        self.kind == LiteralKind::Bool
    }

    pub fn is_null(&self) -> bool {
        self.kind == LiteralKind::Null
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int() || self.is_float()
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Binary(ref left, ref right, ref token) => {
                write!(f, "{} {} {:?}", left, right, token)
            }
            Expr::Logical(ref left, ref right, ref token) => {
                write!(f, "{} {} {:?}", left, right, token)
            }
            Expr::Grouping(ref inside) => write!(f, "({})", inside),
            Expr::Literal(ref lit) => write!(f, "{:?}", lit),
            Expr::Unary(ref item, ref oper) => write!(f, "{:?}{}", oper, item),
            Expr::Variable(ref s) => write!(f, "{}", s),
            Expr::Assign(s, ex) => write!(f, "{} = {}", s, ex),
            Expr::FunctionCall(ref name, ref items) => write!(f, "{}({:?})", name, items),
            x => write!(f, "unimplemented display expr {:?}", x),
        }
    }
}
