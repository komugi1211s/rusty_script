use types::Type;
use crate::tokenizer::token::Token;

use trace::prelude::*;

pub mod declaration;
pub use declaration::*;

#[derive(Debug)]
pub struct ASTree<'m> {
    pub root: Vec<StmtId>,
    pub stmt: Vec<Statement<'m>>,
    pub expr: Vec<Expression<'m>>,
    pub functions: Vec<FunctionData>,
}


impl<'m> ASTree<'m> {
    pub fn new() -> Self {
        Self {
            root:      Vec::with_capacity(128),
            stmt:      Vec::with_capacity(256),
            expr:      Vec::with_capacity(256),
            functions: Vec::with_capacity(128),
        }
    }

    pub fn add_stmt(&mut self, stmt: Statement<'m>) -> StmtId {
        let index = self.stmt.len();
        self.stmt.push(stmt);
        StmtId(index as u32)
    }

    pub fn add_expr(&mut self, expr: Expression<'m>) -> ExprId {
        let index = self.expr.len();
        self.expr.push(expr);
        ExprId(index as u32)
    }

    pub fn add_fn(&mut self, fun: FunctionData) -> usize {
        let index = self.functions.len();
        self.functions.push(fun);
        index
    }

    pub fn add_root(&mut self, stmt_id: StmtId) {
        self.root.push(stmt_id);
    }

    pub fn get_expr(&'m self, id: ExprId) -> &'m Expression {
        self.expr.get(id.0 as usize).unwrap()
    }

    pub fn get_stmt(&'m self, id: StmtId) -> &'m Statement {
        self.stmt.get(id.0 as usize).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionData {
    pub it: DeclarationData,
    pub args: Vec<DeclarationData>,
    pub block_id: StmtId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockData {
    pub local_count: usize,
    pub statements: Vec<StmtId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'m> {
    pub kind: ExprKind,
    pub module: &'m SourceFile,
    pub span: CodeSpan,

    pub lhs: Option<Box<Expression<'m>>>,
    pub rhs: Option<Box<Expression<'m>>>,

    pub oper: Option<Operator>,

    pub variable_name: Option<String>,
    pub arg_expr: Vec<Expression<'m>>,
    pub literal: Option<Literal<'m>>,

    pub end_type: Option<Type>,
}

impl<'m> Expression<'m> {
    pub fn report(&self, title: &str, message: &str) {
        report(title, message);
        spit_line(self.module, &self.span);
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ExprInit<'m> {
    pub kind:     ExprKind,
    pub module:   Option<&'m SourceFile>,
    pub span:     Option<CodeSpan>,

    pub lhs: Option<Box<Expression<'m>>>,
    pub rhs: Option<Box<Expression<'m>>>,

    pub oper: Option<Operator>,

    pub variable_name: Option<String>,
    pub arg_expr: Vec<Expression<'m>>,
    pub literal: Option<Literal<'m>>,

    pub end_type: Option<Type>,
}

impl<'m> ExprInit<'m> {
    pub fn init(self) -> Expression<'m> {
        Expression {
            kind: self.kind,
            span: self.span.unwrap(),
            module: self.module.unwrap(),

            lhs: self.lhs,
            rhs: self.rhs,
            oper: self.oper,
            variable_name: self.variable_name,
            arg_expr: self.arg_expr,
            literal: self.literal,
            end_type: self.end_type
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement<'m> {
    pub span: CodeSpan,
    pub module: &'m SourceFile,
    pub data: Stmt,
}

impl<'m> Statement<'m> {
    pub fn report(&self, title: &str, message: &str) {
        report(title, message);
        spit_line(self.module, &self.span);
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct StmtInit<'m> {
    pub span: Option<CodeSpan>,
    pub module: Option<&'m SourceFile>,
    pub data: Option<Stmt>,
}

impl<'m> StmtInit<'m> {
    pub fn init(self) -> Statement<'m> {
        Statement {
            span: self.span.unwrap(),
            module: self.module.unwrap(),
            data: self.data.unwrap()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprKind {
    Binary,
    Logical,
    FunctionCall,
    Assign,

    Literal,
    Grouping,
    Unary,
    Variable,
    Empty,
}

impl Default for ExprKind {
    fn default() -> Self {
        ExprKind::Empty
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    // DebugPrint
    Print(ExprId),
    Return(Option<ExprId>),
    Expression(ExprId),
    // Defer(Expr),
    Declaration(DeclarationData),
    If(ExprId, StmtId, Option<StmtId>),
    While(ExprId, StmtId),
    For(StmtId, ExprId, ExprId, StmtId),
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

    Ref,
    Deref,

    Wrap,
    Unwrap,

    Asgn,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Operator::*;
        write!(
            f,
            "{}",
            match self {
                Add => "+",
                Sub => "-",
                Div => "/",
                Mul => "*",
                Mod => "%",
                EqEq => "==",
                NotEq => "!=",
                LessEq => "<=",
                MoreEq => ">=",
                Less => ">",
                More => "<",
                Not => "!",
                Neg => "-",
                And => "and",
                Or => "or",
                Ref => "^",
                Deref => "^",
                Wrap => "?",
                Unwrap => "!",
                Asgn => "=",
                _ => "??",
            }
        )
    }
}

impl Operator {
    // NOTE: 新しくオペレータを追加した時エラーがでてほしいので全部手打ち
    pub fn is_arithmetic(&self) -> bool {
        // NOTE - @Improvement: Unaryのマイナスって計算式に入る？
        use Operator::*;
        match self {
            Add | Sub | Div | Mul | Mod | Neg => true,
            EqEq | NotEq | LessEq | MoreEq | Less | More | Not | Ref | Deref | Wrap | Unwrap
            | And | Or | Asgn => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        use Operator::*;
        match self {
            EqEq | NotEq | LessEq | MoreEq | Less | More => true,
            Add | Sub | Div | Mul | Mod | Neg | Not | And | Or | Asgn | Ref | Deref | Wrap
            | Unwrap => false,
        }
    }

    pub fn is_logic(&self) -> bool {
        use Operator::*;
        match self {
            And | Or | Not => true,
            EqEq | NotEq | LessEq | MoreEq | Less | More | Add | Sub | Div | Mul | Mod | Ref
            | Deref | Wrap | Unwrap | Neg | Asgn => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'m> {
    pub tok: Token<'m>,
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

impl<'m> Literal<'m> {
    pub fn new(kind: LiteralKind, tok: &Token<'m>) -> Self {
        Self {
            kind,
            tok: tok.clone(),
        }
    }

    pub fn new_null(tok: &Token<'m>) -> Self {
        Self::new(LiteralKind::Null, tok)
    }

    pub fn new_bool(tok: &Token<'m>) -> Self {
        Self::new(LiteralKind::Bool, tok)
    }

    pub fn new_str(tok: &Token<'m>) -> Self {
        Self::new(LiteralKind::Str, tok)
    }

    pub fn new_int(tok: &Token<'m>) -> Self {
        Self::new(LiteralKind::Int, tok)
    }

    pub fn new_float(tok: &Token<'m>) -> Self {
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
