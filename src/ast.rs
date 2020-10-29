extern crate bitflags;
use super::{
    types::Type,
    tokenizer::token::Token,
    trace::prelude::*,
};

use std::cell::Cell;

#[derive(Debug)]
pub struct ASTree<'m>
{
    pub root: Vec<StmtId>,
    pub stmt: Vec<Statement<'m>>,
    pub expr: Vec<Expression<'m>>,
    pub functions: Vec<FunctionData>,
}

impl<'m> ASTree<'m>
{
    pub fn new() -> Self
    {
        Self {
            root: Vec::with_capacity(128),
            stmt: Vec::with_capacity(256),
            expr: Vec::with_capacity(256),
            functions: Vec::with_capacity(128),
        }
    }

    pub fn add_stmt(&mut self, stmt: Statement<'m>) -> StmtId
    {
        let index = self.stmt.len();
        self.stmt.push(stmt);
        StmtId(index as u32)
    }

    pub fn add_expr(&mut self, expr: Expression<'m>) -> ExprId
    {
        let index = self.expr.len();
        self.expr.push(expr);
        ExprId(index as u32)
    }

    pub fn add_fn(&mut self, fun: FunctionData) -> usize
    {
        let index = self.functions.len();
        self.functions.push(fun);
        index
    }

    pub fn add_root(&mut self, stmt_id: StmtId)
    {
        self.root.push(stmt_id);
    }

    pub fn get_expr<'a>(&'m self, id: ExprId) -> &'a Expression<'m>
    {
        self.expr.get(id.0 as usize).unwrap()
    }

    pub fn get_stmt<'a>(&'m self, id: StmtId) -> &'a Statement<'m>
    {
        self.stmt.get(id.0 as usize).unwrap()
    }

    pub fn set_parent_to_statement(&mut self, parent: StmtId, child: StmtId)
    {
        self.stmt[child.0 as usize].parent = Some(parent);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Statement<'m>
{
    pub span: CodeSpan,
    pub module: &'m SourceFile,
    pub data: Stmt,
    pub parent: Option<StmtId>,
}

impl Reportable for Statement<'_>
{
    fn sourcefile(&self) -> usize { 0 }
    fn span(&self) -> CodeSpan { self.span }
}

impl<'m> Statement<'m>
{
    pub fn report(&self, title: &str, message: &str)
    {
        report(title, message);
        spit_line(self.module, &self.span);
    }

    #[allow(dead_code)]
    pub fn is_parent_conditional(&self, ast: &ASTree<'m>) -> bool
    {
        if let Some(parent_id) = self.parent
        {
            let statement = ast.get_stmt(parent_id);
            match statement.data
            {
                Stmt::While(_, _) => true,
                Stmt::If(_, _, _) => true,
                Stmt::For(_, _, _, _) => true,
                _ => false,
            }
        }
        else
        {
            false
        }
    }

    pub fn function_contains_this_statement(&self, ast: &ASTree<'m>) -> Option<usize>
    {
        if let Some(parent_id) = self.parent
        {
            let statement = ast.get_stmt(parent_id);
            if let Stmt::Function(idx) = statement.data {
                Some(idx)
            } else {
                statement.function_contains_this_statement(ast)
            }
        }
        else
        {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'m>
{
    pub kind: ExprKind,
    pub module: &'m SourceFile,
    pub span: CodeSpan,

    pub lhs: Option<Box<Expression<'m>>>,
    pub rhs: Option<Box<Expression<'m>>>,

    pub oper: Option<Operator>,
    pub variable_name: Option<String>,

    pub array_expr: Vec<Expression<'m>>,

    pub arg_expr: Vec<Expression<'m>>,
    pub literal: Option<Literal<'m>>,

    // gets set by Semantic Analysis.
    pub local_idx: Option<u32>,
    pub end_type:  Option<Type>,
}


impl Expression<'_>
{
    #[allow(dead_code)]
    pub fn is_lvalue(&self) -> bool
    {
        match self.kind
        {
            ExprKind::Assign
            | ExprKind::Binary
            | ExprKind::FunctionCall
            | ExprKind::Grouping =>
            {
                if let Some(inner) = self.end_type.as_ref()
                {
                    use crate::types::TypeKind;
                    inner.kind == TypeKind::Ptr
                }
                else
                {
                    report_compiler_bug("型が未解決の式に左辺値判定は出来ません。", ::std::file!(), ::std::line!(), "self.end_type.is_none()" );
                    unreachable!()
                }
            }

            ExprKind::ArrayRef
            | ExprKind::Variable => true,

            ExprKind::FieldAccess => {
                if let Some(lhs) = self.lhs.as_ref()
                {
                    lhs.is_lvalue()
                }
                else
                {
                    unreachable!()
                }
            }
            ExprKind::Unary => false, // TODO: FIXME: Can be true depends on an operator.

            ExprKind::Empty
            | ExprKind::Logical
            | ExprKind::Literal
            | ExprKind::ArrayInst => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionData
{
    pub own_stmt: StmtId,
    pub it:   DeclarationData,
    pub args: Vec<DeclarationData>,
    pub body: Vec<StmtId>,

    // gets set by semantic analysis.
    pub implicit_return_required: Cell<bool>
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructData
{
    pub own_stmt: StmtId,
    pub it:       DeclarationData,
    pub body:     Vec<DeclarationData>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockData
{
    pub local_count: usize,
    pub statements: Vec<StmtId>,
}

impl Reportable for Expression<'_>
{
    fn sourcefile(&self) -> usize { 0 }
    fn span(&self) -> CodeSpan { self.span }
}

impl<'m> Expression<'m>
{
    pub fn report(&self, title: &str, message: &str)
    {
        report(title, message);
        spit_line(self.module, &self.span);
    }

}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ExprInit<'m>
{
    pub kind: ExprKind,
    pub module: Option<&'m SourceFile>,
    pub span: Option<CodeSpan>,

    pub lhs: Option<Box<Expression<'m>>>,
    pub rhs: Option<Box<Expression<'m>>>,

    pub oper: Option<Operator>,
    pub variable_name: Option<String>,

    pub array_expr: Vec<Expression<'m>>,
    pub arg_expr: Vec<Expression<'m>>,
    pub literal: Option<Literal<'m>>,
    pub end_type: Option<Type>,
}

impl<'m> ExprInit<'m>
{
    pub fn init(self) -> Expression<'m>
    {
        Expression {
            kind: self.kind,
            span: self.span.unwrap(),
            module: self.module.unwrap(),

            lhs: self.lhs,
            rhs: self.rhs,
            oper: self.oper,
            variable_name: self.variable_name,
            local_idx: None,
            array_expr: self.array_expr,
            arg_expr: self.arg_expr,
            literal: self.literal,
            end_type: self.end_type,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Default)]
pub struct StmtInit<'m>
{
    pub span: Option<CodeSpan>,
    pub module: Option<&'m SourceFile>,
    pub data: Option<Stmt>,
    pub parent: Option<StmtId>,
}

impl<'m> StmtInit<'m>
{
    pub fn init(self) -> Statement<'m>
    {
        Statement {
            span: self.span.unwrap(),
            module: self.module.unwrap(),
            data: self.data.unwrap(),
            parent: self.parent,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprKind
{
    Assign,

    Binary,
    Logical,

    Unary,

    FunctionCall,
    ArrayRef,
    FieldAccess,
    Variable,
    Literal,
    Grouping,
    ArrayInst,

    Empty,
}

impl Default for ExprKind
{
    fn default() -> Self
    {
        ExprKind::Empty
    }
}
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt
{
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

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator
{
    // Binary
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

    // Unary
    Not,
    Neg,

    // Logical
    And,
    Or,

    Ref,
    Deref,

    Wrap,
    Unwrap,

    Asgn,
}

impl std::fmt::Display for Operator
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        use Operator::*;
        write!(
            f,
            "{}",
            match self
            {
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
            }
        )
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'m>
{
    pub tok: Token<'m>,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind
{
    Bool,
    Str,
    Int,
    Float,
    Null,
}

impl<'m> Literal<'m>
{
    pub fn new(kind: LiteralKind, tok: &Token<'m>) -> Self
    {
        Self {
            kind,
            tok: tok.clone(),
        }
    }

    pub fn new_null(tok: &Token<'m>) -> Self
    {
        Self::new(LiteralKind::Null, tok)
    }

    pub fn new_bool(tok: &Token<'m>) -> Self
    {
        Self::new(LiteralKind::Bool, tok)
    }

    pub fn new_str(tok: &Token<'m>) -> Self
    {
        Self::new(LiteralKind::Str, tok)
    }

    pub fn new_int(tok: &Token<'m>) -> Self
    {
        Self::new(LiteralKind::Int, tok)
    }

    pub fn new_float(tok: &Token<'m>) -> Self
    {
        Self::new(LiteralKind::Float, tok)
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationData
{
    pub kind: DeclKind,
    pub name: String,
    pub dectype: ParsedType,
    pub prefix: DeclPrefix,

    pub expr: Option<ExprId>,
    pub span: CodeSpan,
}

impl DeclarationData
{
    pub fn is_inferred(&self) -> bool
    {
        self.prefix.is_empty() && self.dectype == ParsedType::Unknown
    }

    pub fn is_annotated(&self) -> bool
    {
        !self.is_inferred()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field(pub String, pub ParsedType);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum ParsedType
{
    Int,
    Str,
    Float,
    Boolean,
    Array(Box<ParsedType>, Option<u32>),
    Pointer(Box<ParsedType>),
    Optional(Box<ParsedType>),
    Struct(Vec<Field>),
    Userdef(String),
    Unknown,
}

impl ParsedType
{
    pub fn match_primitive(cand: &str) -> Option<Self>
    {
        match cand
        {
            "int" => Some(Self::Int),
            "string" => Some(Self::Str),
            "float" => Some(Self::Float),
            "bool" => Some(Self::Boolean),
            _ => None,
        }
    }
}

bitflags! {
    pub struct DeclPrefix: u16 {
        const CONST  = 1 << 1;
        const PUBLIC = 1 << 2;
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind
{
    Variable,
    Argument,
    Struct,
    StructField,
}
