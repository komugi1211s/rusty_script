use crate::tokenizer::token::Token;

use trace::position::CodeSpan;
use trace::SourceFile;

pub mod declaration;
pub use declaration::*;

#[derive(Debug)]
pub struct ASTree<'m> {
    pub file: &'m SourceFile,
    pub ast: Vec<AstNode>,
    pub stmt: Vec<Statement>,
    pub expr: Vec<Expr>,
    pub functions: Vec<FunctionData>,
}

impl<'m> ASTree<'m> {
    pub fn new(m: &'m SourceFile) -> Self {
        Self {
            file: m,
            ast      : vec![],
            stmt     : vec![],
            expr     : vec![],
            functions: vec![]
        }
    }

    pub fn add_stmt(&mut self, stmt: Statement) -> StmtId {
        let index = self.stmt.len();
        self.stmt.push(stmt);
        StmtId(index as u32)
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprId {
        let index = self.expr.len();
        self.expr.push(expr);
        ExprId(index as u32)
    }

    pub fn add_fn(&mut self, fun: FunctionData) -> usize {
        let index = self.functions.len();
        self.functions.push(fun);
        index
    }

    pub fn add_ast(&mut self, stmt_id: StmtId, span: CodeSpan) {
        self.ast.push(AstNode::new(stmt_id, span));
    }

    pub fn get_expr(&'m self, id: ExprId) -> &'m Expr {
        self.expr.get(id.0 as usize).unwrap()
    }

    pub fn get_stmt(&'m self, id: StmtId) -> &'m Statement {
        self.stmt.get(id.0 as usize).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExprId(pub u32);


#[derive(Debug, Clone)]
pub struct AstNode {
    pub span   : CodeSpan,
    pub stmt_id: StmtId,
}

impl AstNode {
    pub fn new(stmt_id: StmtId, span: CodeSpan) -> Self {
        Self { span, stmt_id }
    }
}

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
pub enum Expr {
    Binary(ExprId, ExprId, Operator),
    Logical(ExprId, ExprId, Operator),
    FunctionCall(ExprId, Vec<ExprId>),
    Assign(ExprId, ExprId),

    Literal(Literal),
    Grouping(ExprId),
    Unary(ExprId, Operator),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // DebugPrint
    Print(ExprId),
    Return(Option<ExprId>),
    Expression(ExprId),
    // Defer(Expr),
    Decralation(DeclarationData),
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
        write!(f, "{}", match self {
            Add    => "+",
            Sub    => "-",
            Div    => "/",
            Mul    => "*",
            Mod    => "%",
            EqEq   => "==",
            NotEq  => "!=",
            LessEq => "<=",
            MoreEq => ">=",
            Less   => ">",
            More   => "<",
            Not    => "!",
            Neg    => "-",
            And    => "and",
            Or     => "or",
            Ref    => "^",
            Deref  => "^",
            Wrap   => "?",
            Unwrap => "!",
            Asgn   => "=",
            _      => "??"
        })
    }
} 


impl Operator {
    // NOTE: 新しくオペレータを追加した時エラーがでてほしいので全部手打ち
    pub fn is_arithmetic(&self) -> bool {
        // NOTE - @Improvement: Unaryのマイナスって計算式に入る？
        use Operator::*;
        match self {
            Add | Sub | Div | Mul | Mod | Neg => true,
            EqEq | NotEq | LessEq | MoreEq | Less | More | Not | Ref | Deref | Wrap
            | Unwrap | And | Or | Asgn => false,
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
pub struct Literal {
    pub tok : Token,
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
