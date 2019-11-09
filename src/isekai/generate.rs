 
trait Expr {
}

struct Binary<EX> {
    left: EX,
    right: EX,
    oper: TokenType,
}

impl<EX: Expr> Binary<EX> {
    pub fn new(left: EX, right: EX, oper: TokenType) -> Self {
        Self {
            left,
            right,
            oper,
        }
    }
}

struct Grouping<EX> {
    expr: EX,
}

impl<EX: Expr> Grouping<EX> {
    pub fn new(expr: EX) -> Self {
        Self {
            expr,
        }
    }
}

struct Literal {
    value: TokenType,
}

impl Literal {
    pub fn new(value: TokenType) -> Self {
        Self {
            value,
        }
    }
}

struct Unary<EX> {
    oper: TokenType,
    right: EX,
}

impl<EX: Expr> Unary<EX> {
    pub fn new(oper: TokenType, right: EX) -> Self {
        Self {
            oper,
            right,
        }
    }
}

