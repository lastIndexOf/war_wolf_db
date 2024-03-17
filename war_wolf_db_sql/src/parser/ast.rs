//! stat 由 expr 组成
//! expr 由很多 ident(标识符) 或者 literal(字面量) 等组成
//!
//! #[derive(Debug, PartialEq, Clone)]
//! pub enum Stat {}
//!
//! #[derive(Debug, PartialEq, Clone)]
//! pub enum Expr {}
//!
//! #[derive(Debug, PartialEq, Clone)]
//! pub enum Ident {}

pub type Program = Vec<Clause>;

#[derive(Debug, PartialEq, Clone)]
pub enum Clause {
    SelectClause {
        targets: Vec<Ident>,
        from_table: Ident,
    },
    WhereClause(Vec<Ident>, Ident),
    GroupByClause(Vec<Ident>),
    OrderByClause {
        column: Ident,
        ordering: Ordering,
    },
    JoinClause {
        left: Ident,
        right: Ident,
        join_type: JoinType,
        condition: Option<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    StarExpr,
    IdentExpr(Ident),
    LiteralExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(String);

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ordering {
    Asc,
    Desc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum JoinType {
    Left,
    Right,
    Inner,
    Full,
    Cross,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Add,
    Sub,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Like,
}
