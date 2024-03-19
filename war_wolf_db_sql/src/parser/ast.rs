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
    Select {
        targets: Vec<Expr>,
        from_table: Ident,
    },
    Where(Vec<Expr>),
    GroupBy(Vec<Ident>),
    OrderBy {
        column: Ident,
        ordering: Ordering,
    },
    Join {
        left: Ident,
        right: Ident,
        join_type: JoinType,
        condition: Option<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    FnCall { name: Ident, arguments: Vec<Expr> },
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub(crate) String);

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Star,
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
