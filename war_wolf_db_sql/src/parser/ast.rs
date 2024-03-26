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

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    CreateStmt(Clause),
    InsertStmt(Clause),
    SelectStmt {
        select: Clause,
        from: Clause,
        condition: Option<Clause>,
        ordering: Option<Clause>,
        group_by: Option<Clause>,
    },
    UpdateStmt {
        update: Clause,
        condition: Option<Clause>,
    },
    DeleteStmt {
        target: Ident,
        condition: Clause,
    },
    DropStmt,
    ExplainStmt(Box<Stmt>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Clause {
    CreateClause(CreateType, Ident, Vec<(Ident, Ident)>),
    InsertClause(Ident, Vec<Vec<Expr>>),
    SelectClause(Vec<Expr>),
    UpdateClause(Ident, Vec<Expr>),
    FromClause(Ident, Option<Box<Clause>>),
    JoinClause {
        join_on: Ident,
        join_type: JoinType,
        condition: Vec<Expr>,
    },
    WhereClause(Vec<Expr>),
    GroupByClause(Vec<Expr>),
    OrderByClause(Vec<(Expr, Ordering)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LiteralExpr(Literal),
    FnCallExpr { name: Ident, arguments: Vec<Expr> },
    DotExpr(Box<Expr>, Box<Expr>),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Star,
    Number(i64),
    Boolean(bool),
    String(String),
}

#[derive(Default, Debug, PartialEq, Clone)]
pub enum Ordering {
    #[default]
    Asc,
    Desc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CreateType {
    Table,
    Index,
    Database,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub enum JoinType {
    #[default]
    Inner,
    Left,
    Right,
    Full,
    Outer,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Add,
    Sub,
    Not,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    #[default]
    Assign,
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
