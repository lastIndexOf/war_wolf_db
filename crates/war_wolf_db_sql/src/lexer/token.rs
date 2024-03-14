#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // DDL(data definition language)
    Create,
    Drop,
    Database,
    Table,
    Index,
    // DML(data manipulation language)
    // query
    Select,
    Star,
    From,
    Where,
    GroupBy,
    OrderBy,
    Join,
    Full,
    Inner,
    Outer,
    Left,
    Right,
    On,
    // modify
    Insert,
    Delete,
    Update,
    Set,
    Into,
    Values,
    // operators
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Not,
    Like,
    // identifier and literals
    Id(String),
    Integer(i64),
    QuoteString(String),
    DquoteString(String),
    Null,
    True,
    False,
    // punctuation
    Dot,
    Comma,
    Semicolon,
    LParen,
    RParen,
    // others
    Explain,
}
