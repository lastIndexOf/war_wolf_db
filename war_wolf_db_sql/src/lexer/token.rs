#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // DDL(data definition language)
    Create,
    Drop,
    Database,
    Table,
    Index,
    // DML(data manipulation language)
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
    DQuoteString(String),
    Null,
    Boolean(bool),
    // punctuation
    Dot,
    Comma,
    Semicolon,
    LParen,
    RParen,
    // others
    Explain,
    Illegal,
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
#[repr(C)]
pub struct Tokens<'t> {
    tokens: &'t Vec<Token>,
    pos: usize,
    size: usize,
}

impl<'t> Tokens<'t> {
    pub fn new(tokens: &'t Vec<Token>) -> Self {
        Tokens {
            tokens,
            pos: 0,
            size: tokens.len(),
        }
    }
}
