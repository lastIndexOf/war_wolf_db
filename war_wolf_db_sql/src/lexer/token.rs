use std::{iter::Enumerate, ops::Index, slice::Iter};

use nom::{InputIter, InputLength, InputTake, Needed};

#[derive(Debug, PartialEq, Eq, Clone)]
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
    Cross,
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
    Asc,
    Desc,
    // operators
    Assign,
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
    Eof,
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
pub struct Tokens<'t> {
    tokens: &'t [Token],
}

impl<'t> Tokens<'t> {
    pub fn new(tokens: &'t [Token]) -> Self {
        Tokens { tokens }
    }
}

impl<'t> Index<usize> for Tokens<'t> {
    type Output = Token;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

impl<'t> InputIter for Tokens<'t> {
    type Item = &'t Token;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Iter<'t, Token>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}

impl<'t> InputTake for Tokens<'t> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tokens: &self.tokens[0..count],
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        (
            Tokens {
                tokens: &self.tokens[count..],
            },
            Tokens {
                tokens: &self.tokens[0..count],
            },
        )
    }
}

impl<'t> InputLength for Tokens<'t> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl InputLength for Token {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}
