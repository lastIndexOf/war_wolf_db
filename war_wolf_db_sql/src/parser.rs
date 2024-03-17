use crate::lexer::token::Tokens;

#[macro_use]
mod macros;
mod ast;

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Tokens) {}
}

#[cfg(test)]
mod test_parser {}
