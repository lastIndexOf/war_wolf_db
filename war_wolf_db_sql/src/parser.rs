use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{map, verify},
    multi::many0,
    sequence::terminated,
    Err, IResult,
};

use crate::lexer::token::{Token, Tokens};

use self::ast::{Clause, Ident, Program};

#[macro_use]
mod macros;
mod ast;

tag_token!(token_select, Token::Select);
tag_token!(token_where, Token::Where);
tag_token!(token_group_by, Token::GroupBy);
tag_token!(token_order_by, Token::OrderBy);
tag_token!(token_from, Token::From);
tag_token!(token_eof, Token::EOF);

pub struct Parser;

fn parse_clause(tokens: Tokens) -> IResult<Tokens, Clause> {
    alt((
        parse_clause_select,
        parse_clause_where,
        parse_clause_group_by,
        parse_clause_order_by,
        parse_clause_join,
    ))(tokens)
}

fn parse_clause_select(tokens: Tokens) -> IResult<Tokens, Clause> {
    todo!()
}

fn parse_clause_where(tokens: Tokens) -> IResult<Tokens, Clause> {
    todo!()
}

fn parse_clause_group_by(tokens: Tokens) -> IResult<Tokens, Clause> {
    todo!()
}

fn parse_clause_order_by(tokens: Tokens) -> IResult<Tokens, Clause> {
    todo!()
}

fn parse_clause_join(tokens: Tokens) -> IResult<Tokens, Clause> {
    todo!()
}

fn parse_ident(tokens: Tokens) -> IResult<Tokens, Ident> {
    todo!()
}

fn parse_program(tokens: Tokens) -> IResult<Tokens, Program> {
    terminated(many0(parse_clause), token_eof)(tokens)
}

impl Parser {
    pub fn parse(tokens: Tokens) -> Result<Program, Box<dyn std::error::Error>> {
        match parse_program(tokens) {
            Ok((_, ast_nodes)) => Ok(ast_nodes),
            Err(e) => Err(e.to_string().into()),
        }
    }
}

#[cfg(test)]
mod test_parser {
    use crate::{
        lexer::{token::Tokens, Lexer},
        parser::ast::Program,
        parser::Parser,
    };

    use super::ast::{Clause, Expr, Ident, Infix, Literal};

    fn compare_input_with_ast(input: &str, expected: Program) {
        let tokens = Lexer::lex(input).unwrap();
        let tokens = Tokens::new(&tokens);
        let ast = Parser::parse(tokens).unwrap();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_select_clause() {
        let input = "select a, count(a) from t1 group by a where b > 100";
        let expected: Program = vec![
            Clause::Select {
                targets: vec![
                    Expr::Ident(Ident("a".to_string())),
                    Expr::FnCall {
                        name: Ident("count".to_string()),
                        arguments: vec![Expr::Ident(Ident("a".to_string()))],
                    },
                ],
                from_table: Ident("t1".to_string()),
            },
            Clause::GroupBy(vec![Ident("a".to_string())]),
            Clause::Where(vec![Expr::Infix(
                Infix::Gt,
                Box::new(Expr::Ident(Ident(String::from("b")))),
                Box::new(Expr::Literal(Literal::Number(100))),
            )]),
        ];
        compare_input_with_ast(input, expected);
    }
}
