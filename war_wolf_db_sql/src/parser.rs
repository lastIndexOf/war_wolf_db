use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{map, map_res, opt},
    error::ErrorKind,
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Err, IResult,
};

use crate::lexer::token::{Token, Tokens};

use self::ast::{Clause, Expr, Ident, Infix, JoinType, Literal, Ordering, Program, Stmt};

#[macro_use]
mod macros;

pub mod ast;

tag_token!(token_create, Token::Create);
tag_token!(token_table, Token::Table);
tag_token!(token_index, Token::Index);
tag_token!(token_database, Token::Database);
tag_token!(token_insert, Token::Insert);
tag_token!(token_into, Token::Into);
tag_token!(token_values, Token::Values);
tag_token!(token_update, Token::Update);
tag_token!(token_set, Token::Set);
tag_token!(token_select, Token::Select);
tag_token!(token_from, Token::From);
tag_token!(token_join, Token::Join);
tag_token!(token_full, Token::Full);
tag_token!(token_left, Token::Left);
tag_token!(token_right, Token::Right);
tag_token!(token_inner, Token::Inner);
tag_token!(token_outer, Token::Outer);
tag_token!(token_on, Token::On);
tag_token!(token_where, Token::Where);
tag_token!(token_group_by, Token::GroupBy);
tag_token!(token_order_by, Token::OrderBy);
tag_token!(token_left_paren, Token::LParen);
tag_token!(token_right_paren, Token::RParen);
tag_token!(token_and, Token::And);
tag_token!(token_delete, Token::Delete);
tag_token!(token_dot, Token::Dot);
tag_token!(token_comma, Token::Comma);
tag_token!(token_semicolon, Token::Semicolon);
tag_token!(token_eof, Token::Eof);

pub struct Parser;

fn parse_stmt(tokens: Tokens) -> IResult<Tokens, Stmt> {
    alt((
        parse_stmt_create,
        parse_stmt_insert,
        parse_stmt_select,
        parse_stmt_update,
        parse_stmt_delete,
    ))(tokens)
}

fn parse_stmt_create(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(parse_clause_create, |clause| Stmt::CreateStmt(clause))(tokens)
}

fn parse_stmt_insert(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(parse_clause_insert, |clause| Stmt::InsertStmt(clause))(tokens)
}

fn parse_stmt_select(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(
        tuple((
            parse_clause_select,
            parse_clause_from,
            opt(parse_clause_where),
            opt(parse_clause_group_by),
            opt(parse_clause_order_by),
        )),
        |(select, from, condition, group_by, ordering)| Stmt::SelectStmt {
            select,
            from,
            condition,
            ordering,
            group_by,
        },
    )(tokens)
}

fn parse_stmt_update(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(
        pair(parse_clause_update, opt(parse_clause_where)),
        |(update, condition)| Stmt::UpdateStmt { update, condition },
    )(tokens)
}

fn parse_stmt_delete(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(
        tuple((token_delete, token_from, parse_ident, parse_clause_where)),
        |(_, _, target, condition)| Stmt::DeleteStmt { target, condition },
    )(tokens)
}

fn parse_clause_create(tokens: Tokens) -> IResult<Tokens, Clause> {
    map_res(
        tuple((
            token_create,
            alt((token_table, token_index, token_database)),
            parse_ident,
            delimited(
                token_left_paren,
                many1(tuple((opt(token_comma), parse_ident, parse_ident))),
                token_right_paren,
            ),
            opt(token_semicolon),
        )),
        |(_, create_type, name, rows, _)| {
            let create_type = match create_type {
                Token::Table => ast::CreateType::Table,
                Token::Index => ast::CreateType::Index,
                Token::Database => ast::CreateType::Database,
                _ => return Err(nom::error::Error::new(tokens, ErrorKind::Tag)),
            };

            Ok(Clause::CreateClause(
                create_type,
                name,
                rows.into_iter().map(|(_, name, ty)| (name, ty)).collect(),
            ))
        },
    )(tokens)
}

fn parse_clause_insert(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_insert,
            token_into,
            parse_ident,
            token_values,
            many1(preceded(
                opt(token_comma),
                delimited(
                    token_left_paren,
                    many1(preceded(opt(token_comma), parse_expr)),
                    token_right_paren,
                ),
            )),
        )),
        |(_, _, name, _, rows)| Clause::InsertClause(name, rows),
    )(tokens)
}

// UPDATE employees SET salary = 10000 WHERE name = 'xiaohua';
fn parse_clause_update(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_update,
            parse_ident,
            token_set,
            many1(preceded(opt(token_comma), parse_expr)),
        )),
        |(_, name, _, values)| Clause::UpdateClause(name, values),
    )(tokens)
}

fn parse_clause_select(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_select,
            many1(preceded(opt(token_comma), parse_expr)),
            opt(token_semicolon),
        )),
        |(_, exprs, _)| Clause::SelectClause(exprs),
    )(tokens)
}

fn parse_clause_from(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_from,
            parse_ident,
            opt(parse_clause_join),
            opt(token_semicolon),
        )),
        |(_, ident, join, _)| Clause::FromClause(ident, join.map(|join| Box::new(join))),
    )(tokens)
}

fn parse_clause_where(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_where,
            parse_expr,
            many0(preceded(token_and, parse_expr)),
            opt(token_semicolon),
        )),
        |(_, first, others, _)| Clause::WhereClause([vec![first], others].concat()),
    )(tokens)
}

fn parse_clause_group_by(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_group_by,
            many1(preceded(opt(token_comma), parse_expr)),
            opt(token_semicolon),
        )),
        |(_, exprs, _)| Clause::GroupByClause(exprs),
    )(tokens)
}

fn parse_clause_order_by(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_order_by,
            many1(preceded(
                opt(token_comma),
                tuple((parse_expr, opt(parse_ordering))),
            )),
            opt(token_semicolon),
        )),
        |(_, exprs, _)| {
            Clause::OrderByClause(
                exprs
                    .into_iter()
                    .map(|(ident, ordering)| (ident, ordering.unwrap_or(Ordering::Asc)))
                    .collect(),
            )
        },
    )(tokens)
}

fn parse_clause_join(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            opt(alt((
                token_full,
                token_left,
                token_right,
                token_inner,
                token_outer,
            ))),
            token_join,
            parse_ident,
            token_on,
            parse_expr,
            many0(preceded(token_and, parse_expr)),
        )),
        |(join_type, _, join_on, _, first, others)| {
            let join_type = match join_type {
                Some(Token::Full) => JoinType::Full,
                Some(Token::Left) => JoinType::Left,
                Some(Token::Right) => JoinType::Right,
                Some(Token::Inner) => JoinType::Inner,
                Some(Token::Outer) => JoinType::Outer,
                _ => JoinType::Cross,
            };

            Clause::JoinClause {
                join_on,
                join_type,
                condition: [vec![first], others].concat(),
            }
        },
    )(tokens)
}

fn parse_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (t, left) = alt((
        parse_dot_expr,
        parse_paren_expr,
        parse_fn_call_expr,
        parse_ident_expr,
        parse_literal_expr,
    ))(tokens)?;
    let (t2, op) = take(1_usize)(t)?;
    match parse_infix_op(op[0].clone()) {
        Some(op) => {
            let (t2, right) = parse_expr(t2)?;
            Ok((t2, Expr::InfixExpr(op, Box::new(left), Box::new(right))))
        }
        None => Ok((t, left)),
    }
}

// TODO: support multi dot expr
// bar.foo.baz
fn parse_dot_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(
        tuple((parse_ident, token_dot, parse_ident)),
        |(root, _, child)| {
            Expr::DotExpr(
                Box::new(Expr::IdentExpr(root)),
                Box::new(Expr::IdentExpr(child)),
            )
        },
    )(tokens)
}

fn parse_paren_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    delimited(token_left_paren, parse_expr, token_right_paren)(tokens)
}

fn parse_fn_call_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(
        tuple((
            parse_ident,
            token_left_paren,
            many0(preceded(opt(token_comma), parse_ident)),
            token_right_paren,
        )),
        |(name, _, params, _)| Expr::FnCallExpr {
            name,
            arguments: params
                .into_iter()
                .map(|param| Expr::IdentExpr(param))
                .collect(),
        },
    )(tokens)
}

fn parse_ident_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(parse_ident, Expr::IdentExpr)(tokens)
}

fn parse_literal_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(parse_literal, Expr::LiteralExpr)(tokens)
}

fn parse_ident(tokens: Tokens) -> IResult<Tokens, Ident> {
    let (t, token) = take(1_usize)(tokens)?;

    match token[0].clone() {
        Token::Id(ident) => Ok((t, Ident(ident))),
        _ => Err(Err::Error(nom::error::Error::new(t, ErrorKind::Tag))),
    }
}

fn parse_ordering(tokens: Tokens) -> IResult<Tokens, Ordering> {
    let (t, token) = take(1_usize)(tokens)?;

    match token[0].clone() {
        Token::Desc => Ok((t, Ordering::Desc)),
        Token::Asc => Ok((t, Ordering::Asc)),
        _ => Err(Err::Error(nom::error::Error::new(t, ErrorKind::Tag))),
    }
}

fn parse_literal(tokens: Tokens) -> IResult<Tokens, Literal> {
    let (t, token) = take(1_usize)(tokens)?;

    match token[0].clone() {
        Token::Star => Ok((t, Literal::Star)),
        Token::Integer(int) => Ok((t, Literal::Number(int))),
        Token::DQuoteString(s) | Token::QuoteString(s) => Ok((t, Literal::String(s))),
        _ => Err(Err::Error(nom::error::Error::new(t, ErrorKind::Tag))),
    }
}

fn parse_infix_op(token: Token) -> Option<Infix> {
    match token {
        Token::Assign => Some(Infix::Assign),
        Token::Gt => Some(Infix::Gt),
        Token::Ge => Some(Infix::Ge),
        Token::Lt => Some(Infix::Lt),
        Token::Le => Some(Infix::Le),
        Token::Like => Some(Infix::Like),
        _ => None,
    }
}

fn parse_program(tokens: Tokens) -> IResult<Tokens, Program> {
    terminated(many0(parse_stmt), token_eof)(tokens)
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
    use nom::{
        bytes::complete::take,
        combinator::{map, verify},
        error::Error,
        IResult,
    };

    use crate::{
        lexer::{
            token::{Token, Tokens},
            Lexer,
        },
        parser::{
            ast::{Clause, Expr, Ident, Infix, Literal, Ordering, Program},
            token_select, Parser,
        },
    };

    use super::ast::{JoinType, Stmt};

    fn compare_input_with_ast(input: &str, expected: Program) {
        let tokens = Lexer::lex(input).unwrap();
        let tokens = Tokens::new(&tokens);
        let ast = Parser::parse(tokens).unwrap();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_tag_token() {
        #[inline]
        fn token_select(tokens: Tokens) -> IResult<Tokens, Token> {
            map(
                verify(take(1_usize), |token: &Tokens<'_>| {
                    assert_eq!(token[0], Token::Select);
                    token[0] == Token::Select
                }),
                |token: Tokens<'_>| token[0].clone(),
            )(tokens)
        }

        let tokens = Lexer::lex("select").unwrap();
        let tokens = Tokens::new(&tokens);

        token_select(tokens).unwrap();
    }

    #[test]
    fn test_select_star_clause() {
        let input = "select * from t1;";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::LiteralExpr(Literal::Star)]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: None,
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_select_multi_keywords_clause() {
        let input = "select a, b, c from t1";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![
                Expr::IdentExpr(Ident("a".to_string())),
                Expr::IdentExpr(Ident("b".to_string())),
                Expr::IdentExpr(Ident("c".to_string())),
            ]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: None,
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_select_fn_call_keyword_clause() {
        let input = "select a, count(c) from t1";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![
                Expr::IdentExpr(Ident("a".to_string())),
                Expr::FnCallExpr {
                    name: Ident("count".to_string()),
                    arguments: vec![Expr::IdentExpr(Ident("c".to_string()))],
                },
            ]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: None,
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_from_with_join_clause() {
        let input = "select a from t1 join t2 on t1.a = t2.a";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(
                Ident("t1".to_string()),
                Some(Box::new(Clause::JoinClause {
                    join_on: Ident("t2".to_string()),
                    join_type: JoinType::Cross,
                    condition: vec![Expr::InfixExpr(
                        Infix::Assign,
                        Box::new(Expr::DotExpr(
                            Box::new(Expr::IdentExpr(Ident("t1".to_string()))),
                            Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                        )),
                        Box::new(Expr::DotExpr(
                            Box::new(Expr::IdentExpr(Ident("t2".to_string()))),
                            Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                        )),
                    )],
                })),
            ),
            condition: None,
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_from_with_left_join_clause() {
        let input = "select a from t1 left join t2 on t1.a = t2.a";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(
                Ident("t1".to_string()),
                Some(Box::new(Clause::JoinClause {
                    join_on: Ident("t2".to_string()),
                    join_type: JoinType::Left,
                    condition: vec![Expr::InfixExpr(
                        Infix::Assign,
                        Box::new(Expr::DotExpr(
                            Box::new(Expr::IdentExpr(Ident("t1".to_string()))),
                            Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                        )),
                        Box::new(Expr::DotExpr(
                            Box::new(Expr::IdentExpr(Ident("t2".to_string()))),
                            Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                        )),
                    )],
                })),
            ),
            condition: None,
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_where_lt_clause() {
        let input = "select a from t1 where a < 100";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: Some(Clause::WhereClause(vec![Expr::InfixExpr(
                Infix::Lt,
                Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                Box::new(Expr::LiteralExpr(Literal::Number(100))),
            )])),
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_where_like_clause() {
        let input = "select a from t1 where a like '%foo%'";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: Some(Clause::WhereClause(vec![Expr::InfixExpr(
                Infix::Like,
                Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                Box::new(Expr::LiteralExpr(Literal::String("%foo%".to_string()))),
            )])),
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_multi_where_clause() {
        let input = "select a from t1 where a < 100 and b > 100";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: Some(Clause::WhereClause(vec![
                Expr::InfixExpr(
                    Infix::Lt,
                    Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                    Box::new(Expr::LiteralExpr(Literal::Number(100))),
                ),
                Expr::InfixExpr(
                    Infix::Gt,
                    Box::new(Expr::IdentExpr(Ident("b".to_string()))),
                    Box::new(Expr::LiteralExpr(Literal::Number(100))),
                ),
            ])),
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_multi_where_with_paren_clause() {
        let input = "select a from t1 where (a < 100) and (b > 100)";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: Some(Clause::WhereClause(vec![
                Expr::InfixExpr(
                    Infix::Lt,
                    Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                    Box::new(Expr::LiteralExpr(Literal::Number(100))),
                ),
                Expr::InfixExpr(
                    Infix::Gt,
                    Box::new(Expr::IdentExpr(Ident("b".to_string()))),
                    Box::new(Expr::LiteralExpr(Literal::Number(100))),
                ),
            ])),
            ordering: None,
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    // TODO: Fix this test， impl precedence for infix operators
    // #[test]
    // fn test_multi_where_without_paren_clause() {
    //     let input = "where a < 100 and b > 100";
    //     let expected: Program = vec![Clause::WhereClause(Expr::InfixExpr(
    //         Infix::And,
    //         Box::new(Expr::InfixExpr(
    //             Infix::Lt,
    //             Box::new(Expr::IdentExpr(Ident(String::from("a")))),
    //             Box::new(Expr::LiteralExpr(Literal::Number(100))),
    //         )),
    //         Box::new(Expr::InfixExpr(
    //             Infix::Gt,
    //             Box::new(Expr::IdentExpr(Ident(String::from("b")))),
    //             Box::new(Expr::LiteralExpr(Literal::Number(100))),
    //         )),
    //     ))];
    //     compare_input_with_ast(input, expected);
    // }

    #[test]
    fn test_group_by_clause() {
        let input = "select a from t1 group by a;";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: None,
            ordering: None,
            group_by: Some(Clause::GroupByClause(vec![Expr::IdentExpr(Ident(
                "a".to_string(),
            ))])),
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_order_by_clause() {
        let input = "select a from t1 order by b, a desc";
        let expected: Program = vec![Stmt::SelectStmt {
            select: Clause::SelectClause(vec![Expr::IdentExpr(Ident("a".to_string()))]),
            from: Clause::FromClause(Ident("t1".to_string()), None),
            condition: None,
            ordering: Some(Clause::OrderByClause(vec![
                (Expr::IdentExpr(Ident("b".to_string())), Ordering::Asc),
                (Expr::IdentExpr(Ident("a".to_string())), Ordering::Desc),
            ])),
            group_by: None,
        }];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_update_clause() {
        let input = "update t1 set a = 1, b = 2 where c = 3;";
        let expected: Program = vec![Stmt::UpdateStmt {
            update: Clause::UpdateClause(
                Ident("t1".to_string()),
                vec![
                    Expr::InfixExpr(
                        Infix::Assign,
                        Box::new(Expr::IdentExpr(Ident("a".to_string()))),
                        Box::new(Expr::LiteralExpr(Literal::Number(1))),
                    ),
                    Expr::InfixExpr(
                        Infix::Assign,
                        Box::new(Expr::IdentExpr(Ident("b".to_string()))),
                        Box::new(Expr::LiteralExpr(Literal::Number(2))),
                    ),
                ],
            ),
            condition: Some(Clause::WhereClause(vec![Expr::InfixExpr(
                Infix::Assign,
                Box::new(Expr::IdentExpr(Ident("c".to_string()))),
                Box::new(Expr::LiteralExpr(Literal::Number(3))),
            )])),
        }];
        compare_input_with_ast(input, expected);
    }
}
