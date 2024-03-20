use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    Err, IResult,
};

use crate::lexer::token::{Token, Tokens};

use self::ast::{Clause, Expr, Ident, Infix, Literal, Ordering, Program};

#[macro_use]
mod macros;
mod ast;

tag_token!(token_select, Token::Select);
tag_token!(token_from, Token::From);
tag_token!(token_where, Token::Where);
tag_token!(token_group_by, Token::GroupBy);
tag_token!(token_order_by, Token::OrderBy);
tag_token!(token_left_paren, Token::LParen);
tag_token!(token_right_paren, Token::RParen);
tag_token!(token_comma, Token::Comma);
tag_token!(token_semicolon, Token::Semicolon);
tag_token!(token_eof, Token::EOF);

pub struct Parser;

fn parse_clause(tokens: Tokens) -> IResult<Tokens, Clause> {
    alt((
        parse_clause_select,
        parse_clause_from,
        parse_clause_where,
        parse_clause_group_by,
        parse_clause_order_by,
        // parse_clause_join,
    ))(tokens)
}

fn parse_clause_select(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_select,
            many1(preceded(opt(token_comma), parse_expr)),
            opt(token_semicolon),
        )),
        |(_, exprs, _)| Clause::Select(exprs),
    )(tokens)
}

fn parse_clause_from(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((token_from, parse_ident, opt(token_semicolon))),
        |(_, ident, _)| Clause::From(ident),
    )(tokens)
}

fn parse_clause_where(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((token_where, parse_expr, opt(token_semicolon))),
        |(_, expr, _)| Clause::Where(expr),
    )(tokens)
}

fn parse_clause_group_by(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_group_by,
            many1(preceded(opt(token_comma), parse_ident)),
            opt(token_semicolon),
        )),
        |(_, exprs, _)| Clause::GroupBy(exprs),
    )(tokens)
}

fn parse_clause_order_by(tokens: Tokens) -> IResult<Tokens, Clause> {
    map(
        tuple((
            token_order_by,
            many1(preceded(
                opt(token_comma),
                tuple((parse_ident, opt(parse_ordering))),
            )),
            opt(token_semicolon),
        )),
        |(_, exprs, _)| {
            Clause::OrderBy(
                exprs
                    .into_iter()
                    .map(|(ident, ordering)| (ident, ordering.unwrap_or(Ordering::Asc)))
                    .collect(),
            )
        },
    )(tokens)
}

fn parse_clause_join(tokens: Tokens) -> IResult<Tokens, Clause> {
    todo!()
}

fn parse_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (t, left) = alt((
        parse_paren_expr,
        parse_fn_call_expr,
        parse_ident_expr,
        parse_literal_expr,
    ))(tokens)?;
    let (t2, op) = take(1_usize)(t)?;
    match parse_infix_op(op[0].clone()) {
        Some(op) => {
            let (t2, right) = parse_expr(t2)?;
            Ok((t2, Expr::Infix(op, Box::new(left), Box::new(right))))
        }
        None => Ok((t, left)),
    }
}

fn parse_fn_call_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(
        tuple((
            parse_ident,
            token_left_paren,
            many0(preceded(opt(token_comma), parse_ident)),
            token_right_paren,
        )),
        |(name, _, params, _)| Expr::FnCall {
            name,
            arguments: params.into_iter().map(|param| Expr::Ident(param)).collect(),
        },
    )(tokens)
}

fn parse_ident_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(parse_ident, Expr::Ident)(tokens)
}

fn parse_literal_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(parse_literal, Expr::Literal)(tokens)
}

fn parse_paren_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    delimited(token_left_paren, parse_expr, token_right_paren)(tokens)
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
        Token::Gt => Some(Infix::Gt),
        Token::Ge => Some(Infix::Ge),
        Token::Lt => Some(Infix::Lt),
        Token::Le => Some(Infix::Le),
        Token::And => Some(Infix::And),
        Token::Like => Some(Infix::Like),
        _ => None,
    }
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

    use super::ast::JoinType;

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
        let input = "select *;";
        let expected: Program = vec![Clause::Select(vec![Expr::Literal(Literal::Star)])];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_select_multi_keywords_clause() {
        let input = "select a, b, c";
        let expected: Program = vec![Clause::Select(vec![
            Expr::Ident(Ident("a".to_string())),
            Expr::Ident(Ident("b".to_string())),
            Expr::Ident(Ident("c".to_string())),
        ])];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_select_fn_call_keyword_clause() {
        let input = "select a, count(c)";
        let expected: Program = vec![Clause::Select(vec![
            Expr::Ident(Ident("a".to_string())),
            Expr::FnCall {
                name: Ident("count".to_string()),
                arguments: vec![Expr::Ident(Ident("c".to_string()))],
            },
        ])];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_from_clause() {
        let input = "from t1";
        let expected: Program = vec![Clause::From(Ident("t1".to_string()))];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_where_lt_clause() {
        let input = "where a < 100";
        let expected: Program = vec![Clause::Where(Expr::Infix(
            Infix::Lt,
            Box::new(Expr::Ident(Ident(String::from("a")))),
            Box::new(Expr::Literal(Literal::Number(100))),
        ))];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_where_like_clause() {
        let input = "where a like '%foo%'";
        let expected: Program = vec![Clause::Where(Expr::Infix(
            Infix::Like,
            Box::new(Expr::Ident(Ident(String::from("a")))),
            Box::new(Expr::Literal(Literal::String("%foo%".to_string()))),
        ))];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_multi_where_clause() {
        let input = "where a < 100 where b > 100";
        let expected: Program = vec![
            Clause::Where(Expr::Infix(
                Infix::Lt,
                Box::new(Expr::Ident(Ident(String::from("a")))),
                Box::new(Expr::Literal(Literal::Number(100))),
            )),
            Clause::Where(Expr::Infix(
                Infix::Gt,
                Box::new(Expr::Ident(Ident(String::from("b")))),
                Box::new(Expr::Literal(Literal::Number(100))),
            )),
        ];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_multi_where_with_paren_clause() {
        let input = "where (a < 100) and (b > 100)";
        let expected: Program = vec![Clause::Where(Expr::Infix(
            Infix::And,
            Box::new(Expr::Infix(
                Infix::Lt,
                Box::new(Expr::Ident(Ident(String::from("a")))),
                Box::new(Expr::Literal(Literal::Number(100))),
            )),
            Box::new(Expr::Infix(
                Infix::Gt,
                Box::new(Expr::Ident(Ident(String::from("b")))),
                Box::new(Expr::Literal(Literal::Number(100))),
            )),
        ))];
        compare_input_with_ast(input, expected);
    }

    // TODO: Fix this test， impl precedence for infix operators
    // #[test]
    // fn test_multi_where_without_paren_clause() {
    //     let input = "where a < 100 and b > 100";
    //     let expected: Program = vec![Clause::Where(Expr::Infix(
    //         Infix::And,
    //         Box::new(Expr::Infix(
    //             Infix::Lt,
    //             Box::new(Expr::Ident(Ident(String::from("a")))),
    //             Box::new(Expr::Literal(Literal::Number(100))),
    //         )),
    //         Box::new(Expr::Infix(
    //             Infix::Gt,
    //             Box::new(Expr::Ident(Ident(String::from("b")))),
    //             Box::new(Expr::Literal(Literal::Number(100))),
    //         )),
    //     ))];
    //     compare_input_with_ast(input, expected);
    // }

    #[test]
    fn test_group_by_clause() {
        let input = "group by a;";
        let expected: Program = vec![Clause::GroupBy(vec![Ident("a".to_string())])];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_order_by_clause() {
        let input = "order by b, a desc";
        let expected: Program = vec![Clause::OrderBy(vec![
            (Ident("b".to_string()), Ordering::Asc),
            (Ident("a".to_string()), Ordering::Desc),
        ])];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_update_clause() {
        let input = "update t1 set a = 1, b = 2 where c = 3;";
        let expected: Program = vec![
            Clause::Update(Ident("t1".to_string())),
            Clause::Set(vec![
                Expr::Infix(
                    Infix::Eq,
                    Box::new(Expr::Ident(Ident("a".to_string()))),
                    Box::new(Expr::Literal(Literal::Number(1))),
                ),
                Expr::Infix(
                    Infix::Eq,
                    Box::new(Expr::Ident(Ident("b".to_string()))),
                    Box::new(Expr::Literal(Literal::Number(2))),
                ),
            ]),
            Clause::Where(Expr::Infix(
                Infix::Eq,
                Box::new(Expr::Ident(Ident("c".to_string()))),
                Box::new(Expr::Literal(Literal::Number(3))),
            )),
        ];
        compare_input_with_ast(input, expected);
    }

    #[test]
    fn test_complex_sql() {
        // complex sql demo
        //
        // CREATE TABLE employees (uid int, name text, salary int);
        // INSERT INTO employees VALUES (1, 'xiaoming', 3000);
        // INSERT INTO employees VALUES (2, 'xiaoxiaohong', 4000);
        // INSERT INTO employees VALUES (3, 'xiaohua', 5000);
        // SELECT count(uid) FROM employees;
        // EXPLAIN SELECT * FROM employees WHERE uid >= 2 ORDER BY salary;
        // CREATE INDEX idx ON employees(uid, salary);
        // EXPLAIN SELECT uid FROM employees WHERE uid >= 2 ORDER BY salary;
        // DELETE FROM employees WHERE uid = 1;
        // SELECT * FROM employees WHERE uid = 1;
        // UPDATE FROM employees SET salary = 10000 WHERE name = 'xiaohua';
        // SELECT salary, max(salary) FROM employees GROUP BY salary;

        // CREATE TABLE orders (order_id int, customer_name text, employee_uid int);
        // INSERT INTO orders VALUES (1, 'tom', 3), (2, 'tim', 3), (3, 'mike', 1);
        // SELECT employees.name, orders.customer_name FROM employees FULL JOIN orders ON employees.uid = orders.employee_uid;
        // SELECT employees.name, orders.customer_name FROM employees INNER JOIN orders ON employees.uid = orders.employee_uid;
        // SELECT employees.name, orders.customer_name FROM employees INNER JOIN orders ON employees.uid = orders.employee_uid ORDER BY employees.salary;
    }
}
