#[macro_use]
mod macros;
pub mod token;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, map_res, recognize},
    multi::many0,
    sequence::{delimited, pair},
    IResult,
};
use token::Token;

syntax!(keyword_create, "CREATE", Token::Create);
syntax!(keyword_drop, "DROP", Token::Drop);
syntax!(keyword_database, "DATABASE", Token::Database);
syntax!(keyword_table, "TABLE", Token::Table);
syntax!(keyword_index, "INDEX", Token::Index);
syntax!(keyword_select, "SELECT", Token::Select);
syntax!(keyword_star, "*", Token::Star);
syntax!(keyword_from, "FROM", Token::From);
syntax!(keyword_where, "WHERE", Token::Where);
syntax!(keyword_group_by, "GROUP BY", Token::GroupBy);
syntax!(keyword_order_by, "ORDER BY", Token::OrderBy);
syntax!(keyword_desc, "DESC", Token::Desc);
syntax!(keyword_asc, "ASC", Token::Asc);
syntax!(keyword_join, "JOIN", Token::Join);
syntax!(keyword_full, "FULL", Token::Full);
syntax!(keyword_inner, "INNER", Token::Inner);
syntax!(keyword_outer, "OUTER", Token::Outer);
syntax!(keyword_left, "LEFT", Token::Left);
syntax!(keyword_right, "RIGHT", Token::Right);
syntax!(keyword_on, "ON", Token::On);
syntax!(keyword_insert, "INSERT", Token::Insert);
syntax!(keyword_delete, "DELETE", Token::Delete);
syntax!(keyword_update, "UPDATE", Token::Update);
syntax!(keyword_set, "SET", Token::Set);
syntax!(keyword_into, "INTO", Token::Into);
syntax!(keyword_values, "VALUES", Token::Values);
syntax!(keyword_null, "NULL", Token::Null);
syntax!(keyword_explain, "EXPLAIN", Token::Explain);

fn lex_keyword(s: &str) -> nom::IResult<&str, Token> {
    alt((
        alt((
            keyword_create,
            keyword_insert,
            keyword_delete,
            keyword_update,
            keyword_drop,
            keyword_set,
            keyword_select,
            keyword_explain,
        )),
        alt((
            keyword_where,
            keyword_index,
            keyword_join,
            keyword_star,
            keyword_from,
            keyword_group_by,
            keyword_order_by,
            keyword_desc,
            keyword_asc,
            keyword_into,
            keyword_values,
            keyword_full,
            keyword_inner,
            keyword_outer,
            keyword_left,
            keyword_right,
            keyword_on,
        )),
        keyword_null,
        alt((keyword_database, keyword_table, keyword_null)),
    ))(s)
}

syntax!(operator_ne, "!=", Token::Ne);
syntax!(operator_ge, ">=", Token::Ge);
syntax!(operator_gt, ">", Token::Gt);
syntax!(operator_le, "<=", Token::Le);
syntax!(operator_lt, "<", Token::Lt);
syntax!(operator_eq, "=", Token::Eq);
syntax!(operator_and, "AND", Token::And);
syntax!(operator_or, "OR", Token::Or);
syntax!(operator_not, "NOT", Token::Not);
syntax!(operator_like, "LIKE", Token::Like);

fn lex_operator(s: &str) -> nom::IResult<&str, Token> {
    alt((
        operator_ne,
        operator_ge,
        operator_gt,
        operator_le,
        operator_lt,
        operator_eq,
        operator_and,
        operator_or,
        operator_not,
        operator_like,
    ))(s)
}

syntax!(punctuation_dot, ".", Token::Dot);
syntax!(punctuation_comma, ",", Token::Comma);
syntax!(punctuation_semicolon, ";", Token::Semicolon);
syntax!(punctuation_left_paren, "(", Token::LParen);
syntax!(punctuation_right_paren, ")", Token::RParen);

fn lex_punctuation(s: &str) -> nom::IResult<&str, Token> {
    alt((
        punctuation_dot,
        punctuation_comma,
        punctuation_semicolon,
        punctuation_left_paren,
        punctuation_right_paren,
    ))(s)
}

fn lex_id(s: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |id: &str| Token::Id(id.to_owned()),
    )(s)
}

fn lex_string(s: &str) -> IResult<&str, Token> {
    alt((
        delimited(
            char('"'),
            map(is_not("\""), |ele: &str| {
                Token::DQuoteString(ele.to_owned())
            }),
            char('"'),
        ),
        delimited(
            char('\''),
            map(is_not("'"), |ele: &str| Token::QuoteString(ele.to_owned())),
            char('\''),
        ),
    ))(s)
}

// TODO: support escape string
#[allow(dead_code)]
fn pis(s: &str) -> IResult<&str, Vec<&str>> {
    if s.is_empty() {
        return Ok(("", vec![]));
    }

    let (s, first) = take(1_usize)(s)?;

    println!("first = {first} s = {s} len = {}", s.len());
    match first {
        "\\" => {
            let (s, escape_cr) = take(1_usize)(s)?;
            let (s, rest) = pis(s)?;
            Ok((s, concat_str_vec(escape_cr, rest)))
        }
        _ => {
            let (s, rest) = pis(s)?;
            Ok((s, concat_str_vec(first, rest)))
        }
    }
}

fn concat_str_vec<'a>(first: &'a str, extend: Vec<&'a str>) -> Vec<&'a str> {
    let mut ret = Vec::from([first]);
    ret.extend(extend);
    ret
}

fn lex_integer(s: &str) -> IResult<&str, Token> {
    map_res(digit1, |s: &str| {
        i64::from_str_radix(s, 10).map(|ele| Token::Integer(ele))
    })(s)
}

fn lex_illegal(s: &str) -> IResult<&str, Token> {
    map(take(1_usize), |_| Token::Illegal)(s)
}

#[inline]
fn lex_token(input: &str) -> IResult<&str, Token> {
    alt((
        lex_keyword,
        lex_operator,
        lex_punctuation,
        lex_string,
        lex_id,
        lex_integer,
        lex_illegal,
    ))(input)
}

#[inline]
fn lex_tokens(input: &str) -> IResult<&str, Vec<Token>> {
    many0(delimited(multispace0, lex_token, multispace0))(input)
}

pub struct Lexer;

impl Lexer {
    pub fn lex(input: &str) -> Result<Vec<Token>, Box<dyn std::error::Error>> {
        match lex_tokens(input) {
            Ok((_, tokens)) => Ok([tokens, vec![Token::EOF]].concat()),
            Err(e) => Err(e.to_string().into()),
        }
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    #[test]
    fn test_lex_keyword() {
        assert_eq!(lex_keyword("CREATE"), Ok(("", Token::Create)));
        assert_eq!(lex_keyword("DROP"), Ok(("", Token::Drop)));
        assert_eq!(lex_keyword("DATABASE"), Ok(("", Token::Database)));
        assert_eq!(lex_keyword("TABLE"), Ok(("", Token::Table)));
        assert_eq!(lex_keyword("INDEX"), Ok(("", Token::Index)));
        assert_eq!(lex_keyword("SELECT"), Ok(("", Token::Select)));
        assert_eq!(lex_keyword("*"), Ok(("", Token::Star)));
        assert_eq!(lex_keyword("FROM"), Ok(("", Token::From)));
        assert_eq!(lex_keyword("WHERE"), Ok(("", Token::Where)));
        assert_eq!(lex_keyword("GROUP BY"), Ok(("", Token::GroupBy)));
        assert_eq!(lex_keyword("ORDER BY"), Ok(("", Token::OrderBy)));
        assert_eq!(lex_keyword("JOIN"), Ok(("", Token::Join)));
        assert_eq!(lex_keyword("FULL"), Ok(("", Token::Full)));
        assert_eq!(lex_keyword("INNER"), Ok(("", Token::Inner)));
        assert_eq!(lex_keyword("OUTER"), Ok(("", Token::Outer)));
        assert_eq!(lex_keyword("LEFT"), Ok(("", Token::Left)));
        assert_eq!(lex_keyword("RIGHT"), Ok(("", Token::Right)));
        assert_eq!(lex_keyword("ON"), Ok(("", Token::On)));
        assert_eq!(lex_keyword("INSERT"), Ok(("", Token::Insert)));
        assert_eq!(lex_keyword("DELETE"), Ok(("", Token::Delete)));
        assert_eq!(lex_keyword("UPDATE"), Ok(("", Token::Update)));
        assert_eq!(lex_keyword("SET"), Ok(("", Token::Set)));
        assert_eq!(lex_keyword("INTO"), Ok(("", Token::Into)));
        assert_eq!(lex_keyword("VALUES"), Ok(("", Token::Values)));
    }

    #[test]
    fn test_lex_id() {
        assert_eq!(lex_id("id"), Ok(("", Token::Id("id".to_owned()))));
        assert_eq!(lex_id("id_1"), Ok(("", Token::Id("id_1".to_owned()))));
        assert_eq!(lex_id("_id"), Ok(("", Token::Id("_id".to_owned()))));
        assert_eq!(lex_id("_id_1"), Ok(("", Token::Id("_id_1".to_owned()))));
    }

    #[test]
    fn test_lex_operator() {
        assert_eq!(lex_operator("="), Ok(("", Token::Eq)));
        assert_eq!(lex_operator("!="), Ok(("", Token::Ne)));
        assert_eq!(lex_operator(">"), Ok(("", Token::Gt)));
        assert_eq!(lex_operator(">="), Ok(("", Token::Ge)));
        assert_eq!(lex_operator("<"), Ok(("", Token::Lt)));
        assert_eq!(lex_operator("<="), Ok(("", Token::Le)));
        assert_eq!(lex_operator("AND"), Ok(("", Token::And)));
        assert_eq!(lex_operator("OR"), Ok(("", Token::Or)));
        assert_eq!(lex_operator("NOT"), Ok(("", Token::Not)));
        assert_eq!(lex_operator("LIKE"), Ok(("", Token::Like)));
    }

    #[test]
    fn test_lex_punctuation() {
        assert_eq!(lex_punctuation("."), Ok(("", Token::Dot)));
        assert_eq!(lex_punctuation(","), Ok(("", Token::Comma)));
        assert_eq!(lex_punctuation(";"), Ok(("", Token::Semicolon)));
        assert_eq!(lex_punctuation("("), Ok(("", Token::LParen)));
        assert_eq!(lex_punctuation(")"), Ok(("", Token::RParen)));
    }

    #[test]
    fn test_lex_string() {
        assert_eq!(
            lex_string("\"string\""),
            Ok(("", Token::DQuoteString("string".to_owned())))
        );
        assert_eq!(
            lex_string("'string'"),
            Ok(("", Token::QuoteString("string".to_owned())))
        );
    }

    #[test]
    fn test_base_sql() {
        let input = "CREATE DATABASE test;";
        let tokens = Lexer::lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Create,
                Token::Database,
                Token::Id("test".to_owned()),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_base_sql_with_double_quote_string() {
        let input = "CREATE DATABASE \"test\";";
        let tokens = Lexer::lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Create,
                Token::Database,
                Token::DQuoteString("test".to_owned()),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_base_sql_with_quote_string() {
        let input = "CREATE DATABASE 'test';";
        let tokens = Lexer::lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Create,
                Token::Database,
                Token::QuoteString("test".to_owned()),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_base_sql_with_ordering() {
        let input = "SELECT * FROM t1 ORDER BY a DESC, b ASC;";
        let tokens = Lexer::lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Select,
                Token::Star,
                Token::From,
                Token::Id("t1".to_owned()),
                Token::OrderBy,
                Token::Id("a".to_owned()),
                Token::Desc,
                Token::Comma,
                Token::Id("b".to_owned()),
                Token::Asc,
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    // TODO: escape string not support
    // #[test]
    // fn test_base_sql_with_escape_string() {
    //     let input = "CREATE DATABASE \"test\"\";";
    //     let tokens = Lexer::lex(input).unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             Token::Create,
    //             Token::Database,
    //             Token::DQuoteString("test\"".to_owned()),
    //             Token::Semicolon
    //         ]
    //     );
    // }

    #[test]
    fn test_complex_sql_with_operator() {
        let input = "select a, count(a) from t1 group by a where b > 100";
        let tokens = Lexer::lex(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                // correct token
                Token::Select,
                Token::Id("a".to_owned()),
                Token::Comma,
                Token::Id("count".to_owned()),
                Token::LParen,
                Token::Id("a".to_owned()),
                Token::RParen,
                Token::From,
                Token::Id("t1".to_owned()),
                Token::GroupBy,
                Token::Id("a".to_owned()),
                Token::Where,
                Token::Id("b".to_owned()),
                Token::Gt,
                Token::Integer(100),
                Token::EOF,
            ]
        );
    }
}
