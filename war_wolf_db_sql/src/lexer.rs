#[macro_use]
mod macros;
mod token;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::{map, recognize},
    multi::many0,
    sequence::pair,
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
syntax!(keyword_explain, "EXPLAIN", Token::Explain);

fn lex_keyword(s: &str) -> nom::IResult<&str, Token> {
    alt((
        alt((
            keyword_insert,
            keyword_delete,
            keyword_update,
            keyword_create,
            keyword_select,
            keyword_set,
            keyword_drop,
            keyword_explain,
            keyword_index,
        )),
        alt((
            keyword_database,
            keyword_table,
            keyword_star,
            keyword_from,
            keyword_into,
            keyword_values,
            keyword_where,
            keyword_group_by,
            keyword_order_by,
            keyword_join,
            keyword_full,
            keyword_inner,
            keyword_outer,
            keyword_left,
            keyword_right,
            keyword_on,
        )),
    ))(s)
}

fn lex_operator(s: &str) -> nom::IResult<&str, Token> {
    todo!()
}

fn lex_punctuation(s: &str) -> nom::IResult<&str, Token> {
    todo!()
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
}
