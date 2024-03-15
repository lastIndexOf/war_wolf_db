macro_rules! syntax {
    ($fn_name: ident, $match_str: literal, $token: expr) => {
        fn $fn_name(s: &str) -> nom::IResult<&str, $crate::lexer::token::Token> {
            nom::combinator::map(nom::bytes::complete::tag_no_case($match_str), |_| $token)(s)
        }
    };
}
