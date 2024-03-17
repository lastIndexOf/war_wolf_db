macro_rules! tag_token {
    ($fn_name: ident, $token: expr) => {
        // fn $fn_name(s: &str) -> nom::IResult<&str, $crate::lexer::token::Token> {
        //     nom::combinator::map(nom::bytes::complete::tag_no_case($match_str), |_| $token)(s)
        // }
    };
}
