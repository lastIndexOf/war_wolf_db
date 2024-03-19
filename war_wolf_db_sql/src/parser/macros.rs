macro_rules! tag_token {
    ($fn_name: ident, $token: expr) => {
        fn $fn_name(tokens: Tokens) -> IResult<Tokens, Token> {
            nom::combinator::map(
                nom::combinator::verify(
                    nom::bytes::complete::take(1_usize),
                    |token: &$crate::lexer::token::Tokens<'_>| token[0] == $token,
                ),
                |token: $crate::lexer::token::Tokens<'_>| token[0].clone(),
            )(tokens)
        }
    };
}
