use nom::{
    bytes::streaming::{tag, take_while_m_n},
    combinator::map_res,
    sequence::Tuple,
    IResult,
};

fn parse_hex_to_rgb(s: &str) -> IResult<&str, (u8, u8, u8)> {
    fn parser(s: &str) -> IResult<&str, u8> {
        map_res(take_while_m_n(2, 2, |c: char| c.is_digit(16)), |s| {
            u8::from_str_radix(s, 16)
        })(s)
    }

    let (color_str, _) = tag("#")(s)?;

    (parser, parser, parser).parse(color_str)
}

#[test]
fn test_color_parser() {
    assert_eq!(parse_hex_to_rgb("#2F14DF"), Ok(("", (47, 20, 223))));
}
