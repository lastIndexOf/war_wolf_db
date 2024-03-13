use nom::{
    branch::alt,
    character::complete::{char, digit1, space0},
    combinator::map_res,
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

enum Expr {
    Num(u32),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
}

fn parse_calc_expr(s: &str) -> Result<u32, Box<dyn std::error::Error>> {
    #[inline]
    fn execute_expr(expr: Expr) -> Result<u32, String> {
        match expr {
            Expr::Num(num) => Ok(num),
            Expr::Add(left, right) => Ok(execute_expr(*left)? + execute_expr(*right)?),
            Expr::Sub(left, right) => Ok(execute_expr(*left)? - execute_expr(*right)?),
            Expr::Mul(left, right) => Ok(execute_expr(*left)? * execute_expr(*right)?),
            Expr::Div(left, right) => {
                let dived = execute_expr(*right)?;

                if dived == 0 {
                    return Err("divided by zero".into());
                }

                Ok(execute_expr(*left)? / dived)
            }
            Expr::Pow(left, right) => Ok(execute_expr(*left)?.pow(execute_expr(*right)?)),
        }
    }

    match parse_add_and_sub_expr(s) {
        Ok((_, expr)) => Ok(execute_expr(expr)?),
        Err(e) => Err(e.to_string().into()),
    }
}

fn parse_add_and_sub_expr(s: &str) -> IResult<&str, Expr> {
    let (input, expr) = parse_mul_and_div_expr(s)?;
    let (input, exprs) =
        many0(tuple((alt((char('+'), char('-'))), parse_mul_and_div_expr)))(input)?;

    Ok((input, parse_exprs(expr, exprs)))
}

fn parse_mul_and_div_expr(s: &str) -> IResult<&str, Expr> {
    let (input, expr) = parse_pow_expr(s)?;
    let (input, exprs) = many0(tuple((alt((char('*'), char('/'))), parse_pow_expr)))(input)?;

    Ok((input, parse_exprs(expr, exprs)))
}

fn parse_pow_expr(s: &str) -> IResult<&str, Expr> {
    let (input, expr) = parse_number_expr(s)?;
    let (input, exprs) = many0(tuple(((char('^')), parse_number_expr)))(input)?;

    Ok((input, parse_exprs(expr, exprs)))
}

fn parse_number_expr(s: &str) -> IResult<&str, Expr> {
    alt((
        delimited(
            space0,
            delimited(char('('), parse_add_and_sub_expr, char(')')),
            space0,
        ),
        parse_number,
    ))(s)
}

fn parse_exprs(num: Expr, exprs: Vec<(char, Expr)>) -> Expr {
    exprs.into_iter().fold(num, |acc, (op, expr)| match op {
        '+' => Expr::Add(Box::new(acc), Box::new(expr)),
        '-' => Expr::Sub(Box::new(acc), Box::new(expr)),
        '*' => Expr::Mul(Box::new(acc), Box::new(expr)),
        '/' => Expr::Div(Box::new(acc), Box::new(expr)),
        '^' => Expr::Pow(Box::new(acc), Box::new(expr)),
        _ => unimplemented!(),
    })
}

fn parse_number(s: &str) -> IResult<&str, Expr> {
    map_res(delimited(space0, digit1, space0), |ele: &str| {
        u32::from_str_radix(ele, 10).map(Expr::Num)
    })(s)
}

#[test]
fn test_calc_expr_parser() {
    assert_eq!(parse_calc_expr("1 + 1").unwrap(), 2);
    assert_eq!(parse_calc_expr("4 - 2").unwrap(), 2);
    assert_eq!(parse_calc_expr("2 * 3").unwrap(), 6);
    assert_eq!(parse_calc_expr("6 / 3").unwrap(), 2);
    assert_eq!(parse_calc_expr("2 + 3 * 4").unwrap(), 14);
    assert_eq!(parse_calc_expr("2 * 3 + 10 / 5").unwrap(), 8);
    assert_eq!(parse_calc_expr("(2 + 3) * 4").unwrap(), 20);
    assert_eq!(parse_calc_expr("2 ^ 3").unwrap(), 8);
    assert_eq!(parse_calc_expr("2 + 3 ^ 2").unwrap(), 11);
    assert_eq!(parse_calc_expr("(2 + 2) ^ 2").unwrap(), 16);
    assert_eq!(parse_calc_expr("2 + 2 * 2 ^ 3").unwrap(), 18);
    assert_eq!(parse_calc_expr("3 * (2 + 2) ^ 2 + 1").unwrap(), 49);
    assert!(parse_calc_expr("").is_err());
    assert!(parse_calc_expr("1 / 0").is_err());
    assert!(parse_calc_expr("invalid expression").is_err());
}
