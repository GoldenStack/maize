use core::fmt;
use std::fmt::{Debug, Display};

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Name(String),
    App(Box<Expr>, Box<Expr>)
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Name(str) => write!(f, "{}", str),
            Expr::App(a, b) => write!(f, "({} {})", a, b),
        }
    }
}

type PResult<T> = Result<T, (String, String)>;

fn parse_char(input: &mut &str) -> Option<char> {
    let c = input.chars().next()?;
    let offset = c.len_utf8();
    *input = &input[offset..];
    Some(c)
}

fn peek<T>(function: fn(&mut &str) -> T, input: &mut &str) -> T {
    let old_input = &input[0..];

    let token = function(input);

    *input = old_input;
    return token;
}

fn parse_while(predicate: fn(char) -> bool, input: &mut &str) -> Option<String> {
    let mut str = String::new();

    loop {
        if let Some(char) = input.chars().next() {
            if predicate(char) {
                str.push(char);
                parse_char(input);
                continue;
            }
        }

        return if str.is_empty() { None } else { Some(str) };
    }
}

fn require(token: &str, input: &mut &str) -> PResult<()> {
    ws(input)?;
    if input.starts_with(token) {
        *input = &input[token.len()..];
        Ok(())
    } else {
        Err((input.to_string(), format!("Expected '{}'", token)))
    }
}

fn real_token(input: &mut &str) -> PResult<String> {
    ws(input)?;

    let predicate: fn(char) -> bool = |c| c.is_whitespace() || c == '(' || c == ')';

    let Some(first) = parse_char(input) else {
        return Err((input.to_string(), format!("Expected a token, found none")));
    };

    if predicate(first) {
        return Ok(first.into());
    }

    let mut str = String::new();
    str.push(first);

    loop {
        if let Some(char) = input.chars().next() {
            if !predicate(char) {
                str.push(char);
                parse_char(input);
                continue;
            }
        }
        return Ok(str);
    }
}

fn ws(input: &mut &str) -> PResult<()> {
    parse_while(char::is_whitespace, input);
    Ok(())
}

pub fn get_associativity(left: &String, right: &String) -> Associativity {

    if left == "`=`" { return Associativity::Right; }
    if right == "`=`" { return Associativity::Left; }

    if left == "`->`" && right == "`->`" { return Associativity::Right; }

    if left == "`^`" && right == "`^`" { return Associativity::Right; }
    if left == "`*`" && right == "`*`" { return Associativity::Right; }

    if left == "`^`" && right == "`+`" { return Associativity::Left; }
    if left == "`+`" && right == "`^`" { return Associativity::Right; }

    if left == "*" && right == "+" { return Associativity::Left; }
    if left == "+" && right == "*" { return Associativity::Right; }

    Associativity::Left
}

#[derive(PartialEq, Eq, Debug)]
pub enum Associativity {
    Left, Right
}

pub fn leftmost(expr: &Expr) -> &String {
    match expr {
        Expr::App(a, _) => leftmost(a),
        Expr::Name(name) => name, 
    }
}

fn is_infix(op: &String) -> bool {
    op.starts_with("`") && op.ends_with("`")
}


impl Expr {

    pub fn name(name: &str) -> Expr {
        Expr::Name(name.to_owned())
    }

    pub fn app(app: Expr, var: Expr) -> Expr {
        Expr::App(Box::new(app), Box::new(var))
    }

    pub fn infix(infix: String, left: Expr, right: Expr) -> Expr {
        Expr::app(Expr::app(Expr::Name(infix), left), right)
    }

    pub fn raw_infix(infix: &str, left: &str, right: &str) -> Expr {
        Expr::infix(infix.into(), Expr::name(left), Expr::name(right))
    }

    pub fn fold<const N: usize>(app: Expr, vars: [Expr; N]) -> Expr {
        let mut app = app;
        for var in vars {
            app = Expr::app(app, var);
        }
        app
    }

    pub fn parse_basic(input: &mut &str) -> PResult<Expr> {
        let old_input = &input[0..];
        let token = real_token(input)?;

        if is_infix(&token) {
            *input = old_input;
            return Err((input.to_owned(), format!("Expected operation, found infix '{}'", token)));
        }

        if token == ")" {
            *input = old_input;
            return Err((input.to_owned(), "Found closing parentheses".to_owned()));
        }

        if token == "(" {
            let infix = Expr::parse_infix(None, input)?;

            if let Err(_) = require(")", input) {
                return Err((input.to_owned(), "Expected closing parentheses".to_owned()));
            }

            return Ok(infix);
        }
        

        Ok(Expr::Name(token))
    }
 
    pub fn parse_prefix(left: Option<Expr>, input: &mut &str) -> PResult<Expr> {
        let mut left = match left {
            Some(expr) => expr,
            None => Expr::parse_basic(input)?,
        };

        loop {
            let Ok(right) = Expr::parse_basic(input) else {
                return Ok(left);
            };

            let order = get_associativity(leftmost(&left), leftmost(&right));

            left = Expr::app(left, match order {
                Associativity::Right => Expr::parse_prefix(Some(right), input)?,
                Associativity::Left => right
            });
        }
    }

    pub fn parse_infix(left: Option<Expr>, input: &mut &str) -> PResult<Expr> {
        let mut left = match left {
            Some(expr) => expr,
            None => Expr::parse_prefix(None, input)?,
        };

        loop {
            let old_input = &input[0..];

            let Some(infix) = real_token(input).ok().filter(is_infix).filter(|t| t != ")") else {
                *input = old_input;
                return Ok(left);
            };

            let left_infix = Expr::app(Expr::Name(infix), left);

            let all = Expr::parse_prefix(Some(left_infix), input)?;

            let Some(next) = peek(real_token, input).ok().filter(is_infix) else {
                return Ok(all);
            };

            let order = get_associativity(leftmost(&all), &next);

            left = match order {
                Associativity::Right => {
                    match all {
                        Expr::Name(_) => Expr::parse_infix(Some(all), input)?, // Should not happen
                        Expr::App(l, r) => Expr::App(l, Box::new(Expr::parse_infix(Some(*r), input)?)),
                    }
                }
                Associativity::Left => all
            };
        }
    }

    pub fn parse(input: &mut &str) -> PResult<Expr> {
        Expr::parse_infix(None, input)
    }

}