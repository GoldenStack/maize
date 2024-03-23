use core::fmt;
use std::fmt::{Debug, Display};

#[derive(Debug)]
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

fn peek_token(input: &mut &str) -> Option<String> {
    let old_input = &input[0..];

    let _ = ws(input);
    let token = real_token(input);

    *input = old_input;
    return token.ok();
}

fn parse_while(predicate: fn(char) -> bool, name: &'static str) -> impl Fn(&mut &str) -> PResult<String> {
    move |input| {
        let mut str = String::new();

        loop {
            if let Some(char) = input.chars().next() {
                if predicate(char) {
                    str.push(char);
                    parse_char(input);
                    continue;
                }
            }

            if str.is_empty() {
                return Err((input.to_string(), format!("Expected '{}', found none", name)));
            } else {
                return Ok(str);
            }
        }
    }
}

fn token(token: &'static str) -> impl Fn(&mut &str) -> PResult<()> {
    move |input| {
        if input.starts_with(token) {
            parse_char(input);
            Ok(())
        } else {
            Err((input.to_string(), format!("Expected '{}'", token)))
        }
    }
}

fn real_token(input: &mut &str) -> PResult<String> {

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
    parse_while(char::is_whitespace, "whitespace token(s)")(input)
        .map(|_| ()).or_else(|_| Ok(()))
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

    pub fn app(app: Expr, var: Expr) -> Expr {
        Expr::App(Box::new(app), Box::new(var))
    }

    pub fn parse_basic(input: &mut &str) -> PResult<Expr> {
        let old_input = &input[0..];
        let tk = real_token(input)?;

        if is_infix(&tk) {
            *input = old_input;
            return Err((input.to_owned(), format!("Expected operation, found infix '{}'", tk)));
        }

        // if tk == ")" {
        //     *input = old_input;
        //     return Err((input.to_owned(), "Found closing parentheses".to_owned()));
        // }

        if tk == "(" {
            ws(input)?;
            let infix = Expr::parse_infix(input)?;

            ws(input)?;

            if let Err(_) = token(")")(input) {
                return Err((input.to_owned(), "Expected closing parentheses".to_owned()));
            }

            return Ok(infix);
        }
        

        Ok(Expr::Name(tk))
    }

    pub fn parse_prefix(input: &mut &str) -> PResult<Expr> {
        ws(input)?;
        let mut left = Expr::parse_basic(input)?;

        loop {
            ws(input)?;

            let old_input = &input[0..];

            if peek_token(input).filter(|t| t == ")").is_some() { return Ok(left); }

            let Ok(right) = Expr::parse_basic(input) else {
                return Ok(left);
            };
            ws(input)?;

            let order = get_associativity(leftmost(&left), leftmost(&right));

            let r = if order == Associativity::Right {
                *input = old_input;
                Expr::parse_prefix(input)?
            } else {
                right
            };

            left = Expr::App(Box::new(left), Box::new(r));
        }
    }

    pub fn parse_infix(input: &mut &str) -> PResult<Expr> {
        ws(input)?;
        let mut left = Expr::parse_prefix(input)?;

        loop {
            ws(input)?;

            if peek_token(input).filter(|t| t == ")").is_some() { return Ok(left); }

            let Some(infix) = real_token(input).ok().filter(is_infix) else {
                return Ok(left);
            };

            ws(input)?;

            let old_input = &input[0..];

            let right = Expr::parse_prefix(input)?;
            ws(input)?;

            let Some(next) = peek_token(input).filter(is_infix) else {
                return Ok(Expr::app(Expr::app(Expr::Name(infix), left), right));
            };

            let order = get_associativity(&infix, &next);

            let r = if order == Associativity::Right {
                *input = old_input;
                Expr::parse_infix(input)?
            } else {
                right
            };

            left = Expr::app(Expr::app(Expr::Name(infix), left), r);
        }
    }

}