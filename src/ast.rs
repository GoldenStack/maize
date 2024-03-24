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

fn peek<T>(function: fn(&mut &str) -> T, input: &&str) -> T {
    let copy = &mut &input[0..];
    return function(copy);
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

        match token.as_str() {
            _ if is_infix(&token) => { // Do not allow standalone infixes
                *input = old_input;
                return Err((input.to_owned(), format!("Expected operation, found infix '{}'", token)));
            },
            ")" => { // Do not allow standalone closing parentheses
                *input = old_input;
                return Err((input.to_owned(), "Found closing parentheses".to_owned()));
            },
            "(" => { // Parse opening parentheses
                // Allow any expression within them
                let infix = Expr::parse(input)?;
    
                ws(input)?;
                // Require closing parentheses
                if let Err(_) = require(")", input) {
                    return Err((input.to_owned(), "Expected closing parentheses".to_owned()));
                }
    
                return Ok(infix);
            },
            _ => Ok(Expr::Name(token)), // Otherwise it's a 
        }
    }
 
    pub fn parse_prefix(mut left: Expr, input: &mut &str) -> PResult<Expr> {
        loop {
            // If there isn't another basic expression, this means it's not a
            // prefix operation, so we can exit.
            let Ok(right) = Expr::parse_basic(input) else {
                return Ok(left);
            };

            let order = get_associativity(leftmost(&left), leftmost(&right));

            // If it's right associative relative to the left element we pair it
            // up with the element after it. Otherwise we ignore the element
            // after and allow following loop iterations to handle it.
            left = Expr::app(left, match order {
                Associativity::Right => Expr::parse_prefix(right, input)?,
                Associativity::Left => right
            });
        }
    }

    pub fn parse_infix(mut left: Expr, input: &mut &str) -> PResult<Expr> {

        fn peek_infix(input: &mut &str) -> Option<String> {
            peek(real_token, input).ok().filter(is_infix).filter(|t| t != ")")
        }

        loop {
            // If we can't find an infix operator, this means it's not an infix
            // expression, so we can exit.
            let Some(infix) = peek_infix(input) else {
                return Ok(left);
            };
            real_token(input)?;

            // Parse the left part and the infix part (but flipped, as is infix)
            let left_and_infix = Expr::app(Expr::Name(infix), left);

            // Append the right half 
            let all = Expr::parse_prefix(left_and_infix, input)?;

            // Try to find another infix
            let Some(next) = peek_infix(input) else {
                return Ok(all);
            };

            let order = get_associativity(leftmost(&all), &next);

            // If there's another infix with right associativity relative to the
            // current expression, let it take the right element from this one.
            // Otherwise, fall back and allow following loop iterations to
            // to handle it.
            left = match order {
                Associativity::Right => {
                    match all {
                        Expr::Name(_) => Expr::parse_infix(all, input)?, // Should not happen
                        Expr::App(l, r) => Expr::App(l, Box::new(Expr::parse_infix(*r, input)?)),
                    }
                }
                Associativity::Left => all
            };
        }
    }

    pub fn parse(input: &mut &str) -> PResult<Expr> {
        // Parsing occurs down the parse tree:
        // Infix -> Prefix -> Basic

        // We replicate the first version of that here as it makes other parts
        // of the implementation much simpler.
        Expr::parse_infix(Expr::parse_prefix(Expr::parse_basic(input)?, input)?, input)
    }

}