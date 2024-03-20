use core::fmt;
use std::fmt::Display;

pub struct Definition {
    params: Params,
    expr: Expr
}

impl Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = ", self.params)
    }
}

pub enum Params {
    Name(String),
    App(Box<Params>, Box<Params>),
}

pub enum Expr {
    Name(String),
    App(Box<Expr>, Box<Expr>)
}

impl Display for Params {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Params::Name(str) => write!(f, "{}", str),
            Params::App(a, b) => write!(f, "({} {})", a, b),
        }
    }
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

fn any_token(input: &mut &str) -> PResult<String> {
    parse_while(|c| !c.is_whitespace(), "non-whitespace token(s)")(input)
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
        dbg!(&str);
        return Ok(str);
    }
}

fn alpha_token(input: &mut &str) -> PResult<String> {
    parse_while(char::is_alphabetic, "alphabetical token(s)")(input)
}

fn ws(input: &mut &str) -> PResult<()> {
    parse_while(char::is_whitespace, "whitespace token(s)")(input)
        .map(|_| ()).or_else(|_| Ok(()))
}

impl Definition {
    pub fn parse(input: &mut &str) -> PResult<Definition> {
        let params = Params::parse(input)?;

        ws(input)?;
        token("=")(input)?;
        ws(input)?;

        let expr = Expr::parse(input)?;

        Ok(Definition { params, expr })
    }
}

impl Params {
    pub fn parse(input: &mut &str) -> PResult<Params> {
        let mut next = None;

        loop {
            ws(input)?;

            let right;

            if let Ok(_) = token("(")(input) {
                right = Params::parse(input)?;
    
                ws(input)?;
    
                if let Err(_) = token(")")(input) {
                    return Err((input.to_owned(), "Expected closing parentheses".to_owned()));
                }
            } else if let Ok(token) = alpha_token(input) {
                right = Params::Name(token);
            } else {
                return next.ok_or((input.to_owned(), format!("Expected 'alpha' or '(', found none")));
            }

            if let Some(left) = next {
                next = Some(Params::App(Box::new(left), Box::new(right)));
            } else {
                next = Some(right);
            }
        }
    }
}

pub fn get_associativity(left: &Expr, right: &Expr) -> Associativity {
    let left = leftmost(left);
    let right = leftmost(right);

    if left == "*" && right == "+" { return Associativity::Left; }
    if left == "+" && right == "*" { return Associativity::Right; }
    if left == "`**`" && right == "`*`" { return Associativity::Right; }

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

impl Expr {

    pub fn parse(input: &mut &str) -> PResult<Expr> {
        let mut left = None;

        // Loop for handling left associativity
        loop {

            ws(input)?;

            let old_input = &input[0..];

            let Ok(tkn) = real_token(input) else {
                if let Some(left) = left {
                    return Ok(left);
                } else {
                    return Err((input.to_owned(), "Expected token".to_owned()));
                }
            };
            
            let infix = tkn.starts_with("`") && tkn.ends_with("`");

            let r;

            if tkn == ")" {
                *input = old_input;
                if let Some(left) = left {
                    return Ok(left);
                } else {
                    return Err((input.to_owned(), "Expected token".to_owned()));
                }
            } else if tkn == "(" {
                r = Expr::parse(input)?;
    
                ws(input)?;
    
                if let Err(_) = token(")")(input) {
                    return Err((input.to_owned(), "Expected closing parentheses".to_owned()));
                }
            } else {
                r = Expr::Name(tkn);
            }


            if let Some(l) = left {
                let order = get_associativity(&l, &r);

                println!("{} ||| {} ||| {:?}", l, r, order);

                if order == Associativity::Right {
                    // Right associative parsing is lazy: i.e. we wait for the
                    // tree to be built up from recursive Expr::parse calls.
                    *input = old_input;
                    // if infix {
                    //     left = Some(Expr::App(Box::new(Expr::parse(input)?), Box::new(l)));    
                    // } else {
                        left = Some(Expr::App(Box::new(l), Box::new(Expr::parse(input)?)));
                    // }
                } else {
                    // Left associative parsing is immediate: i.e. we parse
                    // tokens immediately and fill in with the `loop {}`.
                    if infix {
                        left = Some(Expr::App(Box::new(r), Box::new(l)));
                    } else {
                        left = Some(Expr::App(Box::new(l), Box::new(r)));
                    }
                }

            } else {
                left = Some(r);
            }
        }
    }

}