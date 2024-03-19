use core::fmt;
use std::fmt::Display;

pub struct Signature {
    params: Params,
}

impl Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = ", self.params)
    }
}

pub enum Params {
    Name(String),
    App(Box<Params>, Box<Params>),
}

impl Display for Params {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Params::Name(str) => write!(f, "{}", str),
            Params::App(a, b) => write!(f, "({}, {})", a, b),
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

fn alpha_token(input: &mut &str) -> PResult<String> {
    parse_while(char::is_alphabetic, "alphabetical token(s)")(input)
}

fn ws(input: &mut &str) -> PResult<()> {
    parse_while(char::is_whitespace, "whitespace token(s)")(input)
        .map(|_| ()).or_else(|_| Ok(()))
}

impl Signature {
    pub fn parse(input: &mut &str) -> PResult<Signature> {
        let params = Params::parse(input)?;

        ws(input)?;
        token("=")(input)?;

        Ok(Signature { params })
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
