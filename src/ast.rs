use core::fmt;
use std::fmt::{Debug, Display};

/// A general code expression.
/// This is the entire initial AST. Yes, it's really this simple.
#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Name(String),
    App(Box<Expr>, Box<Expr>)
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Associativity {
    Left, Right
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Name(str) => write!(f, "{}", str),
            Expr::App(a, b) => write!(f, "({} {})", a, b),
        }
    }
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

}