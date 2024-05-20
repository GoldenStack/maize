use core::fmt;
use std::fmt::{Debug, Display};

/// A general code expression.
/// This is the entire initial AST. Yes, it's really this simple.
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Expr {
    Name(String),
    App(Box<Expr>, Box<Expr>)
}

/// Associativity rules.
/// Consider the expression `a b c d`.
/// Left associative parsing looks like `(((a b) c) d)`, while right associative
/// parsing looks like `(a (b (c d)))`.
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
