use core::fmt;
use std::fmt::{Debug, Display};

/// A code expression in Maize.
/// 
/// An expression is either a string literal ("Name"), or an application of one
/// expression to another ("App").
/// 
/// This is the entire initial AST. Yes, it's really this simple.
/// 
/// An expression is represented as a binary tree. This allows more well-known
/// algorithms and terms to be applied to it.
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Expr {
    Name(String),
    App(Box<Expr>, Box<Expr>)
}

/// Associativity rules.
/// 
/// Consider the expression `a b c d`.
/// Left associative parsing looks like `(((a b) c) d)`, while right associative
/// parsing looks like `(a (b (c d)))`.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Associativity {
    Left, Right
}

/// Displays an expression.
/// 
/// `Name` types just return the name, while `App` types return stringified
/// forms of the arguments, surrounded by paretheses.
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Name(str) => write!(f, "{}", str),
            Expr::App(a, b) => write!(f, "({} {})", a, b),
        }
    }
}
