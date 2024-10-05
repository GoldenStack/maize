use std::fmt::Display;

pub const OPEN_PAREN: char = '(';
pub const CLOSE_PAREN: char = ')';

/// The abstract syntax tree of Maize code.
/// 
/// This is equivalent to the untyped lambda calculus, as Maize is a simple
/// abstraction over lambda calculus itself.
/// 
/// The syntax tree being this simple allows it to essentially be treated as a
/// binary tree, allowing more well-known terms to be applied to it.
pub enum AST<'a> {
    Var(&'a str),
    Lam(&'a str, Box<AST<'a>>),
    App(Box<AST<'a>>, Box<AST<'a>>),
}

impl<'a> Display for AST<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Var(var) => write!(f, "{var}"),
            AST::Lam(var, body) => write!(f, "(\\{var} -> {body})"),
            AST::App(l, r) => write!(f, "({l} {r})"),
        }
    }
}

/// A reader of source code, storing simple, copyable information.
pub struct Reader<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> Reader<'a> {
    
    /// Creates a new reader from the start of the provided string.
    pub fn new(src: &'a str) -> Self {
        Reader {
            src, pos: 0
        }
    }

    /// Returns a slice containing the unread portion of the source string.
    pub fn slice(&self) -> &'a str {
        &self.src[self.pos..]
    }

    /// Returns the next character in the source string without modifying this
    /// reader.
    pub fn peek(&self) -> Option<char> {
        self.slice().chars().next()
    }

    /// Returns the next character in this string, if possible.
    pub fn next(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    /// Reads a token from this reader.
    /// 
    /// A token consists of either [OPEN_PAREN], [CLOSE_PAREN], any number of
    /// alphabetic characters, or any number of non-alphabetic characters.
    /// 
    /// Whitespace is read prior to actually reading each token.
    pub fn token(&mut self) -> Option<&'a str> {
        // Read whitespace
        while matches!(self.peek(), Some(c) if c.is_whitespace()) {
            _ = self.next();
        }

        const STANDALONE: fn(char) -> bool = |c| c == OPEN_PAREN || c == CLOSE_PAREN;
        const ILLEGAL: fn(char) -> bool = |c| c.is_whitespace() || STANDALONE(c);

        let start = self.pos;
        let first = self.next()?;

        // Some chararcters are completely standalone, so only one of them is allowed.
        if STANDALONE(first) {
            return Some(&self.src[start..self.pos]);
        }

        // Read legal characters that have the same alphanumeric status as the first character.
        while matches!(self.peek(), Some(c) if !ILLEGAL(c) && c.is_alphabetic() == first.is_alphabetic()) {
            _ = self.next();
        }

        return Some(&self.src[start..self.pos]);
    }
}
