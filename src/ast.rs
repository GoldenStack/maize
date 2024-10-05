use std::{collections::{HashMap, HashSet}, fmt::Display};

use daggy::{petgraph::visit::IntoNodeIdentifiers, Dag, NodeIndex};

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

/// A reader of source code. 
pub struct Reader<'a> {
    context: &'a Context<'a>,
    src: &'a str,
    pos: usize,
}

impl<'a> Reader<'a> {
    
    /// Creates a new reader from the start of the provided string.
    pub fn new(context: &'a Context<'a>, src: &'a str) -> Self {
        Reader {
            context, src, pos: 0
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

/// Stores context while parsing, including a partial order for operators,
/// defined associativity between the same operator twice, and whether or not an
/// operator is infix.
pub struct Context<'a> {
    precedence: Dag<&'a str, (), u32>,
    associativity: HashMap<&'a str, Associativity>,
    infix: HashSet<&'a str>,
}

impl<'a> Context<'a> {

    pub fn new() -> Self {
        Context {
            precedence: Dag::new(),
            associativity: HashMap::new(),
            infix: HashSet::new(),
        }
    }

    fn get_index(&self, op: &'a str) -> Option<NodeIndex> {
        self.precedence.node_identifiers()
            .find(|i| self.precedence[*i] == op)
    }

    fn get_or_create_index(&mut self, op: &'a str) -> NodeIndex {
        self.get_index(op).unwrap_or_else(|| self.precedence.add_node(op))
    }

    /// Creates an edge from a to b, indicating that a has a higher precedence
    /// (or binding power) than b.
    /// 
    /// This will return false if the created precedence rule would create a
    /// cycle within the acyclic graph, which is not desired as a cycle would
    /// remove the graph's ability to act as a partial order.
    pub fn gt(&mut self, a: &'a str, b: &'a str) -> bool {
        let a = self.get_or_create_index(a);
        let b = self.get_or_create_index(b);

        self.precedence.update_edge(a, b, ()).is_ok()
    }

    /// Identical semantics to [Context::gt] except with the operators switched.
    pub fn lt(&mut self, a: &'a str, b: &'a str) -> bool {
        self.gt(b, a)
    }

    /// Indicates that an operator has left associativity with itself.
    pub fn la(&mut self, op: &'a str) {
        self.associativity.insert(op, Associativity::Left);
    }

    /// Indicates that an operator has right associativity with itself.
    pub fn ra(&mut self, op: &'a str) {
        self.associativity.insert(op, Associativity::Right);
    }

    /// Indicates that an operator is an infix operator.
    pub fn infix(&mut self, op: &'a str) {
        self.infix.insert(op);
    }

}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
}