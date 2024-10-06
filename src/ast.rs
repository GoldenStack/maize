use std::{collections::{HashMap, HashSet}, fmt::Display};

use daggy::{petgraph::{algo::dijkstra, visit::IntoNodeIdentifiers}, Dag, NodeIndex};

pub const OPEN_PAREN: char = '(';
pub const CLOSE_PAREN: char = ')';

const OPEN_PAREN_STR: &str = "(";
const CLOSE_PAREN_STR: &str = ")";

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

pub type Result<'a, T> = std::result::Result<T, (Reader<'a>, Error<'a>)>;

#[derive(Debug)]
pub enum Error<'a> {
    EOF,
    UnexpectedClosingParentheses,
    ExpectedClosingParentheses,
    UnexpectedInfix(&'a str),
    UndefinedAssociativity(String, String),
}

/// A reader of source code.
#[derive(Debug, Clone, Copy)]
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

    fn err_reset(&mut self, copy: Reader<'a>, err: Error<'a>) -> Result<'a, AST<'a>> {
        self.pos = copy.pos;
        Err((copy, err))
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

    pub fn assoc<'b>(&self, left: &'b AST, right: &'b AST) -> Result<'a, Associativity> {
        fn reduce<'a>(ast: &'a AST<'a>) -> &'a AST<'a> {
            if let AST::App(l, _) = ast {
                reduce(l)
            } else {
                ast
            }
        }

        if let AST::Var(lv) = reduce(left) {
            if let AST::Var(rv) = reduce(right) {
                if let Some(assoc) = self.context.get_assoc(lv, rv) {
                    Ok(assoc)
                } else {
                    match (self.context.is_infix(lv), self.context.is_infix(rv)) {
                        (false, false) => Ok(Associativity::Left),
                        (true, false) => Ok(Associativity::Right),
                        (false, true) => Ok(Associativity::Left),
                        (true, true) => Err((self.clone(), Error::UndefinedAssociativity(lv.to_string(), rv.to_string()))),
                    }
                }
            } else {
                Ok(Associativity::Right)
            }
        } else {
            if let AST::Var(_) = reduce(right) {
                Ok(Associativity::Right)
            } else {
                Ok(Associativity::Right)
            }
        }
    }

    /// Parses a basic expression (a token or an expression between
    /// parentheses).
    pub fn read_basic(&mut self) -> Result<'a, AST<'a>> {
        let start = self.clone();
        
        match self.token().ok_or_else(|| (start, Error::EOF))? {
            op if self.context.is_infix(op) => self.err_reset(start, Error::UnexpectedInfix(op)),
            CLOSE_PAREN_STR => self.err_reset(start, Error::UnexpectedClosingParentheses),
            OPEN_PAREN_STR => {
                let body = self.read()?;

                let start = self.clone();

                if matches!(self.token(), Some(CLOSE_PAREN_STR)) {
                    Ok(body)
                } else {
                    self.err_reset(start, Error::ExpectedClosingParentheses)
                }
            },
            "\\" => {
                let var = self.token().ok_or_else(|| (start, Error::EOF))?;

                Ok(AST::Lam(var, self.read().map(Box::new)?))
            },
            t => Ok(AST::Var(t))
        }
    }

    /// Parses prefix expressions (e.g. `a b c`).
    /// 
    /// All prefix expressions have higher precedence (bind stronger) than infix
    /// expressions, and prefix expressions have left associativity by default.
    pub fn read_prefix(&mut self, mut left: AST<'a>) -> Result<'a, AST<'a>> {
        loop {
            // If there isn't a basic expression to append, we can just return
            // as there's nothing more to be added.
            let Ok(right) = self.read_basic() else {
                return Ok(left);
            };
            
            let order = self.assoc(&left, &right)?;

            // If it's right associative relative to the left element we pair it
            // up with the element after it. Otherwise we ignore the element
            // after and allow following loop iterations to handle it.
            left = AST::App(Box::new(left), Box::new(match order {
                Associativity::Right => self.read_prefix(right)?,
                Associativity::Left => right
            }));
        }
    }

    pub fn read(&mut self) -> Result<'a, AST<'a>> {
        self.read_basic().and_then(|b| self.read_prefix(b))
    }
}

/// Stores context while parsing, including a partial order for operators,
/// defined associativity between the same operator twice, and whether or not an
/// operator is infix.
#[derive(Debug)]
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

    fn get_index(&self, op: &str) -> Option<NodeIndex> {
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

    /// Returns whether or not an operator is infix for this context.
    pub fn is_infix(&self, op: &str) -> bool {
        self.infix.contains(op)
    }

    /// Returns whether or not there is a path from the left node to the right
    /// node using Dijkstra's algorithm.
    fn path_from(&self, left: NodeIndex, right: NodeIndex) -> bool {
        dijkstra(&self.precedence, left, Some(right), |_| 1).contains_key(&right)
    }

    /// Determines the associativity between two tokens, returning nothing if
    /// there is no source specifically for this case that determines their
    /// associativity.
    /// 
    /// Cases with two identical operators will query the local associativity
    /// map to determine their self-referential associativity.
    /// 
    /// Otherwise, the internal directed acyclic graph will be referred to. If a
    /// path exists from the left operator to the right operator, left binds
    /// stronger than right, so we use left associativity. If a path exists from
    /// the right operator to the left operator, right binds stronger than left,
    /// so we use right associativity. The acyclicity of the graph ensures that
    /// these two conditions are mutually exclusive.
    pub fn get_assoc(&self, left: &str, right: &str) -> Option<Associativity> {
        if left == right {
            return self.associativity.get(left).copied();
        }

        let (id_l, id_r) = (self.get_index(left)?, self.get_index(right)?);
        
        if self.path_from(id_l, id_r) {
            Some(Associativity::Left)
        } else if self.path_from(id_r, id_l) {
            Some(Associativity::Right)
        } else {
            None
        }
    }

}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
}