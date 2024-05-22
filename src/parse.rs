use std::collections::{HashMap, HashSet};

use daggy::{petgraph::{algo::dijkstra, visit::IntoNodeIdentifiers}, Dag, NodeIndex};

use crate::ast::{Associativity, Expr};

/// A result from parsing an expression.
/// This contains either the parameterized type `<T>` or the remaining portion
/// of the input string along with an error that occurred.
type PResult<T> = Result<T, (String, PError)>;

/// An error that occurred while parsing.
#[derive(Debug, PartialEq)]
pub enum PError {
    /// Expected the given string at this location, but did not find it.
    Expected(String),

    /// Did not expect the given string at this location, but found it.
    Unexpected(String),

    /// Did not expect an infix expression at this location, but found one.
    UnexpectedInfix(String),

    /// Could not parse due to reaching the end of the string.
    EOF,

    /// Associativity between the two provided operators is undefined.
    UndefinedAssociativity(String, String),

    /// Expected a specified amount of indentation, but did not find it.
    ExpectedIndentation(usize)
}

/// A string reader, containing the source string, a position in the source
/// string, and the minimum indent size.
/// 
/// The reader stores dynamic information about parsing. It's meant to be cheap
/// and copyable.
/// More expensive, static information is stored in the [Context] instance.
#[derive(Debug)]
pub struct Reader<'a> {
    source: &'a str,
    pos: usize,
    min_indent: usize
}

impl<'a> Clone for Reader<'a> {
    fn clone(&self) -> Self {
        Reader {
            source: &self.source,
            pos: self.pos,
            min_indent: self.min_indent,
        }
    }
}

impl<'a> Reader<'a> {

    pub fn new(source: &'a str) -> Self {
        Reader {
            source,
            pos: 0,
            min_indent: 0
        }
    }

    pub fn min_indent(&self) -> usize {
        self.min_indent
    }

    pub fn set_min_indent(&mut self, min_indent: usize) {
        self.min_indent = min_indent;
    }

    pub fn slice(&self) -> &'a str {
        &self.source[self.pos..]
    }

    pub fn next(&mut self) -> Option<char> {
        let c = self.slice().chars().next()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    pub fn peek<T, F: Fn(&mut Reader) -> T>(&self, function: F) -> T {
        let copy = &mut self.clone();
        let result = function(copy);
        return result;
    }

    pub fn err<T>(&self, error: PError) -> PResult<T> {
        Err((self.slice().to_owned(), error))
    }
    
    pub fn err_reset<T>(&mut self, other: Reader<'a>, error: PError) -> PResult<T> {
        *self = other;
        Err((self.slice().to_owned(), error))
    }

}

fn whitespace(input: &mut Reader) -> PResult<usize> {
    let mut has_break = false;
    let mut ws = 0;

    let start = input.clone();

    loop {
        if let Some(char) = input.peek(|i| i.next()) {
            if char.is_whitespace() {
                if char == '\n' {
                    ws = 0;
                    has_break = true;
                } else {
                    ws += 1;
                }

                input.next();
                continue;
            }
        }

        if !has_break || ws >= input.min_indent() {
            return Ok(ws)
        } else {
            return input.err_reset(start, PError::ExpectedIndentation(input.min_indent()))
        }
    }
}

fn real_token(input: &mut Reader) -> PResult<String> {
    whitespace(input)?;

    let standalone = |c: char| c == '(' || c == ')';
    let group = |c: char| c.is_alphanumeric();
    let always_allowed = |c: char| c == '`' || c == '\'' || c == '.';
    let never_allowed = |c: char| c.is_whitespace();

    let Some(first) = input.next() else {
        return input.err(PError::EOF);
    };

    if never_allowed(first) {
        return input.err(PError::Unexpected(first.into()));
    }

    if standalone(first) {
        return Ok(first.into());
    }

    let mut str = String::from(first);

    let mut mode: Option<bool> = if !always_allowed(first) {
        Some(group(first))
    } else {
        None
    };

    loop {
        if let Some(char) = input.peek(|i| i.next()) {
            if standalone(char) || never_allowed(char) {
                return Ok(str);
            }

            if !always_allowed(char) {
                if let Some(mode) = mode {
                    if mode != group(char) {
                        return Ok(str);
                    }
                } else {
                    mode = Some(group(char));
                }
            }

            str.push(char);
            input.next();
            continue;
        }
        return Ok(str);
    }
}

pub fn leftmost(expr: &Expr) -> &String {
    match expr {
        Expr::App(a, _) => leftmost(a),
        Expr::Name(name) => name,
    }
}

pub struct Context {
    partial_order: Dag<String, (), u32>,
    self_referential: HashMap<String, Associativity>,
    infix: HashSet<String>,
}

impl Default for Context {
    fn default() -> Self {
        let mut context = Context::new();

        ["->", "^", "*"]
            .iter().for_each(|i| context.ra(i));

        context.gt("^", "*");
        context.gt("*", "+");
        context.lt("=", "+");
        context.lt("::", "->");
        context.lt("=", "$");
        context.lt("->", "$");
        context.gt(":", "=");
        context.gt(":", "::");
        context.gt("==", "=");

        ["=", "^", "*", "+", ",", "->", "::", "$", ":", "=="]
            .iter().for_each(|i| context.infix(i));

        context
    }
}

impl Context {

    pub fn new() -> Self {
        Context {
            partial_order: Dag::new(),
            self_referential: HashMap::new(),
            infix: HashSet::new()
        }
    }

    pub fn get_ident(&self, str: &str) -> Option<NodeIndex> {
        self.partial_order.node_identifiers()
            .find(|i| self.partial_order[*i] == str)
    }

    pub fn ensure_ident(&mut self, str: &str) -> NodeIndex {
        self.get_ident(str).unwrap_or_else(|| self.partial_order.add_node(str.to_owned()))
    }

    pub fn path_from(&self, left: NodeIndex, right: NodeIndex) -> bool {
        dijkstra(&self.partial_order, left, Some(right), |_| 1).contains_key(&right)
    }

    pub fn gt(&mut self, left: &str, right: &str) -> bool {
        let l = self.ensure_ident(left);
        let r = self.ensure_ident(right);

        self.partial_order.update_edge(l, r, ()).is_ok()
    }

    pub fn lt(&mut self, left: &str, right: &str) -> bool {
        self.gt(right, left)
    }

    /// Gets the defined associativity between two operators.
    /// If they're the same operator, it queries a local map to see what to
    /// return.
    /// 
    /// If they're not, it refers to the internal directed acyclic graph.
    /// If a path exists from left -> right, left binds stronger than right, so
    /// we use left associativity. If a path exists from right -> left, then
    /// right binds stronger than left, so we use right associativity.
    /// The acyclicity of the graph ensures these conditions are mutually
    /// exclusive.
    pub fn get_defined_associativity(&self, left: &str, right: &str) -> Option<Associativity> {
        if left == right {
            return self.self_referential.get(left).copied();
        }
        let (infix_l, infix_r) = (self.get_ident(left)?, self.get_ident(right)?);
        
        if self.path_from(infix_l, infix_r) {
            Some(Associativity::Left)
        } else if self.path_from(infix_r, infix_l) {
            Some(Associativity::Right)
        } else {
            None
        }
    }

    /// Similar behaviour to Parser::get_defined_associativity except with
    /// default values: prefix expressions bind stronger than infix expressions,
    /// prefix operators bind to the left by default, and infix expressions have
    /// no default associativity.
    pub fn get_associativity(&self, left: &str, right: &str, input: &Reader) -> PResult<Associativity> {
        if let Some(defined) = self.get_defined_associativity(left, right) {
            return Ok(defined);
        }

        match (self.is_infix(left), self.is_infix(right)) {
            (true, false) => Ok(Associativity::Right),
            (false, true) => Ok(Associativity::Left),
            (false, false) => Ok(Associativity::Left),
            (true, true) => input.err(PError::UndefinedAssociativity(left.into(), right.into())),
        }
    }

    pub fn la(&mut self, op: &str) {
        self.self_referential.insert(op.to_owned(), Associativity::Left);
    }

    pub fn ra(&mut self, op: &str) {
        self.self_referential.insert(op.to_owned(), Associativity::Right);
    }

    pub fn infix(&mut self, op: &str) {
        self.infix.insert(op.to_owned());
    }

    pub fn is_infix(&self, op: &str) -> bool {
        // Operation is infix if grave symbols are used, or if it's marked as
        // infix. Grave symbols around an infix operation negate its infixation.
        if self.infix.contains(op) {
            return true;
        }

        if op.starts_with("`") && op.ends_with("`") && op.len() > 1 {
            return !self.infix.contains(&op[1..op.len()-1]);
        }

        false
    }
    
}

pub fn parse_basic(context: &Context, input: &mut Reader) -> PResult<Expr> {
    let old_input = input.clone();
    let token = real_token(input)?;

    match token.as_str() {
        // Do not allow standalone infixes
        _ if context.is_infix(&token) => input.err_reset(old_input, PError::UnexpectedInfix(token.into())),

        // Do not allow standalone closing parentheses
        ")" => input.err_reset(old_input, PError::Unexpected(")".into())),

        "(" => { // Parse opening parentheses
            // Allow any expression within them
            let infix = match parse(context, input) {
                Ok(ok) => ok,
                Err(err) => {
                    *input = old_input;
                    return Err(err);
                }
            };

            // Require closing parentheses
            let copy = input.clone();

            match real_token(input) {
                Ok(t) if t == ")" => Ok(infix),
                _ => input.err_reset(copy, PError::Expected(")".into())),
            }
        },

        // Otherwise it's a named token
        _ => Ok(Expr::Name(de_infix(token))),
    }
}

pub fn parse_prefix(context: &Context, mut left: Expr, input: &mut Reader) -> PResult<Expr> {
    loop {
        // If there isn't another basic expression, this means it's not a
        // prefix operation, so we can exit.
        let Ok(right) = parse_basic(context, input) else {
            return Ok(left);
        };

        let order = context.get_associativity(leftmost(&left), leftmost(&right), input)?;

        // If it's right associative relative to the left element we pair it
        // up with the element after it. Otherwise we ignore the element
        // after and allow following loop iterations to handle it.
        left = Expr::App(Box::new(left), Box::new(match order {
                Associativity::Right => parse_prefix(context, right, input)?,
                Associativity::Left => right
        }));
    }
}

fn read_infix(context: &Context, input: &mut Reader) -> Option<String> {
    let copy = input.clone();

    let token = real_token(input).ok().filter(|op| context.is_infix(op));

    // Revert progress
    if token.is_none() {
        *input = copy;
    }

    token
}

pub fn indentation(input: &Reader) -> usize {
    let last_line_break = input.source[0..input.pos].rfind("\n") // Find a line break
        .map(|n| n + 1) // Take the character after it, if there is one
        .unwrap_or(input.pos); // Otherwise, just take the length of the entire string

    input.pos - last_line_break
}

pub fn parse_indented_block(context: &Context, mut left: Expr, input: &mut Reader) -> PResult<Expr> {

    let mut copy = input.clone();
    whitespace(&mut copy)?;
    let indentation = indentation(&copy);

    let previous_indentation = input.min_indent();

    loop {
        input.set_min_indent(indentation);
        if whitespace(input).is_err() {
            return Ok(left);
        }

        input.set_min_indent(indentation + 1);
        let parsed = match parse(context, input) {
            Ok(expr) => expr,
            Err(err) => {
                return match err.1 {
                    PError::EOF | PError::ExpectedIndentation(_) => Ok(left),
                    _ => Err(err)
                };
            }
        };

        input.set_min_indent(previous_indentation);

        left = Expr::App(Box::new(left), Box::new(parsed));
    }
}

pub fn parse_infix(context: &Context, mut left: Expr, input: &mut Reader) -> PResult<Expr> {

    loop {
        // If we can't find an infix operator, this means it's not an infix
        // expression, so we can exit.
        let Some(infix) = read_infix(context, input) else {
            return Ok(left);
        };

        if infix == ":" {
            return parse_indented_block(context, left, input);
        }

        // Parse the left part and the infix part (but flipped, as is infix)
        let left_and_infix = Expr::App(Box::new(Expr::Name(de_infix(infix))), Box::new(left));

        // Append the right half
        let all = parse_prefix(context, left_and_infix, input)?;

        // Try to find another infix
        let Some(next) = input.peek(|input| read_infix(context, input)) else {
            return Ok(all);
        };

        let order = context.get_associativity(leftmost(&all), &next, input)?;

        // If there's another infix with right associativity relative to the
        // current expression, let it take the right element from this one.
        // Otherwise, fall back and allow following loop iterations to
        // to handle it.
        left = match order {
            Associativity::Right => {
                match all {
                    Expr::Name(_) => parse_infix(context, all, input)?, // Should not happen
                    Expr::App(l, r) => Expr::App(l, Box::new(parse_infix(context, *r, input)?)),
                }
            }
            Associativity::Left => all
        };
    }
}

pub fn parse(context: &Context, input: &mut Reader) -> PResult<Expr> {
    // Parsing occurs down the parse tree:
    // Infix -> Prefix -> Basic

    // We replicate the first version of that here as it makes other parts
    // of the implementation much simpler.
    parse_infix(context, parse_prefix(context, parse_basic(context, input)?, input)?, input)
}

pub fn de_infix(op: String) -> String {
    if op.starts_with("`") && op.ends_with("`") && op.len() > 1 {
        op[1..op.len()-1].to_owned()
    } else {
        op
    }
}