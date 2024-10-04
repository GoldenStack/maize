use std::collections::{HashMap, HashSet};

use daggy::{petgraph::{algo::dijkstra, visit::IntoNodeIdentifiers}, Dag, NodeIndex};

pub const BLOCK_OPERATOR: &'static str = ":";

pub const LINE_BREAK: char = '\n';

pub const OPEN_PAREN: char = '(';
pub const CLOSE_PAREN: char = ')';
pub const BACKTICK: char = '`';

const OPEN_PAREN_STR: &'static str = "(";
const CLOSE_PAREN_STR: &'static str = ")";
const BACKTICK_STR: &'static str = "`";

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
pub enum AST {
    Name(String),
    App(Box<AST>, Box<AST>)
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
impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AST::Name(str) => write!(f, "{}", str),
            AST::App(a, b) => write!(f, "({} {})", a, b),
        }
    }
}


/// A result from parsing an expression.
/// 
/// This contains either the parameterized type `<T>` or the remaining portion
/// of the input string along with an error that occurred.
pub type PResult<T> = Result<T, (String, PError)>;

/// An error that occurred while parsing.
#[derive(Debug, PartialEq)]
pub enum PError {
    /// Expected closing parentheses, but did not find them.
    ExpectedClosingParentheses,

    /// Did not expect closing parentheses, but found them.
    UnexpectedClosingParentheses,

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

    /// Creates a new reader at the start of the given string and with no
    /// minimum indentation.
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

    /// Returns a slice containing the unread portion of the source string.
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

    /// Creates an error at the current position in this reader.
    pub fn err<T>(&self, error: PError) -> PResult<T> {
        Err((self.slice().to_owned(), error))
    }
    
    /// Identical return value to [Reader::err], but replaces this reader with
    /// the other. This is intended for reverting to a previous state.
    pub fn err_reset<T>(&mut self, other: Reader<'a>, error: PError) -> PResult<T> {
        *self = other;
        Err((self.slice().to_owned(), error))
    }

}

/// Reads whitespace from a reader.
/// 
/// This can fail because the parser supports significant whitespace.
/// Specifically, the last line break before a token must have at least
/// [Reader::min_indent] characters of whitespace directly following it.
pub fn whitespace(input: &mut Reader) -> PResult<usize> {
    let mut has_break = false;
    let mut ws = 0;

    let start = input.clone();

    loop {
        if let Some(char) = input.peek(|i| i.next()).filter(|c| c.is_whitespace()) {
            if char.is_whitespace() {
                if char == LINE_BREAK {
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

/// Parses the next token from a reader.
/// This includes reading whitespace.
/// 
/// Specifically, a token is either:
/// - Exactly one `(` or `)`
/// - Any number of alphanumeric characters or `\``, `'`, or `.`
/// - Any number of non-alphanumeric characters or `\``, `'`, or `.`
pub fn next_token(input: &mut Reader) -> PResult<String> {
    whitespace(input)?;

    let group = |c: char| c.is_alphanumeric();
    let always_allowed = |c: char| c == BACKTICK || c == '\'' || c == '.';
    let never_allowed = |c: char| c == OPEN_PAREN || c == CLOSE_PAREN || c.is_whitespace();

    let Some(first) = input.next() else {
        return input.err(PError::EOF);
    };

    if first == OPEN_PAREN || first == CLOSE_PAREN {
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
            if never_allowed(char) {
                return Ok(str);
            }

            if !always_allowed(char) {
                match mode {
                    Some(mode) if mode != group(char) => return Ok(str),
                    _ => mode = Some(group(char)),
                }
            }

            str.push(char);
            input.next();
            continue;
        }
        return Ok(str);
    }
}

/// Returns the value of the leftmost node in an expression. This is possible
/// because the leftmost node must be an [Expr::Name].
pub fn leftmost(expr: &AST) -> &String {
    match expr {
        AST::Name(name) => name,
        AST::App(left, _) => leftmost(left)
    }
}

/// Returns the leftmost node in an expression, along with the depth of the
/// node.
/// 
/// The returned expression is guaranteed to be an [Expr::Name].
pub fn leftmost_wrapped(expr: &AST) -> (&AST, u64) {
    match expr {
        AST::Name(_) => (expr, 0),
        AST::App(left, _) => {
            let c = leftmost_wrapped(left);
            (c.0, c.1 + 1)
        },
    }
}

/// Stores context for parsing.
/// 
/// Each `Context` instance contains a partial ordering of operator precedences,
/// a list of associativities of operators with themselves, and a set containing
/// all expressions that are infix by default.
/// 
/// The context stores static information about parsing.
/// Cheap, copyable, and dynamic information is stored in the [Reader] instance.
pub struct Context {
    partial_order: Dag<String, (), u32>,
    self_referential: HashMap<String, Associativity>,
    infix: HashSet<String>,
}

impl Default for Context {
    fn default() -> Self {
        let mut context = Context::new();

        ["->", "^", "*", "\\"]
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

    /// Returns the numerical index, in the partial order graph, of the provided
    /// operator, or `None` if none exists.
    pub fn get_ident(&self, operator: &str) -> Option<NodeIndex> {
        self.partial_order.node_identifiers()
            .find(|i| self.partial_order[*i] == operator)
    }

    /// Returns the numerical index, in the partial order graph, of the provided
    /// operator, creating one if necessary.
    pub fn ensure_ident(&mut self, operator: &str) -> NodeIndex {
        self.get_ident(operator)
            .unwrap_or_else(|| self.partial_order.add_node(operator.to_owned()))
    }

    /// Returns whether or not there is a path from the left node to the right
    /// node using Dijkstra's algorithm.
    pub fn path_from(&self, left: NodeIndex, right: NodeIndex) -> bool {
        dijkstra(&self.partial_order, left, Some(right), |_| 1).contains_key(&right)
    }

    /// Indicates that the first operator has higher binding power than the
    /// second by adding an edge from the first to the second.
    /// 
    /// If there is already a path from the second operator to the first
    /// operator, a cycle would occur; in this case, the edge is not added and
    /// `false` is returned.
    pub fn gt(&mut self, first: &str, second: &str) -> bool {
        let f = self.ensure_ident(first);
        let s = self.ensure_ident(second);

        self.partial_order.update_edge(f, s, ()).is_ok()
    }

    /// Indicates that the second operator has higher binding power than the
    /// first by adding an edge from the second to the first.
    /// 
    /// This has identical semantics to [Context::gt], except with the arguments
    /// switched.
    pub fn lt(&mut self, first: &str, second: &str) -> bool {
        self.gt(second, first)
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

    /// Marks the provided operator as left associative (to itself).
    pub fn la(&mut self, op: &str) {
        self.self_referential.insert(op.to_owned(), Associativity::Left);
    }

    /// Marks the provided operator as right associative (to itself).
    pub fn ra(&mut self, op: &str) {
        self.self_referential.insert(op.to_owned(), Associativity::Right);
    }

    /// Marks the provided operator as an infix operator.
    pub fn infix(&mut self, op: &str) {
        self.infix.insert(op.to_owned());
    }

    /// Determines whether or not a given token is an infix operator.
    /// A token is infix if it's marked as infix in this context and is not
    /// placed between grave symbols (\`), or if it's a non-infix operator that
    /// is placed between grave symbols.
    pub fn is_infix(&self, op: &str) -> bool {
        // Operation is infix if grave symbols are used, or if it's marked as
        // infix. Grave symbols around an infix operation negate its infixation.
        if self.infix.contains(op) {
            return true;
        }

        if let Some(token) = de_infix(op) {
            return !self.infix.contains(token);
        }

        false
    }
    
}

/// Parses a basic "token". This is the most low-level part of the parser.
/// 
/// A basic "token" is either a name ([Expr::Name]) or a parenthesized
/// expression (e.g. `(1 + 2)`).
pub fn parse_basic(context: &Context, input: &mut Reader) -> PResult<AST> {
    let old_input = input.clone();
    let token = next_token(input)?;

    match token.as_str() {
        // Do not allow standalone infixes
        _ if context.is_infix(&token) => input.err_reset(old_input, PError::UnexpectedInfix(token.into())),

        // Do not allow standalone closing parentheses
        CLOSE_PAREN_STR => input.err_reset(old_input, PError::UnexpectedClosingParentheses) ,

        OPEN_PAREN_STR => { // Parse opening parentheses
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

            match next_token(input) {
                Ok(t) if t == CLOSE_PAREN_STR => Ok(infix),
                _ => input.err_reset(copy, PError::ExpectedClosingParentheses),
            }
        },

        // Otherwise it's a named token
        _ => Ok(AST::Name(de_infix_into(token))),
    }
}

/// Parses prefix expressions (e.g. `a b c`).
/// 
/// All prefix expressions bind stronger than infix expressions, and prefix
/// expressions have left associativity by default.
pub fn parse_prefix(context: &Context, mut left: AST, input: &mut Reader) -> PResult<AST> {
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
        left = AST::App(Box::new(left), Box::new(match order {
                Associativity::Right => parse_prefix(context, right, input)?,
                Associativity::Left => right
        }));
    }
}

/// Optionally parses an infix operator. This returns an option instead of an
/// error because at no place in the code are infix operators required.
pub fn read_infix(context: &Context, input: &mut Reader) -> Option<String> {
    let copy = input.clone();

    let token = next_token(input).ok().filter(|op| context.is_infix(op));

    // Revert progress
    if token.is_none() {
        *input = copy;
    }

    token
}

/// Counts the current indentation in a reader. This is defined as the number
/// of characters since the last line break.
pub fn indentation(input: &Reader) -> usize {
    let last_line_break = input.source[0..input.pos].rfind("\n") // Find the last line break
        .map(|n| n + 1) // Take the character after it, if there is one
        .unwrap_or(input.pos); // Otherwise, just take the length of the entire string

    input.source[last_line_break..input.pos].chars().count()
}

/// Parses an indented block in a reader.
/// 
/// An indented block is defined as the following:
/// The block operator, a colon (`:`), indicates the start of a block, which
/// involves significant whitespace.
/// 
/// A block, following the block operator, is a list of expressions that are
/// sequentially applied to the expression before the block operator.
/// The minimum indentation for a block operator is defined as the indentation
/// of the token immediately after the block operator.
/// ```Maize
/// a = Map: x = 2
///          y = 3
/// ```
/// The indentation of the above block is 9 characters, because the token `x` is
/// 9 characters after the start of a line.
/// Expressions within a block can be continued by including more whitespace
/// than is required in the block. For example, to split `1 + 2` between two
/// lines, indent the `+ 2` more than is required.
/// ```Maize
/// a = Map:
///      x = 1
///       + 2
///      y = 3
/// ```
/// A block is over when it reaches a token with less indentation that is
/// required for the block. For example, the following code is equal to the
/// expression `(A (B C) D)`.
/// ```Maize
/// A:
///  B:
///   C
///  D
/// E
/// ```
/// You can see `E` is not included in the `A` block because it has less
/// indentation than `B`.
/// 
/// The semantics of indentation in Maize are similar to Haskell, except with a
/// single symbolic operator instead of multiple different keywords. For
/// reference, see [Haskell/Indentation](https://en.wikibooks.org/wiki/Haskell/Indentation).
/// 
/// 
pub fn parse_indented_block(context: &Context, mut left: AST, input: &mut Reader) -> PResult<AST> {

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

        left = AST::App(Box::new(left), Box::new(parsed));
    }
}

/// Parses infix expressions (e.g. `1 + 2`).
/// Infix expressions bind the weakest out of any type of expression, and their
/// associativity, if relevant in an expression while parsing, must be defined
/// via [Context::gt] or [Context::lt], or a compile error will occur.
/// 
/// The block operator (`:`) is defined as an infix expression, and is handled
/// here. It introduces significant whitespace to the language.
pub fn parse_infix(context: &Context, mut left: AST, input: &mut Reader) -> PResult<AST> {

    loop {
        // If we can't find an infix operator, this means it's not an infix
        // expression, so we can exit.
        let Some(infix) = read_infix(context, input) else {
            return Ok(left);
        };

        if infix == BLOCK_OPERATOR {
            return parse_indented_block(context, left, input);
        }

        // Parse the left part and the infix part (but flipped, as is infix)
        let left_and_infix = AST::App(Box::new(AST::Name(de_infix_into(infix))), Box::new(left));

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
                    AST::Name(_) => parse_infix(context, all, input)?, // Should not happen
                    AST::App(l, r) => AST::App(l, Box::new(parse_infix(context, *r, input)?)),
                }
            }
            Associativity::Left => all
        };
    }
}

/// Parses an expression.
/// 
/// Parsing occurs down the parse tree:
/// 
/// [parse_infix] -> [parse_prefix] -> [parse_basic]
/// 
/// Lower-down parses bind stronger than more high-level ones.
pub fn parse(context: &Context, input: &mut Reader) -> PResult<AST> {
    // We replicate the first version of the tree here as it makes other parts
    // of the implementation much simpler.
    parse_infix(context, parse_prefix(context, parse_basic(context, input)?, input)?, input)
}

pub fn de_infix_into(op: String) -> String {
    de_infix(&op).map(str::to_owned).unwrap_or(op)
}

pub fn de_infix(op: &str) -> Option<&str> {
    const LEN: usize = BACKTICK_STR.len();
    let is_infix = op.starts_with(BACKTICK) && op.ends_with(BACKTICK) && op.len() >= 2 * LEN;

    if is_infix { Some(&op[LEN..op.len()-LEN]) } else { None }
}

#[cfg(test)]
mod tests {

use super::{AST, parse, Context, Reader};

fn name(name: &str) -> AST {
    AST::Name(name.to_owned())
}

fn app(app: AST, var: AST) -> AST {
    AST::App(Box::new(app), Box::new(var))
}

fn infix(infix: &str, left: AST, right: AST) -> AST {
    app(app(AST::Name(infix.into()), left), right)
}

fn raw_infix(infix_expr: &str, left: &str, right: &str) -> AST {
    infix(infix_expr.into(), name(left), name(right))
}

#[test]
pub fn test_parsing() {
    let context = Context::default();
    
    let input = "length (pos x y z) = (x ^ 2 + y ^ 2 + z ^ 2) ^ 0.5";
    let mut input = Reader::new(input);

    let expected = infix(
        "=",
        app(
            name("length"),
            [name("pos"), name("x"), name("y"), name("z")].into_iter().reduce(app).unwrap()
        ),
        infix(
            "^",
            infix(
                "+",
                raw_infix("^", "x", "2"),
                infix(
                    "+",
                    raw_infix("^", "y", "2"),
                    raw_infix("^", "z", "2")
                )
            ),
            name("0.5")
        )
    );
    
    assert_eq!(parse(&context, &mut input), Ok(expected));

}

#[test]
pub fn test_parsing_equality() {
    assert_parse(
        "a + c d e",
        "(`+` a) ((c d) e)",
    );

    assert_parse(
        "fst (a, b) = a",
        "(`=` (fst ((`,` a) b))) a",
    );

    assert_parse(
        "example :: Int -> Int -> List Int -> Map $ List String",
        "(`::` example) ((`->` Int) ((`->` Int) ((`->` (List Int)) ((`$` Map) (List String)))))"
    );
}

#[test]
pub fn test_block_operator() {
    assert_parse(
        "
        pos = (id Pos):
          x :: 5
          y :: 7",
        "((`=` pos) (((id Pos) ((`::` x) 5)) ((`::` y) 7)))"
    );

    assert_parse(r#"
        module Main where:

        seven = 5
         + 2
        true = 5 == 5

        map = Map:
            name = "oven"
            example :: Bool
            example = True"#,
    r#"(((((module Main) where) ((`=` seven) ((`+` 5) 2))) ((`=` true) ((`==` 5) 5))) ((`=` map) (((Map ((`=` name) "oven")) ((`::` example) Bool)) ((`=` example) True))))"#);

    assert_parse_rem(r#"
    A:
     B:
      C:
       D: H
          I
           J
          K
       D2:L
    Unread
    "#, 16,
    "(A (B ((C (((D H) (I J)) K)) (D2 L))))", 0);
}

fn assert_parse(first: &str, second: &str) {
    assert_parse_rem(first, 0, second, 0);
}

fn assert_parse_rem(first: &str, first_len: usize, second: &str, second_len: usize) {
    let context = Context::default();
    let (mut f, mut s) = (Reader::new(first), Reader::new(second));
    assert_eq!(parse(&context, &mut f), parse(&context, &mut s));

    assert_eq!(f.slice().len(), first_len);
    assert_eq!(s.slice().len(), second_len);
}

}