use std::collections::{HashMap, HashSet};

use daggy::{petgraph::{algo::dijkstra, visit::IntoNodeIdentifiers}, Dag, NodeIndex};

use crate::ast::{Associativity, Expr};

type PResult<T> = Result<T, (String, String)>;

fn parse_char(input: &mut &str) -> Option<char> {
    let c = input.chars().next()?;
    let offset = c.len_utf8();
    *input = &input[offset..];
    Some(c)
}

fn peek<T, F: Fn(&mut &str) -> T>(function: F, input: &&str) -> T {
    let copy = &mut &input[0..];
    return function(copy);
}

fn read<T, E, F: Fn(&mut &str) -> Result<T, E>>(function: F, input: &mut &str) -> Result<T, E> {
    let old = &mut &input[0..];
    let result = function(input);
    if result.is_err() { *input = old; }
    return result;
}

fn parse_while(predicate: fn(char) -> bool, input: &mut &str) -> Option<String> {
    let mut str = String::new();

    loop {
        if let Some(char) = input.chars().next() {
            if predicate(char) {
                str.push(char);
                parse_char(input);
                continue;
            }
        }

        return if str.is_empty() { None } else { Some(str) };
    }
}

fn require(token: &str, input: &mut &str) -> PResult<()> {
    ws(input)?;
    if input.starts_with(token) {
        *input = &input[token.len()..];
        Ok(())
    } else {
        Err((input.to_string(), format!("Expected '{}'", token)))
    }
}

fn real_token(input: &mut &str) -> PResult<String> {
    ws(input)?;

    let standalone = |c: char| c == '(' || c == ')';
    let group = |c: char| c.is_alphanumeric();
    let always_allowed = |c: char| c == '`' || c == '\'' || c == '.';
    let never_allowed = |c: char| c.is_whitespace();

    let Some(first) = parse_char(input) else {
        return Err((input.to_string(), format!("Expected a token, found none")));
    };

    if never_allowed(first) {
        return Err((input.to_string(), format!("Expected a token, found invalid character {}", first)));
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
        if let Some(char) = input.chars().next() {
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
            parse_char(input);
            continue;
        }
        return Ok(str);
    }
}

fn ws(input: &mut &str) -> PResult<()> {
    parse_while(char::is_whitespace, input);
    Ok(())
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

        ["=", "^", "*", "+", ",", "->", "::", "$"]
            .iter().for_each(|i| context.infix(i));

        context
    }
}

impl Context {

    pub fn new() -> Self {
        Context {
            partial_order: Dag::new(),
            self_referential: HashMap::new(),
            infix: HashSet::new(),
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
    pub fn get_associativity(&self, left: &str, right: &str, input: &str) -> PResult<Associativity> {
        if let Some(defined) = self.get_defined_associativity(left, right) {
            return Ok(defined);
        }

        match (self.is_infix(left), self.is_infix(right)) {
            (true, false) => Ok(Associativity::Right),
            (false, true) => Ok(Associativity::Left),
            (false, false) => Ok(Associativity::Left),
            (true, true) => Err((input.to_owned(), format!("Undefined operator associativity between {} and {}", left, right))),
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

pub fn parse_basic(context: &Context, input: &mut &str) -> PResult<Expr> {
    let old_input = &input[0..];
    let token = real_token(input)?;

    match token.as_str() {
        _ if context.is_infix(&token) => { // Do not allow standalone infixes
            *input = old_input;
            return Err((input.to_owned(), format!("Expected operation, found infix '{}'", token)));
        },
        ")" => { // Do not allow standalone closing parentheses
            *input = old_input;
            return Err((input.to_owned(), "Found closing parentheses".to_owned()));
        },
        "(" => { // Parse opening parentheses
            // Allow any expression within them
            let infix = match parse(context, input) {
                Ok(ok) => ok,
                Err(err) => {
                    *input = old_input;
                    return Err(err);
                }
            };

            ws(input)?;
            // Require closing parentheses
            if let Err(_) = require(")", input) {
                *input = old_input;
                return Err((input.to_owned(), "Expected closing parentheses".to_owned()));
            }

            return Ok(infix);
        },
        _ => Ok(Expr::Name(de_infix(token))), // Otherwise it's a named token
    }
}

pub fn parse_prefix(context: &Context, mut left: Expr, input: &mut &str) -> PResult<Expr> {
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
        left = Expr::app(left, match order {
                Associativity::Right => parse_prefix(context, right, input)?,
                Associativity::Left => right
        });
    }
}

fn read_infix(context: &Context, input: &mut &str) -> PResult<String> {
    read(|input| real_token(input).and_then(|op| {
        if context.is_infix(&op) && op != ")" {
            Ok(op)
        } else {
            Err((input.to_owned(), format!("Expected infix operation, found {}", op)))
        }
    }), input)
}

pub fn parse_infix(context: &Context, mut left: Expr, input: &mut &str) -> PResult<Expr> {

    loop {
        // If we can't find an infix operator, this means it's not an infix
        // expression, so we can exit.
        let Ok(infix) = read_infix(context, input) else {
            return Ok(left);
        };

        // Parse the left part and the infix part (but flipped, as is infix)
        let left_and_infix = Expr::app(Expr::Name(de_infix(infix)), left);

        // Append the right half
        let all = parse_prefix(context, left_and_infix, input)?;

        // Try to find another infix
        let Ok(next) = peek(|input| read_infix(context, input), input) else {
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

pub fn parse(context: &Context, input: &mut &str) -> PResult<Expr> {
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