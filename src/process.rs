use std::{collections::HashMap, fmt::{self, write, Display}};

use crate::parse::AST;

pub enum Expr {
    Var(String),
    Idx(usize),
    Lam(usize, Box<Expr>), // Named De Bruijn indices for convenience
    App(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(var) => write!(f, "\"{var}\""),
            Expr::Idx(i) => write!(f, "{i}"),
            Expr::Lam(i, expr) => write!(f, "(\\{i} {expr})"),
            Expr::App(l, r) => write!(f, "({l}, {r})"),
        }
    }
}

pub fn process(input: AST) -> Expr {

    let mut index = 0usize;
    let mut map: HashMap<String, usize> = HashMap::new();

    process2(input, &mut index, &mut map)

}

fn process2(input: AST, index: &mut usize, map: &mut HashMap<String, usize>) -> Expr {
    let (l, r) = match input {
        AST::Name(n) => return map.get(&n).cloned().map(Expr::Idx).unwrap_or(Expr::Var(n)),
        AST::App(l, r) => (l, r),
    };

    if let AST::App(ll, lr) = l.as_ref() {
        if matches!(ll.as_ref(), AST::Name(name) if name == "\\") {
            if let AST::Name(n) = lr.as_ref() {
                let i = *index;
                *index += 1;

                map.insert(n.clone(), i); 
        
                return Expr::Lam(i, Box::new(process2(*r, index, map)));
            }
        }
    }

    let l = process2(*l, index, map);
    let r = process2(*r, index, map);

    Expr::App(Box::new(l), Box::new(r))

}
