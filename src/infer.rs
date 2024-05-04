use std::collections::HashMap;

use itertools::Itertools;

use crate::ast::Expr;

pub fn count(expr: &Expr) -> (&Expr, u64) {
    match expr {
        Expr::Name(_) => (expr, 0),
        Expr::App(l, _) => {
            let c = count(l);
            (c.0, c.1 + 1)
        }
    }
}

pub fn infer(expr: &Expr) -> () {

    let exprs = collect(expr);

    // Assign each expression a type, removing duplicates
    // Technically this just needs to deduplicate equivalent Expr::Name nodes, 
    // as the remaining algorithm will take care of the rest.
    let mut identified: HashMap<&Expr, usize> = exprs
        .iter()
        .unique()
        .enumerate()
        .map(|(k, v)| (*v, k))
        .collect();

    let mut graph: HashMap<(usize, u64), usize> = HashMap::new();

    // In "a b" and "a c", "a b" and "a c" have the same type.
    for expr in exprs.iter() {
        let (origin, count) = count(expr);

        let Some(origin_id) = identified.get(origin) else { continue };

        if let Some(id) = graph.get(&(*origin_id, count)) {
            identified.insert(expr, *id);
        } else {
            graph.insert((*origin_id, count), *identified.get(expr).unwrap());
        }

    }

    let mut graph2: HashMap<usize, usize> = HashMap::new();

    // In "a b" and "a c", "b" and "c" have the same type.
    for expr in exprs.iter() {
        let Expr::App(l, r) = expr else { continue };

        if let Some(id) = graph2.get(identified.get(l.as_ref()).unwrap()) {
            identified.insert(r, *id);
        } else {
            graph2.insert(*identified.get(l.as_ref()).unwrap(), *identified.get(r.as_ref()).unwrap());
        }
    }

    println!("\nInferred types");
    for (k, v) in identified.iter().sorted_by_key(|(_, v)| *v) {
        println!("{}\t :: {}", v, k);
    }
}

fn collect<'a>(expr: &'a Expr) -> Vec<&'a Expr> {
    let mut vec = Vec::new();
    collect_int(expr, &mut vec);
    vec
}

fn collect_int<'a>(expr: &'a Expr, vec: &mut Vec<&'a Expr>) {
    vec.push(expr);
    match expr {
        Expr::Name(_) => {}
        Expr::App(l, r) => {
            collect_int(l, vec);
            collect_int(r, vec);
        }
    }
}
