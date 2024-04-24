use std::collections::HashMap;

use itertools::Itertools;

use crate::ast::Expr;

pub fn infer(expr: &Expr) -> () {

    let exprs = collect(expr);

    // Assign each expression a type, removing duplicates
    // Technically this just needs to deduplicate equivalent Expr::Name nodes, 
    // as the remaining algorithm will take care of the rest.
    let identified: HashMap<&Expr, usize> = exprs
        .iter()
        .unique()
        .enumerate()
        .map(|(k, v)| (*v, k))
        .collect();

    println!("\nIdentified");
    for (k, v) in identified.iter().sorted_by_key(|(_, v)| *v) {
        println!("{}\t :: {}", v, k);
    }

    let mut graph: HashMap<usize, Vec<usize>> = HashMap::new();
    
    // Symmetry: create a graph of applications, and then start with names.
    for expr in identified.keys() {
        let Expr::App(l, r) = expr else { continue };
        let Some(idl) = identified.get(l.as_ref()) else { continue };
        let Some(idr) = identified.get(r.as_ref()) else { continue };
        
        graph.entry(*idl).or_insert_with(Vec::new).push(*idr);
    }

    let rev_get = |i: &usize| identified.iter().find(|(_, v)| *v == i).map(|(k, _)| k).unwrap();

    println!("\nApps");
    for (k, v) in graph.iter().sorted_by_key(|(_, v)| *v) {
        println!("{}\t applied to all of {}", rev_get(k), v.iter().map(|i| rev_get(i)).map(|expr| format!("{}", expr)).collect::<Vec<_>>().join(",   "));
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
