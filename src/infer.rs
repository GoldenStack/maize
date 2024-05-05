use std::collections::HashMap;

use itertools::Itertools;

use crate::ast::Expr;

pub fn count_applications(expr: &Expr) -> (&Expr, u64) {
    match expr {
        Expr::Name(_) => (expr, 0),
        Expr::App(l, _) => {
            let c = count_applications(l);
            (c.0, c.1 + 1)
        }
    }
}

pub fn infer_once(types: &HashMap<&Expr, usize>) -> Vec<(usize, usize)> {
    // List of types that we know are equal.
    let mut relations: Vec<(usize, usize)> = Vec::new();

    // In "a b" and "a c", "a b" and "a c" have the same type.
    
    // Maps an expression and a number of applications to a resulting type.
    // This is implemented with a map for efficiency, but could be implemented
    // with a simple two-dimensional loop.
    let mut application_map: HashMap<(usize, u64), usize> = HashMap::new();
    
    for expr in types.keys() {
        let (base_left_expr, application_count) = count_applications(expr);

        let base_left_id = *types.get(base_left_expr).unwrap();
        let current_id = *types.get(expr).unwrap();

        if let Some(existing_id) = application_map.get(&(base_left_id, application_count)) {
            relations.push((current_id, *existing_id));
        } else {
            application_map.insert((base_left_id, application_count), current_id);
        }
    }

    // In "a b" and "a c", "b" and "c" have the same type.

    // Maps an expression to the ID of expressions applied to it.
    // This is implemented with a map for efficiency, but could be implemented
    // with a simple two-dimensional loop.
    let mut expr_to_application_id: HashMap<usize, usize> = HashMap::new();

    for expr in types.keys() {
        let Expr::App(left, right) = expr else { continue };

        let left_id = *types.get(left.as_ref()).unwrap();
        let right_id = *types.get(right.as_ref()).unwrap();

        if let Some(generic_app_id) = expr_to_application_id.get(&left_id) {
            relations.push((right_id, *generic_app_id));
        } else {
            expr_to_application_id.insert(left_id, right_id);
        }
    }

    relations
} 

pub fn infer_many(types: &mut HashMap<&Expr, usize>) {
    loop {
        let relations = infer_once(types);

        println!("No. changes: {}", relations.len());

        let mut made_change = false;

        for (l, r) in relations.iter() {
            if l == r { continue; }
            for v in types.values_mut() {
                if v == l {
                    *v = *r;
                    made_change = true;
                }
            }
        }

        if !made_change { return; }
    }
}

pub fn infer(expr: &Expr) -> HashMap<&Expr, usize> {
    // Assign each expression a type, removing duplicates.
    // Technically, this doesn't need to deduplicate equivalent nodes as the
    // remaining algorithm will take care of the rest, but there's no harm in
    // easy deduplication.
    let mut types: HashMap<&Expr, usize> = collect(expr)
        .iter()
        .unique()
        .enumerate()
        .map(|(k, v)| (*v, k))
        .collect();

    println!("\nTypes");
    for (k, v) in types.iter().sorted_by_key(|(_, v)| *v) {
        println!("{}\t :: {}", v, k);
    }

    infer_many(&mut types);

    println!("\nIdentified After");
    for (k, v) in types.iter().sorted_by_key(|(_, v)| *v) {
        println!("{}\t :: {}", v, k);
    }
    
    types
}

fn collect<'a>(expr: &'a Expr) -> Vec<&'a Expr> {
    let mut vec = Vec::new();
    collect_into(expr, &mut vec);
    vec
}

fn collect_into<'a>(expr: &'a Expr, vec: &mut Vec<&'a Expr>) {
    vec.push(expr);
    match expr {
        Expr::Name(_) => {}
        Expr::App(l, r) => {
            collect_into(l, vec);
            collect_into(r, vec);
        }
    }
}
