use std::collections::HashMap;

use itertools::Itertools;

use crate::ast::Expr;

/// Counts the number of applications the given expression is, returning the
/// base expression and the count.
/// 
/// Expressions are binary trees, so expressions can be considered to have a
/// "leftmost" term. The "base expression" is this leftmost term, and the
/// "count" is the depth of this term, based on the original expression.
pub fn count_applications(expr: &Expr) -> (&Expr, u64) {
    match expr {
        Expr::Name(_) => (expr, 0),
        Expr::App(l, _) => {
            let c = count_applications(l);
            (c.0, c.1 + 1)
        }
    }
}

/// Performs a single pass of type inference over the provided typed
/// expressions, returning a list of pairs of types that are equal.
/// 
/// The two inference rules are as follows:
/// - Return type equality:
///     In the expressions "a b" and "a c", "a b" and "a c" have the same type.
/// - Parameter type equality:
///     In the expressions "a b" and "a c", "b" and "c" have the same type.
/// 
/// Importantly, these rules apply for separate expressions with the same type,
/// not just the same expression being applied twice.
/// 
/// These rules are all that is required for most general type inference.
/// 
/// Although this works for most common expressions, this algorithm is known to
/// have issues with polymorphic functions, which can function as generics or
/// templates, terms you may know from other languages.
/// It cannot correctly infer them because the definitions of these polymorphic
/// functions are unknown until the types of parameters provided to them are
/// known, because the types of the functions are functions themselves.
/// 
/// Take the identity operator, for example.
/// ```Maize
/// id a = a
/// ```
/// This has no type requirements, and is polymorphic to every type. Thus, type
/// inference over usages of it, utilizing the algorithm explained prior, make
/// incorrect assumptions about it. Two separate usages of the identity function
/// will be assumed to have equal parameter types, as well as equal results.
/// ```Maize
/// (id a) (id b)
/// ```
/// In the code segment above, `a` and `b` are assumed to have equal types,
/// because they are both the result of applying one argument to the identity
/// function. This is an incorrect assumption. Furthermore, `(id a)` and
/// `(id b)` are assumed to have the same type, which is also incorrect. Their
/// true types cannot be known until the types of `a` and `b` are known.
/// 
/// Polymorphic functions break both of the core rules used in the algorithm, so
/// it is impossible to make any assumptions about types from their usages.
/// 
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

/// Performs as many inference passes over the provided types as is necessary.
/// This modifies the provided map in-place to indicate which types are equal
/// by modifying the keys of the map.
/// 
/// Inference passes stop when no more inferences are made than the last
/// iteration.
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

/// Assigns an arbitrary type number to each sub-expression (subtree) of the
/// given expression.
/// The numbers are meaningless, except for that subtrees with equal type
/// numbers are assumed to have equal types.
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

/// Collects all subtrees of the provided expression into a list.
fn collect<'a>(expr: &'a Expr) -> Vec<&'a Expr> {
    let mut vec = Vec::new();
    collect_into(expr, &mut vec);
    vec
}

/// Collects all subtrees of the provided expression into the provided list.
/// This is used internally in [collect].
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
