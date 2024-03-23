mod ast;

use crate::ast::Expr;

fn main() {

    // let mut input = "length (pos x y z) `=` (x `^` 2 `+` y `^` 2 `+` z `^` 2) `^` 0.5";
    let mut input = "(x `^` 2 `+` y `^` 2 `+` z `^` 2)";
    // let mut input = "2 `+` x `^` List Int `^` 2";

    // let mut input = "(2 `*` 4)";

    let mut input = "1 `*` (2 `+` 3) `*` 4";

    println!("{}", Expr::parse_infix(&mut input).unwrap());
}
