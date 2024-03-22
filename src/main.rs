mod ast;

use crate::ast::Expr;

fn main() {

    let mut input = "length (pos x y z) `=` (x `^` 2 `+` y `^` 2 `+` z `^` 2) `^` 0.5";

    println!("{}", Expr::parse(&mut input).unwrap());
}
