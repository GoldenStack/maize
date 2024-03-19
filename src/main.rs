mod ast;

use crate::ast::Signature;

fn main() {
    let mut input = "(add a) b c (pos x y z ) d = 3";

    println!("{}", Signature::parse(&mut input).unwrap());
}
