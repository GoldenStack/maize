mod ast;

use crate::ast::Expr;

fn main() {
    let mut input = "(add a) b c (pos x y z ) d = 3";


    // a + b + c + d
    // (a + (b + (c + d)))
    // (((a + b) + c) + d)

    // (((a ^ b) * c) + d)
    // (a + (b * (c ^ d)))

    // (+ * a b)
    // vs
    // (+ (* a b))

    let mut input = "(+ a * (`b` c) d e)"; // ((+ a) (((* (`b` c)) d) e))

    let mut input = "a * b `+` c";
    let mut input = "a + b `+` c";
    let mut input = "a * b `c` d `e` f";
    let mut input = "a `b` c `d`";

    let mut input = "1 `*` a `**` b `*` c";

    // let mut input = "* a + b c d e";
    // ((+ a) ((((* b) c) d) e))
    // let mut input = "a b c d e";

    println!("{}", Expr::parse(&mut input).unwrap());
}
