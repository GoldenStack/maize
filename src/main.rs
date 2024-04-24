mod ast;
mod parse;
mod infer;

use crate::{infer::infer, parse::{parse, Context}};

fn main() {

    let mut input = "length (pos x y z) = (x ^ 2 + y ^ 2 + z ^ 2) ^ 0.5";

    let context = Context::default();

    // let mut input = "example :: Int -> Int -> List Int -> Map $ List String";
    // let mut input = "fst (a, b) = a";

    println!("Parsing:  {}", input);
    println!("Yields:   {}", parse(&context, &mut input).unwrap());


    println!("{:?}", infer(&parse(&context, &mut "length (pos x y z) = (x ^ 2 + y ^ 2 + z ^ 2) ^ 0.5").unwrap()));
}

#[test]
pub fn test_parsing() {
    use crate::ast::Expr;

    let context = Context::default();
    
    let mut input = "length (pos x y z) = (x ^ 2 + y ^ 2 + z ^ 2) ^ 0.5";

    let expected = Expr::infix(
        "=".into(),
        Expr::app(
            Expr::name("length"),
            Expr::fold(Expr::name("pos"), [Expr::name("x"), Expr::name("y"), Expr::name("z")])
        ),
        Expr::infix(
            "^".into(),
            Expr::infix(
                "+".into(),
                Expr::raw_infix("^", "x", "2"),
                Expr::infix(
                    "+".into(),
                    Expr::raw_infix("^", "y", "2"),
                    Expr::raw_infix("^", "z", "2")
                )
            ),
            Expr::name("0.5")
        )
    );
    
    assert_eq!(parse(&context, &mut input), Ok(expected));

}

#[test]
pub fn test_parsing_equality() {
    assert_parse(
        "a + c d e",
        "(`+` a) ((c d) e)",
    );

    assert_parse(
        "fst (a, b) = a",
        "(`=` (fst ((`,` a) b))) a",
    );

    assert_parse(
        "example :: Int -> Int -> List Int -> Map $ List String",
        "(`::` example) ((`->` Int) ((`->` Int) ((`->` (List Int)) ((`$` Map) (List String)))))"
    );
}

fn assert_parse(mut first: &str, mut second: &str) {
    let context = Context::default();
    assert_eq!(parse(&context, &mut first), parse(&context, &mut second));

    assert_eq!(first.len(), 0);
    assert_eq!(second.len(), 0);
}
