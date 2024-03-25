mod ast;

use crate::ast::Parser;

pub fn default_parser() -> Parser {
    let mut parser = Parser::new();
    parser.ra("`->`");
    parser.ra("`^`");
    parser.ra("`*`");
    parser.gt("`^`", "`*`");
    parser.gt("`*`", "`+`");
    parser.lt("`=`", "`+`");
    parser.infix("`=`");
    parser.infix("`^`");
    parser.infix("`*`");
    parser.infix("`+`");
    parser
}

fn main() {

    let mut input = "length (pos x y z) `=` (x `^` 2 `+` y `^` 2 `+` z `^` 2) `^` 0.5";

    let parser = default_parser();

    // let mut input = "fst (a `,` b) `=` a"; 

    println!("Parsing:  {}", input);
    println!("Yields:   {}", parser.parse(&mut input).unwrap());
}

#[test]
pub fn test_parsing() {
    use crate::ast::Expr;

    let parser = default_parser();
    
    let mut input = "length (pos x y z) `=` (x `^` 2 `+` y `^` 2 `+` z `^` 2) `^` 0.5";

    let expected = Expr::infix(
        "`=`".into(),
        Expr::app(
            Expr::name("length"),
            Expr::fold(Expr::name("pos"), [Expr::name("x"), Expr::name("y"), Expr::name("z")])
        ),
        Expr::infix(
            "`^`".into(),
            Expr::infix(
                "`+`".into(),
                Expr::raw_infix("`^`", "x", "2"),
                Expr::infix(
                    "`+`".into(),
                    Expr::raw_infix("`^`", "y", "2"),
                    Expr::raw_infix("`^`", "z", "2")
                )
            ),
            Expr::name("0.5")
        )
    );
    
    assert_eq!(parser.parse(&mut input), Ok(expected));

}

fn assert_parse(mut first: &str, mut second: &str) {
    let parser = default_parser();
    assert_eq!(parser.parse(&mut first), parser.parse(&mut second));

    assert_eq!(first.len(), 0);
    assert_eq!(second.len(), 0);
}
