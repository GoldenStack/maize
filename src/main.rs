mod ast;

use crate::ast::Expr;

fn main() {

    let mut input = "   length   (   pos   x   y   z   ) `=`   (   x   `^`   2   `+`   y   `^`   2   `+`   z   `^`   2  )   `^`   0.5   ";

    // let mut input = "fst (a `,` b) `=` a";

    println!("Parsing:  {}", input);
    println!("Yields:   {}", Expr::parse(&mut input).unwrap());
}

#[test]
pub fn test_parsing() {
    
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
    
    assert_eq!(Expr::parse(&mut input), Ok(expected));

}

fn assert_parse(mut first: &str, mut second: &str) {
    assert_eq!(Expr::parse(&mut first), Expr::parse(&mut second));

    assert_eq!(first.len(), 0);
    assert_eq!(second.len(), 0);
}
