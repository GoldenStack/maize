
use crate::{ast::Expr, parse::{parse, Context, Reader}};

pub fn name(name: &str) -> Expr {
    Expr::Name(name.to_owned())
}

pub fn app(app: Expr, var: Expr) -> Expr {
    Expr::App(Box::new(app), Box::new(var))
}

pub fn infix(infix: String, left: Expr, right: Expr) -> Expr {
    app(app(Expr::Name(infix), left), right)
}

pub fn raw_infix(infix_expr: &str, left: &str, right: &str) -> Expr {
    infix(infix_expr.into(), name(left), name(right))
}

pub fn fold<const N: usize>(app_expr: Expr, vars: [Expr; N]) -> Expr {
    let mut folded = app_expr;
    for var in vars {
        folded = app(folded, var);
    }
    folded
}

#[test]
pub fn test_parsing() {
    let context = Context::default();
    
    let input = "length (pos x y z) = (x ^ 2 + y ^ 2 + z ^ 2) ^ 0.5";
    let mut input = Reader::new(input);

    let expected = infix(
        "=".into(),
        app(
            name("length"),
            fold(name("pos"), [name("x"), name("y"), name("z")])
        ),
        infix(
            "^".into(),
            infix(
                "+".into(),
                raw_infix("^", "x", "2"),
                infix(
                    "+".into(),
                    raw_infix("^", "y", "2"),
                    raw_infix("^", "z", "2")
                )
            ),
            name("0.5")
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
    let (mut f, mut s) = (Reader::new(first), Reader::new(second));
    assert_eq!(parse(&context, &mut f), parse(&context, &mut s));

    assert_eq!(f.slice().len(), 0);
    assert_eq!(s.slice().len(), 0);
}
