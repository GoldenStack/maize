mod ast;
mod parse;
mod infer;
mod test;

use crate::{infer::infer, parse::{parse, Context, Reader}};

fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");

    // let input = "length (pos x y z) = (x ^ 2 + y ^ 2 + z ^ 2) ^ 0.5";
    let input = r"
    pos = (id Pos):
        x :: 5
        y :: 7
    ";

    let context = Context::default();

    let mut input = Reader::new(input);

    // let mut input = "example :: Int -> Int -> List Int -> Map $ List String";
    // let mut input = "fst (a, b) = a";

    println!("Parsing:  {:?}", input);
    println!("Yields:   {}", parse(&context, &mut input).unwrap());

    // println!("{:?}", infer(&parse(&context, &mut "length (pos x y z) = (x ^ 2 + y ^ 2 + 2 ^ z) ^ 2").unwrap()));
    // println!("{:?}", infer(&parse(&context, &mut "id a, id b").unwrap()));
    // println!("{:?}", infer(&parse(&context, &mut "a b -> a c -> b d -> c e").unwrap()));
    // println!("{:?}", infer(&parse(&context, &mut "a b -> a c -> a d -> b d -> c e -> f a -> f g -> b f").unwrap()));

    // Type inference fails here because `->` takes any type.
    // Solution: explicit external polymorphism. Functions can only be assumed
    // to be polymorphic insofar as they have been externally declared as
    // polymorphic 
    // println!("{:?}", infer(&parse(&context, &mut "a b -> a c -> a d -> b d -> c e -> f a -> f g -> b f").unwrap()));
}
