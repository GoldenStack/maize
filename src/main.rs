pub mod parse;
pub mod infer;

use crate::parse::{parse, Context, Reader};

fn main() {
    let input = "(\\x \\y y) 1 2";

    let context = Context::default();

    let mut input = Reader::new(input);


    println!("Parsing:  {:?}", input);
    println!("Yields:   {}", parse(&context, &mut input).unwrap());
}
