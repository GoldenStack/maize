pub mod parse;
pub mod process;
pub mod infer;

use process::process;

use crate::parse::{parse, Context, Reader};

fn main() {
    let input = "(\\x x \\y \\x x y)) 1 2";

    let context = Context::default();

    let mut input = Reader::new(input);
    println!("Parsing:  {:?}", input);

    let mut parsed = parse(&context, &mut input).unwrap();
    println!("Yields:   {}", parsed);


    let mut processed = process(parsed);
    println!("Processed: {}", processed);
}
