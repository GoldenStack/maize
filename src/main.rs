use ast::{Context, Reader};

pub mod ast;

fn main() {
    let mut context = Context::new();
    context.gt("^", "*");
    context.gt("*", "+");

    context.infix_many(["^", "*", "+"]);
    
    let src = r"(\x \y x + y) 1 2 ";

    let mut reader = Reader::new(&context, src);

    let read = reader.read().unwrap();

    println!("{}", read);
}
