use ast::{Context, Reader};

pub mod ast;

fn main() {
    let mut context = Context::new();
    context.gt("*", "+");
    
    let src = "\\x \\y + 1 * 2 4";

    let mut reader = Reader::new(&context, src);

    println!("{}", reader.read().unwrap());
}
