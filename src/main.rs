use ast::{Context, Reader};

pub mod ast;

fn main() {
    let mut context = Context::new();
    
    let src = "((((((((((((((abc)))))))))))))) + ,./";

    let mut reader = Reader::new(&context, src);

    println!("{}", reader.read().unwrap());
}
