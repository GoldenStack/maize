use ast::Reader;

pub mod ast;

fn main() {
    let src = "abc + ,./";

    let mut reader = Reader::new(src);

    while let Some(v) = reader.token() {
        println!("{v}");
    }
}
