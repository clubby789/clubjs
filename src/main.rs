mod ast;
mod intern;
mod lex;
mod span;

fn main() {
    let src = std::fs::read_to_string("src.js").unwrap();
    let p = ast::Parser::new(&src);
    println!("{:#?}", p.parse());
}
