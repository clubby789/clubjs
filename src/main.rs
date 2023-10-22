use std::sync::OnceLock;

use span::SourceMap;

mod ast;
mod intern;
mod lex;
mod span;

static SOURCE_MAP: OnceLock<SourceMap> = OnceLock::new();

fn main() {
    let src = std::fs::read_to_string("src.js").unwrap();
    let p = ast::Parser::new(&src);
    println!("{:#?}", p.parse());
}
