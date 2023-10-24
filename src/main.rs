mod ast;
mod codegen;
mod intern;
mod lex;
mod session;
mod span;

use std::path::PathBuf;

use session::SESSION;

fn main() {
    let path = std::env::args()
        .nth(1)
        .map_or_else(|| PathBuf::from("src.js"), |a| a.try_into().unwrap());
    let src = std::fs::read_to_string(&path).unwrap();
    let p = ast::Parser::new(&src, path);
    // println!("{:#?}", p.parse());
    dbg!(codegen::FunctionBuilder::codegen(p.parse()));
}
