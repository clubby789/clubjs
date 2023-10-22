mod ast;
mod ast_lowering;
mod intern;
mod lex;
mod session;
mod span;

use session::SESSION;

fn main() {
    let src = std::fs::read_to_string("src.js").unwrap();
    let p = ast::Parser::new(&src);
    // println!("{:#?}", p.parse());
    p.parse();
}
