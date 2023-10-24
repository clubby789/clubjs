mod ast;
mod codegen;
mod intern;
mod interpreter;
mod lex;
mod session;
mod span;
mod util;

use std::path::PathBuf;

use interpreter::Agent;
use session::SESSION;

fn main() {
    let path = std::env::args()
        .nth(1)
        .map_or_else(|| PathBuf::from("src.js"), |a| a.try_into().unwrap());
    let src = std::fs::read_to_string(&path).unwrap();
    let agent = Agent::new();
    let script = agent.borrow().parse_script(&src, path);
    agent.borrow_mut().script_evaluation(script);
}
