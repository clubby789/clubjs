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
    // TODO: test everything
    for entry in glob::glob("./test262/test/language/**/*.js")
        .unwrap()
        .filter_map(|e| if let Ok(p) = e { Some(p) } else { None })
        .take(1)
    {
        println!(
            "Executing `{}`",
            entry.file_name().unwrap().to_string_lossy()
        );
        if let Ok(_) = std::panic::catch_unwind(|| execute_test(entry)) {
            println!("Passed")
        } else {
            println!("Failed")
        }
    }
}

fn execute_test(path: PathBuf) {
    let src = std::fs::read_to_string(&path).unwrap();
    let agent = Agent::new();
    let assert_path = PathBuf::from("./test262/harness/assert.js");
    let sta_path = PathBuf::from("./test262/harness/sta.js");
    let assert = std::fs::read_to_string(&assert_path).expect("test262 not found");
    let sta = std::fs::read_to_string(&sta_path).expect("test262 not found");

    let assert = agent.parse_script(&assert, assert_path);
    println!("assert done");
    let sta = agent.parse_script(&sta, sta_path);
    println!("sta done");
    agent.script_evaluation(assert);
    agent.script_evaluation(sta);
    let sta = agent.parse_script(&src, path);
    agent.script_evaluation(sta);
}
