use std::{
    env::args_os,
    fs,
    io::{stdin, stdout, IsTerminal, Write},
    path::Path,
};

use bergbuch::parser::Parser;
use bergbuch::scanner::scan;

use bergbuch::interpreter::Interpreter;

fn main() {
    if let Some(arg) = args_os().nth(1) {
        run_file(Path::new(&arg));
    } else {
        run_prompt();
    }
}

fn run_file(path: &Path) {
    let content = fs::read_to_string(path).expect("Error reading file.");
    let mut interpreter = Interpreter::new();
    run(content.as_str(), &mut interpreter);
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();
    if stdin().is_terminal() {
        print!("> ");
        stdout().flush().unwrap();
    }

    for line in stdin().lines() {
        run(&line.expect("Error reading stdin"), &mut interpreter);
        if stdin().is_terminal() {
            print!("> ");
            stdout().flush().unwrap();
        }
    }
}

fn run(code: &str, interpreter: &mut Interpreter) {
    let (scanned, _err) = scan(code);
    let mut parser = Parser::new(&scanned);
    let parsed = parser.parse();
    match parsed {
        Ok(parsed) => {
            if let Err(err) = interpreter.interpret(parsed) {
                println!("{}", err);
            }
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}
