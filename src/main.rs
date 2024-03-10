use std::{
    env::args_os,
    fs,
    io::{stdin, stdout, IsTerminal, Write},
    path::Path, process::ExitCode,
};

use bergbuch::interpreter::Interpreter;

fn main() -> ExitCode {
    if args_os().len() > 2 {
        eprintln!("usage: lox [file]");
        return ExitCode::FAILURE;
    }

    if let Some(arg) = args_os().nth(1) {
        run_file(Path::new(&arg));
    } else {
        run_prompt();
    }

    ExitCode::SUCCESS
}

fn run_file(path: &Path) {
    let content = fs::read_to_string(path).expect("Error reading file.");
    let mut interpreter = Interpreter::new();
    interpreter.run(content.as_str());
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();
    if stdin().is_terminal() {
        print!("> ");
        stdout().flush().unwrap();
    }

    for line in stdin().lines() {
        if let Err(err) = interpreter.run(&line.expect("Error reading stdin")) {
            println!("{}", err);
        }
        if stdin().is_terminal() {
            print!("> ");
            stdout().flush().unwrap();
        }
    }
}
