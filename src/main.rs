use std::{
    env::args_os,
    fs,
    io::{stdin, stdout, IsTerminal, Write},
    path::Path,
    process::ExitCode,
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
    if let Err(err) = interpreter.run(content.as_str()) {
        println!("error: {}", err);
    }
}

fn run_prompt() {
    let input = stdin();
    let mut interpreter = Interpreter::new();
    loop {
        if input.is_terminal() {
            print!("> ");
            stdout().flush().unwrap();
        }
        let mut line = String::new();
        let len = input.read_line(&mut line).unwrap();
        if len == 0 {
            break;
        }
        if let Err(err) = interpreter.run(&line) {
            println!("error: {}", err);
        }
    }
}
