use std::{
    env::args_os,
    fs,
    io::{stdin, stdout, IsTerminal, Write, self},
    path::Path,
    process::ExitCode,
};

use bergbuch::interpreter::Interpreter;
use rustyline::{error::ReadlineError};
use rustyline::{DefaultEditor};
use std::error::Error;

fn main() -> ExitCode {
    if args_os().len() > 2 {
        eprintln!("usage: lox [file]");
        return ExitCode::FAILURE
    }

    if let Some(arg) = args_os().nth(1) {
        run_file(Path::new(&arg));
    } else {
        return if run_prompt().is_ok() {
            ExitCode::SUCCESS
        } else {
            ExitCode::FAILURE
        }
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

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut interpreter = Interpreter::new();
    if !stdin().is_terminal() {
        let program = io::read_to_string(stdin().lock())?;
        if let Err(err) = interpreter.run(program.as_str()) {
            println!("error: {}", err);
        }
    }

    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                if let Err(err) = interpreter.run(&line) {
                    println!("error: {}", err);
                }
            },
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => return Ok(()),
            Err(err) => {
                break Err(Box::new(err));
            }
        }
    }
}
