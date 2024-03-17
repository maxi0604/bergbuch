use std::{
    env::args_os,
    fs,
    io::{stdin, IsTerminal, self},
    path::Path,
    process::ExitCode,
};

use bergbuch::treewalk::interpreter::Interpreter;
use rustyline::{error::ReadlineError, Cmd, ConditionalEventHandler, Event, EventContext, EventHandler, Highlighter, KeyEvent, Movement, RepeatCount};
use rustyline::{Editor};
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Validator, Helper, Completer, Hinter};
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

struct TabEventHandler;
impl ConditionalEventHandler for TabEventHandler {
    fn handle(&self, _: &Event, _n: RepeatCount, _: bool, _: &EventContext) -> Option<Cmd> {
        Some(Cmd::Indent(Movement::WholeLine))
    }
}

#[derive(Helper, Completer, Hinter, Highlighter, Validator)]
struct MyHelper {
    #[rustyline(Completer)]
    completer: (),
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut interpreter = Interpreter::new();
    if !stdin().is_terminal() {
        let program = io::read_to_string(stdin().lock())?;
        if let Err(err) = interpreter.run(program.as_str()) {
            println!("error: {}", err);
        }
    }

    let h = MyHelper {
        completer: (),
        validator: MatchingBracketValidator::new()
    };
    let mut rl = Editor::new()?;
    rl.set_helper(Some(h));
    rl.bind_sequence(
        KeyEvent::from('\t'),
        EventHandler::Conditional(Box::new(TabEventHandler)),
    );

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
