use std::{
    cell::RefCell,
    env::args_os,
    fs,
    io::{stdin, stdout, IsTerminal, Write},
    path::Path,
    rc::Rc,
};

use bergbuch::expr::EvalError;
use bergbuch::parser::Parser;
use bergbuch::scanner::scan;
use bergbuch::scope::Scope;
use bergbuch::statement::Stmt;

struct Interpreter {
    global_scope: Rc<RefCell<Scope>>,
}

impl Interpreter {
    fn interpret(&mut self, program: impl IntoIterator<Item = Stmt>) -> Result<(), EvalError> {
        for stmt in program.into_iter() {
            stmt.exec(self.global_scope.clone())?;
        }
        Ok(())
    }

    fn new() -> Interpreter {
        Interpreter {
            global_scope: Default::default(),
        }
    }
}

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
    let parsed = parser.program();
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
