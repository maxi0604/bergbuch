use crate::scanner::scan;
use crate::expr::{EvalError, Val};
use crate::parser::Parser;
use crate::scope::Scope;
use crate::statement::Stmt;
use std::{cell::RefCell, rc::Rc};

pub struct Interpreter {
    global_scope: Rc<RefCell<Scope>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn run(&mut self, code: &str) {
        let (scanned, _err) = scan(code);
        let mut parser = Parser::new(&scanned);
        let parsed = parser.parse();
        match parsed {
            Ok(parsed) => {
                if let Err(err) = self.interpret(parsed) {
                    println!("{}", err);
                }
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
    pub fn interpret(&mut self, program: impl IntoIterator<Item = Stmt>) -> Result<(), EvalError> {
        for stmt in program.into_iter() {
            stmt.exec(self.global_scope.clone())?;
        }
        Ok(())
    }

    pub fn new() -> Interpreter {
        Interpreter {
            global_scope: Default::default(),
        }
    }

    pub fn get_global(&self, id: &str) -> Option<Val> {
        (*self.global_scope).borrow().get(id)
    }
}
