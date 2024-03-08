use crate::scanner::scan;
use crate::expr::{EvalError, NativeCall, Val};
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
        let mut global_scope = Scope::default();
        global_scope.declare("clock".into(), Val::NativeFunc(NativeCall::Clock));
        Interpreter {
            global_scope: Rc::new(RefCell::new(global_scope)),
        }
    }

    pub fn get_global(&self, id: &str) -> Option<Val> {
        (*self.global_scope).borrow().get(id)
    }
}
