use crate::expr::EvalError;
use crate::expr::Val;
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
