use crate::expr::{EvalError, Val};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type ScopeLink = Rc<RefCell<Scope>>;
#[derive(Debug, PartialEq, Default)]
pub struct Scope {
    stack: HashMap<Rc<str>, Val>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn get(&self, id: &str) -> Option<Val> {
        if let Some(val) = self.stack.get(id) {
            return Some(val.clone());
        }

        let mut cur = self.parent.clone()?;
        loop {
            let borrow = (*cur).borrow();
            if let Some(val) = borrow.stack.get(id) {
                return Some(val.clone());
            }

            let next = borrow.parent.clone()?;
            drop(borrow);
            cur = next;
        }
    }

    #[allow(clippy::map_entry)]
    pub fn set(&mut self, id: Rc<str>, val: Val) -> Result<(), EvalError> {
        if self.stack.contains_key(&id) {
            self.stack.insert(id, val);
            return Ok(());
        }

        let mut cur = self.parent.clone();
        loop {
            let Some(real) = cur else {
                break;
            };

            let mut borrow = (*real).borrow_mut();
            if borrow.stack.contains_key(&id) {
                borrow.stack.insert(id, val);
                return Ok(());
            }

            cur = borrow.parent.clone();
        }

        Err(EvalError::UndefinedVariable)
    }

    pub fn declare(&mut self, id: Rc<str>, val: Val) {
        self.stack.insert(id, val);
    }

    pub fn new_child(this: ScopeLink) -> Scope {
        Scope {
            stack: Default::default(),
            parent: Some(this),
        }
    }
}
