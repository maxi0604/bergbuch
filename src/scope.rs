use crate::expr::Val;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type ScopeLink = Rc<RefCell<Scope>>;
#[derive(Debug, PartialEq, Default)]
pub struct Scope {
    stack: HashMap<Rc<str>, Val>,
    parent: Option<ScopeLink>,
}

impl Scope {
    pub fn try_get_here(&self, id: &str) -> Option<Val> {
        self.stack.get(id).cloned()
    }
    pub fn get(&self, id: &str, dist: usize) -> Val {
        // self is of type &Scope, the rest of the elements
        // of the chain are of type ScopeLink. Do one resolution before the loop.
        if dist == 0 {
            return self
                .stack
                .get(id)
                .cloned()
                .expect("Reference to undefined value should have been caught in resolution");
        }

        let mut cur = self
            .parent
            .clone()
            .unwrap_or_else(|| panic!("Resolver gave invalid depth {dist}"));

        for _ in 0..dist - 1 {
            let borrow = (*cur).borrow();
            let next = borrow
                .parent
                .clone()
                .expect("Resolver gave invalid depth, I've reached the bottom of the stack");
            drop(borrow);
            cur = next;
        }

        let borrow = (*cur).borrow();
        borrow
            .stack
            .get(id)
            .cloned()
            .expect("Reference to undefined value should have been caught in resolution")
    }

    #[allow(clippy::map_entry)]
    pub fn set(&mut self, id: Rc<str>, dist: usize, val: Val) {
        if dist == 0 {
            self.stack.insert(id, val);
            return;
        }

        let mut cur = self
            .parent
            .clone()
            .unwrap_or_else(|| panic!("Resolver gave invalid depth {dist}"));
        for _ in 0..dist - 1 {
            let borrow = (*cur).borrow();
            let next = borrow
                .parent
                .clone()
                .expect("Resolver gave invalid depth, I've reached the bottom of the stack");
            drop(borrow);
            cur = next;
        }

        (*cur).borrow_mut().stack.insert(id, val);
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
