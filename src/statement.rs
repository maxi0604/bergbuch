use crate::expr::{EvalError, ExprRef, Val};
use crate::scope::{Scope, ScopeLink};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(ExprRef),
    Expr(ExprRef),
    Declare(Rc<str>, Option<ExprRef>),
    Block(Vec<Stmt>),
    If(ExprRef, Box<Stmt>),
    While(ExprRef, Box<Stmt>),
}

impl Stmt {
    pub fn exec(&self, scope: ScopeLink) -> Result<(), EvalError> {
        match self {
            Self::Print(expr) => println!("{}", expr.eval(scope.clone())?),
            Self::Expr(expr) => {
                expr.eval(scope.clone())?;
            }
            Self::Declare(id, val) => {
                if let Some(val) = val {
                    let val = val.eval(scope.clone())?;
                    (*scope).borrow_mut().declare(id.clone(), val);
                } else {
                    (*scope).borrow_mut().declare(id.clone(), Val::Nil);
                }
            }
            Self::Block(stmts) => {
                let child = Rc::new(RefCell::new(Scope::new_child(scope)));
                for stmt in stmts.iter() {
                    stmt.exec(child.clone())?;
                }
            }
            Self::If(cond, stmt) => {
                if cond.eval(scope.clone())?.truthy() {
                    stmt.exec(Rc::new(RefCell::new(Scope::new_child(scope))))?;
                }
            }
            Self::While(cond, stmt) => {
                let child = Rc::new(RefCell::new(Scope::new_child(scope.clone())));
                while cond.eval(scope.clone())?.truthy() {
                    stmt.exec(child.clone())?;
                }
            }
        }
        Ok(())
    }
}
