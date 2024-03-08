use std::{
    rc::Rc, cell::RefCell
};
use crate::expr::{EvalError, ExprRef, Val};
use crate::scope::{ScopeLink, Scope};

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(ExprRef),
    Expr(ExprRef),
    Declare(Rc<str>, Option<ExprRef>),
    Block(Vec<Stmt>),
    If(Box<Stmt>),
    While(Box<Stmt>),
}

impl Stmt {
    pub fn exec(&self, scope: ScopeLink) -> Result<(), EvalError> {
        match self {
            Self::Print(expr) => println!("{}", expr.eval(scope.clone())?),
            Self::Expr(expr) => { expr.eval(scope.clone())?; },
            Self::Declare(id, val) => {
                if let Some(val) = val {
                    let val = val.eval(scope.clone())?;
                    (*scope).borrow_mut().define(id.clone(), val);
                } else {
                    (*scope).borrow_mut().define(id.clone(), Val::Nil);
                }
            }
            Self::Block(stmts) => {
                let child = Rc::new(RefCell::new(Scope::new_child(scope)));
                for stmt in stmts.iter() {
                    stmt.exec(child.clone())?;
                }
            }
            _ => todo!("TODO")
        }
        Ok(())
    }
}
