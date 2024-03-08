use crate::expr::{self, EvalError, ExprRef, Val};
use crate::scope::{Scope, ScopeLink};
use std::borrow::BorrowMut;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Print(ExprRef),
    Expr(ExprRef),
    Declare(Rc<str>, Option<ExprRef>),
    Block(Vec<Stmt>),
    If(ExprRef, Box<Stmt>, Option<Box<Stmt>>),
    While(ExprRef, Box<Stmt>),
    Return(Option<ExprRef>),
    Fun(Rc<str>, Vec<Rc<str>>, Vec<Stmt>)
}

pub enum ExecInterruption {
    Return(Option<Val>),
    Err(EvalError),
}

impl From<EvalError> for ExecInterruption {
    fn from(value: EvalError) -> Self {
        ExecInterruption::Err(value)
    }
}

impl Stmt {
    pub fn exec(&self, scope: ScopeLink) -> Result<(), ExecInterruption> {
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
            Self::If(cond, stmt, other) => {
                if cond.eval(scope.clone())?.truthy() {
                    stmt.exec(Rc::new(RefCell::new(Scope::new_child(scope))))?;
                } else if let Some(other) = other {
                    other.exec(Rc::new(RefCell::new(Scope::new_child(scope))))?;
                }
            }
            Self::While(cond, stmt) => {
                let child = Rc::new(RefCell::new(Scope::new_child(scope.clone())));
                while cond.eval(scope.clone())?.truthy() {
                    stmt.exec(child.clone())?;
                }
            }
            Self::Return(Some(expr)) => {
                return Err(ExecInterruption::Return(Some(expr.eval(scope.clone())?)));
            }
            Self::Return(None) => {
                return Err(ExecInterruption::Return(None));
            }
            Self::Fun(id, args, func) => {
                (*scope).borrow_mut().declare(id.clone(), Val::LoxFunc(args.clone(), func.clone()))
            }
        }
        Ok(())
    }
}
