use super::expr::{Class, EvalErr, ExprRef, Func, Val};
use super::scope::{Scope, ScopeLink};
use std::{cell::RefCell, rc::Rc};
use rustc_hash::FxHashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Print(ExprRef),
    Expr(ExprRef),
    Declare(Rc<str>, Option<ExprRef>),
    Block(Vec<Stmt>),
    If(ExprRef, Box<Stmt>, Option<Box<Stmt>>),
    While(ExprRef, Box<Stmt>),
    Return(Option<ExprRef>),
    Fun(Rc<str>, Vec<Rc<str>>, Vec<Stmt>),
    Class(Rc<str>, Option<(Rc<str>, usize)>, Vec<Stmt>),
}

pub enum ExecInterruption {
    Return(Option<Val>),
    Err(EvalErr),
}

impl From<EvalErr> for ExecInterruption {
    fn from(value: EvalErr) -> Self {
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
                    stmt.exec(scope.clone())?;
                } else if let Some(other) = other {
                    other.exec(scope.clone())?;
                }
            }
            Self::While(cond, stmt) => {
                while cond.eval(scope.clone())?.truthy() {
                    stmt.exec(scope.clone())?;
                }
            }
            Self::Return(Some(expr)) => {
                return Err(ExecInterruption::Return(Some(expr.eval(scope.clone())?)));
            }
            Self::Return(None) => {
                return Err(ExecInterruption::Return(None));
            }
            Self::Fun(id, args, func) => (*scope).borrow_mut().declare(
                id.clone(),
                Val::LoxFunc(Func(
                    args.clone().into(),
                    func.clone().into(),
                    scope.clone(),
                )),
            ),
            Self::Class(id, parent, funs) => {
                let mut funcs = FxHashMap::default();
                let (parent_class, par_scope) = if let Some((parent, dist)) = parent {
                    let Val::LoxClass(par_class) = (*scope).borrow().get(parent, *dist) else {
                        return Err(ExecInterruption::Err(EvalErr::TypeError));
                    };

                    let mut par_scope = Scope::new_child(scope.clone());
                    par_scope.declare("super".into(), Val::LoxClass(par_class.clone()));
                    let par_scope = Rc::new(RefCell::new(par_scope));
                    (Some(par_class), par_scope)
                } else {
                    (None, scope.clone())
                };

                for fun in funs.iter() {
                    let Self::Fun(id, args, body) = fun else {
                        panic!("Classes must only contain functions. Did the parser mess up?");
                    };
                    let func = Func(args.clone().into(), body.clone().into(), par_scope.clone());
                    funcs.insert(id.clone(), func);
                }
                (*scope)
                    .borrow_mut()
                    .declare(id.clone(), Val::LoxClass(Class(parent_class, funcs).into()))
            }
        }
        Ok(())
    }
}
