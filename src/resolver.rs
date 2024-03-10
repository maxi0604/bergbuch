use std::collections::HashMap;
use std::rc::Rc;
use std::fmt::{Display, self};
use crate::expr::{Expr};

use crate::statement::Stmt;

#[derive(Default)]
struct ResolverScope {
    vars: HashMap<Rc<str>, bool>,
}

#[derive(Debug, Clone)]
pub enum ResolverErr {
    UseWhileDeclaring,
    UndeclaredVar,
    UndefinedVar,
    RedeclarationInSameScope
}

type ErrList = Vec<ResolverErr>;
impl Display for ResolverErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UseWhileDeclaring => write!(f, "Using variable in its declaration."),
            Self::UndeclaredVar => write!(f, "Undeclared variable."),
            Self::UndefinedVar => write!(f, "Type error."),
            Self::RedeclarationInSameScope => write!(f, "Cannot redeclare variable in same scope."),
        }
    }
}


impl ResolverScope {
    fn new() -> Self {
        Default::default()
    }
}

pub struct Resolver {
    stack: Vec<ResolverScope>
}

impl Resolver {
    pub fn new() -> Self {
        // Just the global scope is present.
        Self { stack: vec![ResolverScope::new()] }
    }
    pub fn resolve(&mut self, program: &mut [Stmt]) -> Result<(), Vec<ResolverErr>> {
        let mut result = vec![];

        for stmt in program {
            self.resolve_stmt(stmt, &mut result);
        }

        if result.len() > 0 {
            Err(result)
        } else {
            Ok(())
        }
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt, errs: &mut ErrList) {
        match stmt {
            Stmt::If(cond, if_stmt, else_stmt) => {
                self.resolve_expr(cond, errs);
                self.resolve_stmt(if_stmt, errs);
                if let Some(else_stmt) = else_stmt {
                    self.resolve_stmt(else_stmt, errs);
                }
            }
            Stmt::While(cond, body) => {
                self.resolve_expr(cond, errs);
                self.resolve_stmt(body, errs);
            }
            Stmt::Expr(expr) | Stmt::Print(expr) | Stmt::Return(Some(expr)) => self.resolve_expr(expr, errs),
            Stmt::Block(stmts) => {
                self.push_scope();
                for inner in stmts.iter_mut() {
                    self.resolve_stmt(inner, errs);
                }
                self.pop_scope();
            }
            Stmt::Return(None) => {}
            Stmt::Declare(id, val) => {
                self.declare(id.clone());
                if let Some(val) = val {
                    self.resolve_expr(val, errs);
                }
                self.define(id.clone());
            }
            Stmt::Fun(id, args, body) => {
                // Function is defined within its body.
                // (Otherwise recursion would be forbidden)
                self.declare(id.clone());
                self.define(id.clone());

                self.push_scope();
                // All args are defined in the scope of a function.
                for arg in args.iter() {
                    self.declare(arg.clone());
                    self.define(arg.clone());
                }

                for inner in body.iter_mut() {
                    self.resolve_stmt(inner, errs);
                }
                self.pop_scope();
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr, errs: &mut ErrList) {
        match expr {
            Expr::Call(fun, args) => {
                self.resolve_expr(fun, errs);
                for arg in args.iter_mut() {
                    self.resolve_expr(arg, errs);
                }
            }
            Expr::Unary(_, r) => self.resolve_expr(r, errs),
            Expr::Binary(_, l, r) => {
                self.resolve_expr(l, errs);
                self.resolve_expr(r, errs);
            }
            Expr::Literal(_) => {},
            Expr::Assignment(id, dist, val) => {
                self.resolve_expr(val, errs);
                match self.resolve_id(id) {
                    Ok(res) => *dist = res,
                    Err(err) => errs.push(err),
                }
            }
            Expr::Variable(id, dist) => {
                if self.currently_declaring(id) {
                    errs.push(ResolverErr::UseWhileDeclaring);
                } else {
                    match self.resolve_id(id) {
                        Ok(res) => *dist = res,
                        Err(err) => errs.push(err),
                    }
                }
            }

        }
    }

    fn resolve_id(&mut self, id: &str) -> Result<usize, ResolverErr> {
        for (i, scope) in self.stack.iter().rev().enumerate() {
            if scope.vars.contains_key(id) {
                // Since undefined values are resolved to "nil" at runtime
                // we consider this to be resolved.
                return Ok(i);
            }
        }

        Err(ResolverErr::UndeclaredVar)
    }

    fn push_scope(&mut self) {
        self.stack.push(ResolverScope::new());
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn declare(&mut self, id: Rc<str>) {
        self.stack.last_mut().expect("Popped global scope").vars.insert(id, false);
    }

    fn define(&mut self, id: Rc<str>) {
        self.stack.last_mut().expect("Popped global scope").vars.insert(id, true);
    }
    fn currently_declaring(&mut self, id: &str) -> bool {
        !self.stack.last_mut().expect("Popped global scope").vars.get(id).unwrap_or(&true)
    }
}
