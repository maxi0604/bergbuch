use std::collections::HashMap;
use std::rc::Rc;
use std::fmt::{Display, self};
use crate::expr::{Expr, ExprRef};

use crate::statement::Stmt;

#[derive(Default)]
struct ResolverScope {
    vars: HashMap<Rc<str>, bool>,
}

#[derive(Debug, Clone)]
pub enum ResolverErr {
    UndeclaredVar,
    UndefinedVar,
    RedeclarationInSameScope
}

impl Display for ResolverErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
    pub fn resolve(&mut self, program: &mut [Stmt]) {
        for stmt in program {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::If(cond, if_stmt, else_stmt) => {
                self.resolve_expr(cond);
                self.resolve_stmt(if_stmt);
                if let Some(else_stmt) = else_stmt {
                    self.resolve_stmt(else_stmt);
                }
            }
            Stmt::While(cond, body) => {
                self.resolve_expr(cond);
                self.resolve_stmt(body);
            }
            Stmt::Expr(expr) | Stmt::Print(expr) | Stmt::Return(Some(expr)) => self.resolve_expr(expr),
            Stmt::Block(stmts) => {
                self.push_scope();
                for inner in stmts.iter_mut() {
                    self.resolve_stmt(inner);
                }
                self.pop_scope();
            }
            Stmt::Return(None) => {}
            Stmt::Declare(id, val) => {
                self.declare(id.clone());
                if let Some(val) = val {
                    self.resolve_expr(val);
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
                    self.resolve_stmt(inner);
                }
                self.pop_scope();
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Call(fun, args) => {
                self.resolve_expr(fun);
                for arg in args.iter_mut() {
                    self.resolve_expr(arg);
                }
            }
            Expr::Unary(_, r) => self.resolve_expr(r),
            Expr::Binary(_, l, r) => {
                self.resolve_expr(l);
                self.resolve_expr(r);
            }
            Expr::Literal(_) => {},
            Expr::Assignment(id, dist, val) => {
                self.resolve_expr(val);
                *dist = self.resolve_id(id).expect("Undefined var");
            }
            Expr::Variable(id, dist) => {
                if self.currently_declaring(id) {
                    println!("Can't use var in declaration");
                }
                *dist = self.resolve_id(id).expect("Undefined var");
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
