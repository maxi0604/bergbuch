use std::{
    fmt::{self, Display, Write}, rc::Rc, cell::RefCell, time::{self, Duration},
};

use crate::{scope::{ScopeLink, Scope}, statement::{ExecInterruption, Stmt}};
use crate::token::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub enum NativeCall {
    Clock,
}

impl NativeCall {
    pub fn arity(&self) -> usize {
        match self {
            Self::Clock => 0,
        }
    }

    pub fn call(&self, vals: &[Val]) -> Result<Val, EvalError> {
        if self.arity() == vals.len() {
            match self {
                Self::Clock => Ok(Self::clock())
            }
        } else {
            Err(EvalError::WrongArgumentCount(self.arity(), vals.len()))
        }
    }

    fn clock() -> Val {
        Val::Num(time::SystemTime::now().duration_since(time::UNIX_EPOCH).unwrap_or(Duration::ZERO).as_secs_f64())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    String(Rc<str>),
    Num(f64),
    Bool(bool),
    LoxFunc(Vec<Rc<str>>, Vec<Stmt>, ScopeLink),
    NativeFunc(NativeCall),
    Nil,
}

impl Val {
    pub fn truthy(&self) -> bool {
        match self {
            Val::String(str) => str.len() > 0,
            Val::Num(x) => *x != 0.0 && !f64::is_nan(*x),
            Val::Bool(x) => *x,
            Val::NativeFunc(_) | Val::LoxFunc(..) => true,
            Val::Nil => false,
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(x) => write!(f, "{}", x),
            Self::Num(x) => write!(f, "{}", x),
            Self::Bool(x) => write!(f, "{}", x),
            Self::NativeFunc(_) => write!(f, "<native function>"),
            Self::LoxFunc(..) => write!(f, "<lox function>"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

pub type ExprRef = Box<Expr>;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(TokenType, ExprRef, ExprRef),
    Unary(TokenType, ExprRef),
    Literal(Val),
    Variable(Rc<str>, usize),
    Assignment(Rc<str>, usize, ExprRef),
    Call(ExprRef, Vec<ExprRef>),
}

impl Expr {
    pub fn eval(&self, scope: ScopeLink) -> Result<Val, EvalError> {
        match self {
            Self::Literal(v) => Ok(v.clone()),
            Self::Binary(op, x, y) => {
                let l = x.eval(scope.clone())?;

                if *op == TokenType::Or && l == Val::Bool(true)
                    || *op == TokenType::And && l == Val::Bool(false)
                {
                    return Ok(l);
                }

                let r = y.eval(scope.clone())?;
                match (op, l, r) {
                    (TokenType::Plus, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a + b)),
                    (TokenType::Minus, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a - b)),
                    (TokenType::Star, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a * b)),
                    (TokenType::Slash, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a / b)),

                    (TokenType::Less, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a < b)),
                    (TokenType::LessEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a <= b)),
                    (TokenType::GreaterEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a >= b)),
                    (TokenType::Greater, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a > b)),

                    (TokenType::Or, Val::Bool(_), Val::Bool(b)) => Ok(Val::Bool(b)),
                    (TokenType::And, Val::Bool(_), Val::Bool(b)) => Ok(Val::Bool(b)),

                    // The book only allow concatenating two strings or adding numbers.
                    // I allow stringifying values here.
                    (TokenType::Plus, Val::String(a), b) => {
                        let mut c = a.to_string();
                        // Writing to string can't fail.
                        let _ = write!(c, "{}", b);
                        Ok(Val::String(c.into()))
                    }

                    (TokenType::Plus, a, Val::String(b)) => {
                        let mut c = String::new();
                        // Writing to string can't fail.
                        let _ = write!(c, "{}", a);
                        c.push_str(&b);
                        Ok(Val::String(c.into()))
                    }

                    (TokenType::EqualEqual, x, y) => Ok(Val::Bool(x == y)),
                    (TokenType::BangEqual, x, y) => Ok(Val::Bool(x != y)),

                    _ => Err(EvalError::TypeError),
                }
            }
            Self::Unary(op, x) => {
                let l = x.eval(scope)?;
                match (op, l) {
                    (TokenType::Bang, Val::Bool(a)) => Ok(Val::Bool(!a)),
                    (TokenType::Minus, Val::Num(a)) => Ok(Val::Num(-a)),
                    _ => Err(EvalError::TypeError),
                }
            }
            Self::Variable(id, dist) => Ok((*scope)
                .borrow()
                .get(id, *dist)),
            Self::Assignment(id, dist, val) => {
                let r = val.eval(scope.clone())?;
                (*scope).borrow_mut().set(id.clone(), *dist, r.clone());
                Ok(r)
            } // Self::Grouping(exp) => exp.eval(),
            Self::Call(fun, args) => {
                let fun = fun.eval(scope.clone())?;

                match fun {
                    Val::LoxFunc(expected_args, fun, closure) => {
                        if args.len() != expected_args.len() {
                            return Err(EvalError::WrongArgumentCount(expected_args.len(), args.len()))
                        }
                        let child = Rc::new(RefCell::new(Scope::new_child(closure.clone())));
                        let mut bor = (*child).borrow_mut();
                        for (exp, arg) in expected_args.iter().zip(args) {
                            bor.declare(exp.clone(), arg.eval(scope.clone())?)
                        }
                        // Otherwise eval panics because a mutable borrow is held.
                        drop(bor);

                        for stmt in fun.iter() {
                            match stmt.exec(child.clone()) {
                                Ok(()) => continue,
                                Err(ExecInterruption::Err(e)) => return Err(e),
                                Err(ExecInterruption::Return(Some(val))) => return Ok(val),
                                Err(ExecInterruption::Return(None)) => return Ok(Val::Nil),
                            };
                        }

                        Ok(Val::Nil)
                    }
                    Val::NativeFunc(nc) => {
                        let mut evaluated = Vec::with_capacity(args.len());
                        for arg in args {
                            evaluated.push(arg.eval(scope.clone())?);
                        }
                        nc.call(&evaluated)
                    }
                    _ => Err(EvalError::TypeError)
                }
            }
              // _ => Err(EvalError::TypeError)
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    TypeError,
    UndefinedVariable,
    WrongArgumentCount(usize, usize),
    UnexpectedReturn
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError => write!(f, "Type error."),
            Self::UndefinedVariable => write!(f, "Undefined variable."),
            Self::WrongArgumentCount(exp, act) => write!(f, "Expected {exp} arguments, got {act}"),
            Self::UnexpectedReturn => write!(f, "`return` outside of function."),
        }
    }
}

impl From<ExecInterruption> for EvalError {
    fn from(value: ExecInterruption) -> Self {
        match value {
            ExecInterruption::Err(e) => e,
            ExecInterruption::Return(_) => Self::UnexpectedReturn,
        }
    }
}
