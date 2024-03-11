use std::{
    cell::RefCell,
    fmt::{self, Display, Write},
    rc::Rc,
    time::{self, Duration},
};

use crate::token::TokenType;
use crate::{
    scope::{Scope, ScopeLink},
    statement::{ExecInterruption, Stmt},
};

use rustc_hash::FxHashMap;
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

    pub fn call(&self, vals: &[Val]) -> Result<Val, EvalErr> {
        if self.arity() == vals.len() {
            match self {
                Self::Clock => Ok(Self::clock()),
            }
        } else {
            Err(EvalErr::WrongArgumentCount(self.arity(), vals.len()))
        }
    }

    fn clock() -> Val {
        Val::Num(
            time::SystemTime::now()
                .duration_since(time::UNIX_EPOCH)
                .unwrap_or(Duration::ZERO)
                .as_secs_f64(),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func(pub Rc<[Rc<str>]>, pub Rc<[Stmt]>, pub ScopeLink);
#[derive(Debug, PartialEq, Clone)]
pub struct Class(pub FxHashMap<Rc<str>, Func>);
#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    String(Rc<str>),
    Num(f64),
    Bool(bool),
    LoxFunc(Func),
    LoxClass(Rc<Class>),
    LoxInstance(Rc<Class>, Rc<RefCell<FxHashMap<Rc<str>, Val>>>),
    NativeFunc(NativeCall),
    Nil,
}

impl Val {
    pub fn truthy(&self) -> bool {
        match self {
            Val::String(str) => str.len() > 0,
            Val::Num(x) => *x != 0.0 && !f64::is_nan(*x),
            Val::Bool(x) => *x,
            Val::Nil => false,
            _ => true,
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
            Self::LoxClass(..) => write!(f, "<lox class>"),
            Self::LoxInstance(..) => write!(f, "<lox instance>"),
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
    Get(ExprRef, Rc<str>),
    Set(ExprRef, Rc<str>, ExprRef),
    This(usize),
}

impl Expr {
    pub fn eval(&self, scope: ScopeLink) -> Result<Val, EvalErr> {
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
                    #[cfg(not(feature = "compliant"))]
                    (TokenType::Plus, Val::String(a), b) => {
                        let mut c = a.to_string();
                        // Writing to string can't fail.
                        let _ = write!(c, "{}", b);
                        Ok(Val::String(c.into()))
                    }

                    #[cfg(not(feature = "compliant"))]
                    (TokenType::Plus, a, Val::String(b)) => {
                        let mut c = String::new();
                        // Writing to string can't fail.
                        let _ = write!(c, "{}", a);
                        c.push_str(&b);
                        Ok(Val::String(c.into()))
                    }

                    (TokenType::EqualEqual, x, y) => Ok(Val::Bool(x == y)),
                    (TokenType::BangEqual, x, y) => Ok(Val::Bool(x != y)),

                    _ => Err(EvalErr::TypeError),
                }
            }
            Self::Unary(op, x) => {
                let l = x.eval(scope)?;
                match (op, l) {
                    (TokenType::Bang, Val::Bool(a)) => Ok(Val::Bool(!a)),
                    (TokenType::Minus, Val::Num(a)) => Ok(Val::Num(-a)),
                    _ => Err(EvalErr::TypeError),
                }
            }
            Self::Variable(id, dist) => Ok((*scope).borrow().get(id, *dist)),
            Self::Assignment(id, dist, val) => {
                let r = val.eval(scope.clone())?;
                (*scope).borrow_mut().set(id.clone(), *dist, r.clone());
                Ok(r)
            } // Self::Grouping(exp) => exp.eval(),
            Self::Call(fun, args) => {
                let fun = fun.eval(scope.clone())?;

                match fun {
                    Val::LoxFunc(Func(expected_args, fun, closure)) => {
                        if args.len() != expected_args.len() {
                            return Err(EvalErr::WrongArgumentCount(
                                expected_args.len(),
                                args.len(),
                            ));
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
                    Val::LoxClass(class) => Ok(Val::LoxInstance(
                        class.clone(),
                        RefCell::new(FxHashMap::default()).into(),
                    )),
                    _ => Err(EvalErr::TypeError),
                }
            }
            Self::Get(target, id) => {
                let target = target.eval(scope.clone())?;
                let Val::LoxInstance(ref class, ref values) = target else {
                    return Err(EvalErr::TypeError);
                };
                let borrow = (*values).borrow();
                if let Some(val) = borrow.get(id) {
                    Ok(val.clone())
                } else if let Some(class_func) = class.0.get(id) {
                    let mut ret = class_func.clone();
                    let mut this_scope = Scope::new_child(scope.clone());
                    this_scope.declare("this".into(), target.clone());
                    ret.2 = Rc::new(RefCell::new(this_scope));
                    Ok(Val::LoxFunc(ret))
                } else {
                    Err(EvalErr::UndefinedVariable)
                }
            }
            Self::Set(target, id, val) => {
                let Val::LoxInstance(_, vals) = target.eval(scope.clone())? else {
                    return Err(EvalErr::TypeError);
                };
                let mut borrow = (*vals).borrow_mut();
                let val = val.eval(scope.clone())?;
                borrow.insert(id.clone(), val.clone());
                Ok(val)
            }
            Self::This(depth) => Ok((*scope).borrow().get("this", *depth)),
        }
    }
}

#[derive(Debug)]
pub enum EvalErr {
    TypeError,
    UndefinedVariable,
    WrongArgumentCount(usize, usize),
    UnexpectedReturn,
}

impl Display for EvalErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError => write!(f, "Type error."),
            Self::UndefinedVariable => write!(f, "Undefined variable."),
            Self::WrongArgumentCount(exp, act) => write!(f, "Expected {exp} arguments, got {act}"),
            Self::UnexpectedReturn => write!(f, "`return` outside of function."),
        }
    }
}

impl From<ExecInterruption> for EvalErr {
    fn from(value: ExecInterruption) -> Self {
        match value {
            ExecInterruption::Err(e) => e,
            ExecInterruption::Return(_) => Self::UnexpectedReturn,
        }
    }
}
