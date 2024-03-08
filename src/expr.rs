use std::{
    fmt, fmt::Display, rc::Rc
};

use crate::token::TokenType;
use crate::scope::ScopeLink;
#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    String(Rc<str>),
    Num(f64),
    Bool(bool),
    Nil
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(x) => write!(f, "{}", x),
            Self::Num(x) => write!(f, "{}", x),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Nil => write!(f, "nil")
        }
    }
}

pub type ExprRef = Box<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(TokenType, ExprRef, ExprRef),
    Unary(TokenType, ExprRef),
    Literal(Val),
    Variable(Rc<str>),
    Assignment(Rc<str>, ExprRef)
    // This seems unnecessary so far.
    // Grouping(ExprRef)
}

impl Expr {
    pub fn eval(&self, scope: ScopeLink) -> Result<Val, EvalError> {
        match self {
            Self::Literal(v) => Ok(v.clone()),
            Self::Binary(op, x, y) => {
                let l = x.eval(scope.clone())?;

                if *op == TokenType::Or && l == Val::Bool(true) || *op == TokenType::And && l == Val::Bool(false) {
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
                    (TokenType::EqualEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a == b)),
                    (TokenType::GreaterEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a >= b)),
                    (TokenType::Greater, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a > b)),

                    (TokenType::Or, Val::Bool(_), Val::Bool(b)) => Ok(Val::Bool(b)),
                    (TokenType::And, Val::Bool(_), Val::Bool(b)) => Ok(Val::Bool(b)),

                    (TokenType::Plus, Val::String(a), Val::String(b)) => {
                        let mut c = a.to_string();
                        c.push_str(&b);
                        Ok(Val::String(c.into()))
                    },

                    (TokenType::EqualEqual, x, y) => Ok(Val::Bool(x == y)),
                    (TokenType::BangEqual, x, y) => Ok(Val::Bool(x != y)),

                    _ => Err(EvalError::TypeError)
                }
            }
            Self::Unary(op, x) => {
                let l = x.eval(scope)?;
                match (op, l) {
                    (TokenType::Bang, Val::Bool(a)) => Ok(Val::Bool(!a)),
                    (TokenType::Minus, Val::Num(a)) => Ok(Val::Num(-a)),
                    _ => Err(EvalError::TypeError)
                }
            }
            Self::Variable(id) => {
                (*scope).borrow().get(id).ok_or(EvalError::UndefinedVariable)
            },
            Self::Assignment(id, val) => {
                let r = val.eval(scope.clone())?;
                (*scope).borrow_mut().define(id.clone(), r.clone());
                Ok(r)
            }
            // Self::Grouping(exp) => exp.eval(),
            // _ => Err(EvalError::TypeError)
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    TypeError,
    UndefinedVariable
}


impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError => write!(f, "Type error."),
            Self::UndefinedVariable => write!(f, "Undefined variable.")
        }
    }
}

