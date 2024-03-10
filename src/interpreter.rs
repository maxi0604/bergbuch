use crate::resolver::{Resolver, ResolverErr};
use crate::scanner::scan;
use crate::expr::{EvalError, NativeCall, Val};
use crate::parser::{ParseErr, Parser};
use crate::scope::{Scope, ScopeLink};
use crate::statement::Stmt;
use std::{cell::RefCell, rc::Rc, fmt};

pub struct Interpreter {
    global_scope: ScopeLink,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum InterpretErr {
    ParseError(ParseErr),
    ResolverErr(ResolverErr),
    EvalErr(EvalError),
}

impl fmt::Display for InterpretErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalErr(x)  => write!(f, "{x}"),
            Self::ParseError(x) => write!(f, "{x}"),
            Self::ResolverErr(x) => write!(f, "{x}"),
        }

    }

}

impl From<ParseErr> for InterpretErr {
    fn from(val: ParseErr) -> Self {
        InterpretErr::ParseError(val)
    }
}

impl From<ResolverErr> for InterpretErr {
    fn from(val: ResolverErr) -> Self {
        InterpretErr::ResolverErr(val)
    }
}

impl From<EvalError> for InterpretErr {
    fn from(val: EvalError) -> Self {
        InterpretErr::EvalErr(val)
    }
}

impl Interpreter {
    pub fn run(&mut self, code: &str) -> Result<(), InterpretErr> {
        let (scanned, _err) = scan(code);
        let mut parser = Parser::new(&scanned);
        let mut parsed = parser.parse()?;
        let mut resolver = Resolver::new();
        resolver.resolve(&mut parsed);
        if let Err(err) = self.interpret(&parsed) {
            println!("{}", err);
            Err(err.into())
        } else {
            Ok(())
        }
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> Result<(), EvalError> {
        for stmt in program.iter() {
            stmt.exec(self.global_scope.clone())?;
        }
        Ok(())
    }

    pub fn new() -> Interpreter {
        let mut global_scope = Scope::default();
        global_scope.declare("clock".into(), Val::NativeFunc(NativeCall::Clock));
        Interpreter {
            global_scope: Rc::new(RefCell::new(global_scope)),
        }
    }

    pub fn get_global(&self, id: &str) -> Option<Val> {
        (*self.global_scope).borrow().try_get_here(id)
    }
}
