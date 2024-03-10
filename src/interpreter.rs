use crate::resolver::{Resolver, ResolverErr};
use crate::scanner::scan;
use crate::expr::{EvalErr, NativeCall, Val};
use crate::parser::{ParseErr, Parser};
use crate::scope::{Scope, ScopeLink};
use crate::statement::Stmt;
use std::{cell::RefCell, rc::Rc, fmt};

pub struct Interpreter {
    global_scope: ScopeLink,
    resolver: Resolver,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum InterpretErr {
    ParseErr(ParseErr),
    ResolverErrs(Vec<ResolverErr>),
    EvalErr(EvalErr),
}

impl fmt::Display for InterpretErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvalErr(x)  => write!(f, "{x}"),
            Self::ParseErr(x) => write!(f, "{x}"),
            Self::ResolverErrs(x) => {
                for err in x.iter() {
                    writeln!(f, "{err}")?;
                }
                Ok(())
            }
        }

    }

}

impl From<ParseErr> for InterpretErr {
    fn from(val: ParseErr) -> Self {
        InterpretErr::ParseErr(val)
    }
}

impl From<Vec<ResolverErr>> for InterpretErr {
    fn from(val: Vec<ResolverErr>) -> Self {
        InterpretErr::ResolverErrs(val)
    }
}

impl From<EvalErr> for InterpretErr {
    fn from(val: EvalErr) -> Self {
        InterpretErr::EvalErr(val)
    }
}

impl Interpreter {
    pub fn run(&mut self, code: &str) -> Result<(), InterpretErr> {
        let (scanned, _err) = scan(code);
        let mut parser = Parser::new(&scanned);
        let mut parsed = parser.parse()?;
        self.resolver.resolve(&mut parsed)?;
        self.interpret(&parsed)?;
        Ok(())
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> Result<(), EvalErr> {
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
            resolver: Resolver::new()
        }
    }

    pub fn get_global(&self, id: &str) -> Option<Val> {
        (*self.global_scope).borrow().try_get_here(id)
    }
}
