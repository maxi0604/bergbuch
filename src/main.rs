use core::fmt;
use std::{
    collections::HashMap, env::args_os, fmt::Display, fs, io::{stdin, stdout, IsTerminal, Write}, path::Path, rc::Rc
};

type ExprRef = Box<Expr>;
type ExprResult = Result<ExprRef, ParseErr>;

#[derive(Debug, Default)]
struct Scope<'a> {
    stack: HashMap<Rc<str>, Val>,
    parent: Option<&'a Scope<'a>>
}

impl<'a> Scope<'a> {
    fn get(&self, id: &str) -> Option<&Val> {
        self.stack.get(id).or_else(|| self.parent?.get(id))
    }

    fn define(&mut self, id: Rc<str>, val: Val) {
        self.stack.insert(id, val);
    }

    fn new_child(&self) -> Scope {
        Scope {
            stack: Default::default(),
            parent: Some(self)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Val {
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

#[derive(Debug, Clone)]
enum Expr {
    Binary(Token, ExprRef, ExprRef),
    Unary(Token, ExprRef),
    Literal(Val),
    Variable(Rc<str>),
    Assignment(Rc<str>, ExprRef)
    // This seems unnecessary so far.
    // Grouping(ExprRef)
}


#[derive(Debug, Clone)]
enum Stmt {
    Print(ExprRef),
    Expr(ExprRef),
    Declare(Rc<str>, Option<ExprRef>),
    Block(Vec<Stmt>),
}

struct Interpreter<'a> {
    global_scope: Scope<'a>
}

impl<'a> Interpreter<'a> {
    fn interpret(&mut self, program: impl IntoIterator<Item = Stmt>) -> Result<(), EvalError> {
        for stmt in program.into_iter() {
            stmt.exec(&mut self.global_scope)?;
        }
        Ok(())
    }

    fn new() -> Interpreter<'a> {
        Interpreter {
            global_scope: Default::default()
        }
    }
}

impl Stmt {
    fn exec(&self, scope: &mut Scope) -> Result<(), EvalError> {
        match self {
            Self::Print(expr) => println!("{}", expr.eval(scope)?),
            Self::Expr(expr) => { expr.eval(scope)?; },
            Self::Declare(id, val) => {
                if let Some(val) = val {
                    let val = val.eval(scope)?;
                    scope.define(id.clone(), val);
                } else {
                    scope.define(id.clone(), Val::Nil);
                }
            }
            Self::Block(stmts) => {
                let mut child = scope.new_child();
                for stmt in stmts.iter() {
                    stmt.exec(&mut child)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
enum EvalError {
    TypeError,
    UndefinedVariable
}

#[derive(Debug, Clone)]
enum ParseErr {
    UnexpectedToken,
    UnmatchedPair,
    NotLvalue,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError => write!(f, "Type error."),
            Self::UndefinedVariable => write!(f, "Undefined variable.")
        }
    }
}


impl Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken => write!(f, "Unexpected token."),
            Self::UnmatchedPair => write!(f, "Unmatched pair."),
            Self::NotLvalue => write!(f, "Left side is not assignable."),
        }
    }
}

impl Expr {
    fn eval(&self, scope: &mut Scope) -> Result<Val, EvalError> {
        match self {
            Self::Literal(v) => Ok(v.clone()),
            Self::Binary(op, x, y) => {
                let l = x.eval(scope)?;

                if *op == Token::Or && l == Val::Bool(true) || *op == Token::And && l == Val::Bool(false) {
                    return Ok(l);
                }

                let r = y.eval(scope)?;
                match (op, l, r) {
                    (Token::Plus, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a + b)),
                    (Token::Minus, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a - b)),
                    (Token::Star, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a * b)),
                    (Token::Slash, Val::Num(a), Val::Num(b)) => Ok(Val::Num(a / b)),

                    (Token::Less, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a < b)),
                    (Token::LessEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a <= b)),
                    (Token::EqualEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a == b)),
                    (Token::GreaterEqual, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a >= b)),
                    (Token::Greater, Val::Num(a), Val::Num(b)) => Ok(Val::Bool(a > b)),

                    (Token::Or, Val::Bool(_), Val::Bool(b)) => Ok(Val::Bool(b)),
                    (Token::And, Val::Bool(_), Val::Bool(b)) => Ok(Val::Bool(b)),

                    (Token::Plus, Val::String(a), Val::String(b)) => {
                        let mut c = a.to_string();
                        c.push_str(&b);
                        Ok(Val::String(c.into()))
                    },

                    (Token::EqualEqual, x, y) => Ok(Val::Bool(x == y)),
                    (Token::BangEqual, x, y) => Ok(Val::Bool(x != y)),

                    _ => Err(EvalError::TypeError)
                }
            }
            Self::Unary(op, x) => {
                let l = x.eval(scope)?;
                match (op, l) {
                    (Token::Bang, Val::Bool(a)) => Ok(Val::Bool(!a)),
                    (Token::Minus, Val::Num(a)) => Ok(Val::Num(-a)),
                    _ => Err(EvalError::TypeError)
                }
            }
            Self::Variable(id) => {
                scope.get(id).ok_or(EvalError::UndefinedVariable).cloned()
            },
            Self::Assignment(id, val) => {
                let r = val.eval(scope)?;
                scope.define(id.clone(), r.clone());
                Ok(r)
            }
            // Self::Grouping(exp) => exp.eval(),
            // _ => Err(EvalError::TypeError)
        }
    }
}
struct Scanner<'a> {
    // TODO: Sucky string representation given that we don't usually need to index further than 2 away
    // but it will have to do.
    str: &'a [char],
    index: usize,
}

impl<'a> Scanner<'a> {
    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        self.str.get(self.index - 1).copied()
    }

    // "match" is a keyword in the metalanguage already.
    fn match_next(&mut self, c: char) -> bool {
        match self.str.get(self.index) {
            Some(match_c) if *match_c == c => { self.index += 1; true }
            Some(_) | None => false
        }
    }

    fn peek(&self) -> Option<char> {
        self.str.get(self.index).copied()
    }

    fn new(str: &'a [char]) -> Self {
        Self {
            index: 0,
            str
        }
    }

    fn index(&self) -> usize {
        self.index
    }
}

struct Parser<'a> {
    // TODO: Sucky string representation given that we don't usually need to index further than 2 away
    // but it will have to do.
    tokens: &'a [Token],
    index: usize,
}

impl<'a> Parser<'a> {
    fn has_next(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn match_next_lits<const N: usize>(&mut self, ttypes: [Token; N]) -> bool {
        if self.index >= self.tokens.len() {
            return false;
        }

        let res = ttypes.iter().any(|x| *x == self.tokens[self.index]);
        if res {
            self.index += 1;
        }

        res
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn advance(&mut self) -> Option<&Token> {
        self.index += 1;
        self.tokens.get(self.index - 1)
    }

    fn check(&mut self, tok: &Token) -> bool {
        self.tokens.get(self.index) == Some(tok)
    }

    fn consume(&mut self, tok: &Token) -> Result<(), ParseErr> {
        if self.tokens.get(self.index) != Some(tok) {
            return Err(ParseErr::UnmatchedPair);
        }
        self.index += 1;

        Ok(())
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.index - 1]
    }

    fn new(tokens: &[Token]) -> Parser {
        Parser {
            index: 0,
            tokens
        }
    }

    // Parsing the actual grammar.
    fn program(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let mut res = vec![];
        while self.has_next() {
            res.push(self.declaration()?);
        }
        Ok(res)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseErr> {
        if self.match_next_lits([Token::Var]) {
            if let Some(Token::Identifier(id)) = self.advance() {
                let id = id.clone();
                if self.match_next_lits([Token::Equal]) {
                    let value = self.equality()?;
                    self.consume(&Token::Semicolon)?;
                    Ok(Stmt::Declare(id.into(), Some(value)))
                } else {
                    self.consume(&Token::Semicolon)?;
                    Ok(Stmt::Declare(id.into(), None))
                }
            } else {
                Err(ParseErr::NotLvalue)
            }
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseErr> {
        if self.match_next_lits([Token::Print]) {
            let val = self.print_statement();
            val
        } else if self.match_next_lits([Token::LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        }
        else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let mut res = vec![];
        while self.has_next() && !self.check(&Token::RightBrace) {
            res.push(self.declaration()?.clone());
        }
        self.consume(&Token::RightBrace)?;
        Ok(res)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseErr> {
        let res = self.expression();
        self.consume(&Token::Semicolon)?;
        Ok(Stmt::Print(res?))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseErr> {
        let res = self.expression();
        self.consume(&Token::Semicolon)?;
        Ok(Stmt::Expr(res?))
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.equality()?;
        if self.match_next_lits([Token::Equal]) {
            if let Token::Identifier(tok) = self.previous() {
                Ok(Box::new(Expr::Assignment(tok.clone(), expr)))
            } else {
                Err(ParseErr::NotLvalue)
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> ExprResult {
        let mut expr = self.comparison();

        while self.match_next_lits([Token::BangEqual, Token::EqualEqual]) {
            // TODO: op.clone clones the string literal. Maybe refcount these.
            let op = self.previous().clone();
            let right = self.comparison();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn comparison(&mut self) -> ExprResult {
        let mut expr = self.term();
        while self.match_next_lits([Token::Greater, Token::GreaterEqual, Token::Less, Token::LessEqual]) {
            let op = self.previous().clone();
            let right = self.term();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn term(&mut self) -> ExprResult {
        let mut expr = self.factor();

        while self.match_next_lits([Token::Plus, Token::Minus]) {
            let op = self.previous().clone();
            let right = self.factor();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }


        expr
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary();

        while self.match_next_lits([Token::Slash, Token::Star]) {
            let op = self.previous().clone();
            let right = self.unary();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn unary(&mut self) -> Result<ExprRef,ParseErr> {
        if self.match_next_lits([Token::Bang, Token::Minus]) {
            Ok(Box::new(Expr::Unary(self.previous().clone(), self.primary()?)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<ExprRef, ParseErr> {
        let res = match self.advance().ok_or(ParseErr::UnexpectedToken)? {
            Token::True => Expr::Literal(Val::Bool(true)),
            Token::False => Expr::Literal(Val::Bool(false)),
            Token::Nil => Expr::Literal(Val::Nil),
            Token::Number(x) => Expr::Literal(Val::Num(*x)),
            Token::String(x) => Expr::Literal(Val::String(x.clone())),
            Token::LeftParen => {
                let expr = *self.expression()?;
                self.consume(&Token::RightParen)?;
                expr
            }
            Token::Identifier(x) => Expr::Variable(x.clone()),
            // TODO: Error reporting, synchronize()
            _ => return Err(ParseErr::UnexpectedToken)
        };

        Ok(Box::new(res))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Slash,
    Semicolon,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(Rc<str>),
    String(Rc<str>),
    Number(f64),

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While
}

fn main() {
    if let Some(arg) = args_os().nth(1) {
        run_file(Path::new(&arg));
    } else {
        run_prompt();
    }
}

fn run_file(path: &Path) {
    let content = fs::read_to_string(path).expect("Error reading file.");
    let mut interpreter = Interpreter::new();
    run(content.as_str(), &mut interpreter);
}

fn report_error(line: u64, err: &str) {
    println!("[{line}] {err}");
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();
    if stdin().is_terminal() {
        print!("> ");
        stdout().flush().unwrap();
    }

    for line in stdin().lines() {
        run(&line.expect("Error reading stdin"), &mut interpreter);
        if stdin().is_terminal() {
            print!("> ");
            stdout().flush().unwrap();
        }
    }
}

fn run(code: &str, interpreter: &mut Interpreter) {
    let (scanned, _err) = scan(code);
    let mut parser = Parser::new(&scanned);
    let parsed = parser.program();
    if let Ok(parsed) = parsed {
        if let Err(err) = interpreter.interpret(parsed) {
            println!("{}", err);
        }
    }
}


fn scan(code: &str) -> (Vec<Token>, bool) {
    let chars = code.chars().collect::<Vec<_>>();
    let mut scanner = Scanner::new(&chars);
    let mut result = vec![];
    let mut line = 1;
    let mut error = false;

    loop {
        if let Some(c) = scanner.advance() {
            let tok = match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ',' => Token::Comma,
                '.' => Token::Dot,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                ';' => Token::Semicolon,
                '/' => if scanner.match_next('/') {
                    while !matches!(scanner.peek(), Some('\n') | None) {
                        scanner.advance();
                    };
                    continue;
                } else { Token::Slash },
                '>' => if scanner.match_next('=') { Token::GreaterEqual } else { Token::Greater },
                '=' => if scanner.match_next('=') { Token::EqualEqual } else { Token::Equal },
                '<' => if scanner.match_next('=') { Token::LessEqual } else { Token::Less },
                '!' => if scanner.match_next('=') { Token::BangEqual } else { Token::Bang },
                '\n' => { line += 1; continue; }
                '\r' | '\t' | ' ' => continue,
                '"' => {
                    let start = scanner.index();

                    while scanner.peek().is_some_and(|x| x != '"') {
                        scanner.advance();
                    }

                    if scanner.peek().is_none() {
                        report_error(line, "Unclosed string");
                        continue;
                    }

                    let range = &chars[start..scanner.index()];
                    // Consume closing " after getting index
                    scanner.advance();
                    let string = range.iter().collect::<String>();
                    Token::String(string.into())
                }
                '0'..='9' => {
                    let start = scanner.index();
                    while matches!(scanner.peek(), Some('.' | '0'..='9')) {
                        scanner.advance();
                    }

                    let range = &chars[start - 1..scanner.index()];
                    match range.iter().collect::<String>().parse() {
                        Ok(num) => Token::Number(num),
                        Err(e) => {
                            report_error(line, e.to_string().as_str());
                            error = true;
                            continue;
                        }
                    }
                }
                c if c.is_alphabetic() => {
                    let start = scanner.index();

                    while scanner.peek().is_some_and(char::is_alphanumeric) {
                        scanner.advance();
                    }

                    let range = &chars[start - 1..scanner.index()];
                    let string = range.iter().collect::<String>();
                    match string.as_str() {
                        "and" => Token::And,
                        "class" => Token::Class,
                        "else" => Token::Else,
                        "false" => Token::False,
                        "for" => Token::For,
                        "fun" => Token::Fun,
                        "if" => Token::If,
                        "nil" => Token::Nil,
                        "or" => Token::Or,
                        "print" => Token::Print,
                        "return" => Token::Return,
                        "super" => Token::Super,
                        "this" => Token::This,
                        "true" => Token::True,
                        "var" => Token::Var,
                        "while" => Token::While,
                        _ => Token::Identifier(string.into()),
                    }

                }
                c => {
                    report_error(line, format!("Unexpected character '{}'", c).as_str());
                    continue;
                }
            };

            result.push(tok);
        } else {
            return (result, error)
        }
    }
}
