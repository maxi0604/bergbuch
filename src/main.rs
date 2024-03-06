use std::{
    env::args_os, fs, io::{stdin, stdout, IsTerminal, Write}, path::Path
};

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

    fn peek_nth(&self, n: usize) -> Option<char> {
        self.str.get(self.index + n).copied()
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

#[derive(Debug)]
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

    Identifier(String),
    String(String),
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
    run(content.as_str());
}

fn report_error(line: u64, err: &str) {
    println!("[{line}] {err}");
}

fn run_prompt() {
    if stdin().is_terminal() {
        print!("> ");
        stdout().flush().unwrap();
    }

    for line in stdin().lines() {
        run(&line.expect("Error reading stdin"));
        if stdin().is_terminal() {
            print!("> ");
            stdout().flush().unwrap();
        }
    }
}

fn run(code: &str) {
    let scanned = scan(code);
    dbg!(scanned);
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
                    Token::String(string)
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
                        _ => Token::Identifier(string),
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
