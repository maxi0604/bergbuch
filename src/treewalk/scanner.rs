use super::token::*;
struct Scanner<'a> {
    // TODO: Sucky string representation given that we don't usually need to index further than 2 away
    // but it will have to do.
    str: &'a [char],
    index: usize,
}

fn report_error(line: usize, err: &str) {
    println!("[{line}] {err}");
}

impl<'a> Scanner<'a> {
    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        self.str.get(self.index - 1).copied()
    }

    // "match" is a keyword in the metalanguage already.
    fn match_next(&mut self, c: char) -> bool {
        let res = self.str.get(self.index).is_some_and(|d| c == *d);
        if res {
            self.index += 1;
        }
        res
    }

    fn peek(&self) -> Option<char> {
        self.str.get(self.index).copied()
    }

    fn new(str: &'a [char]) -> Self {
        Self { index: 0, str }
    }

    fn index(&self) -> usize {
        self.index
    }
}

pub fn scan(code: &str) -> (Vec<Token>, bool) {
    let chars = code.chars().collect::<Vec<_>>();
    let mut scanner = Scanner::new(&chars);
    let mut result = vec![];
    let mut line: usize = 1;
    let mut error = false;

    loop {
        if let Some(c) = scanner.advance() {
            let tok = match c {
                '(' => TokenType::LeftParen,
                ')' => TokenType::RightParen,
                '{' => TokenType::LeftBrace,
                '}' => TokenType::RightBrace,
                ',' => TokenType::Comma,
                '.' => TokenType::Dot,
                '+' => TokenType::Plus,
                '-' => TokenType::Minus,
                '*' => TokenType::Star,
                ';' => TokenType::Semicolon,
                '/' => {
                    if scanner.match_next('/') {
                        while !matches!(scanner.peek(), Some('\n') | None) {
                            scanner.advance();
                        }
                        continue;
                    }
                    TokenType::Slash
                }
                '>' => {
                    if scanner.match_next('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    }
                }
                '=' => {
                    if scanner.match_next('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    }
                }
                '<' => {
                    if scanner.match_next('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    }
                }
                '!' => {
                    if scanner.match_next('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    }
                }
                '\n' => {
                    line += 1;
                    continue;
                }
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
                    TokenType::String(string.into())
                }
                '0'..='9' => {
                    let start = scanner.index();
                    while matches!(scanner.peek(), Some('.' | '0'..='9')) {
                        scanner.advance();
                    }

                    let range = &chars[start - 1..scanner.index()];
                    match range.iter().collect::<String>().parse() {
                        Ok(num) => TokenType::Number(num),
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
                        "and" => TokenType::And,
                        "class" => TokenType::Class,
                        "else" => TokenType::Else,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "nil" => TokenType::Nil,
                        "or" => TokenType::Or,
                        "print" => TokenType::Print,
                        "return" => TokenType::Return,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        _ => TokenType::Identifier(string.into()),
                    }
                }
                c => {
                    report_error(line, format!("Unexpected character '{}'", c).as_str());
                    continue;
                }
            };

            result.push(Token::new(tok, line));
        } else {
            return (result, error);
        }
    }
}
