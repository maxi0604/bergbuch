use std::rc::Rc;
#[derive(Debug, Clone)]
pub struct Token {
    pub data: TokenType,
    pub line: usize,
}

impl Token {
    pub fn new(data: TokenType, line: usize) -> Self {
        Token { data, line }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
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
    While,
}
