use crate::token::*;
use crate::expr::*;
use crate::statement::Stmt;

use std::{
    fmt,
    fmt::Display
};

type ExprResult = Result<ExprRef, ParseErr>;

#[derive(Debug, Clone)]
pub enum ParseErrType {
    UnexpectedToken,
    ExpectedToken(TokenType, Option<Token>),
    NotLvalue,
}

#[derive(Debug)]
pub struct ParseErr {
    data: ParseErrType,
    source: Option<Token>,
}


impl ParseErr {
    fn new(data: ParseErrType, source: Option<Token>) -> Self {
        ParseErr {
            data,
            source
        }
    }
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(src) = &self.source {
            write!(f, "[line {}] ", src.line)?;
        }
        match (&self.data, &self.source) {
            (ParseErrType::UnexpectedToken, Some(src)) => write!(f, "Unexpected token '{:?}'.", src.data),
            (ParseErrType::UnexpectedToken, None) => write!(f, "Unexpected token."),
            (ParseErrType::ExpectedToken(t, Some(act)), Some(src)) => write!(f,
                "Expected token '{:?}' to close {:?} [line {}] but got {:?}",
                t, src.data, src.line, act.data),
            (ParseErrType::ExpectedToken(t, None), Some(src)) => write!(f,
                "Expected token '{:?}' to close {:?} [line {}] but got no more token",
                t, src.data, src.line),
            (ParseErrType::ExpectedToken(t, None), None) => write!(f, "Expected token '{:?}', got EOF", t),
            (ParseErrType::ExpectedToken(t, Some(act)), None) => write!(f, "[line {}] Expected token '{:?}' but got {:?}", act.line, t, act.data),
            (ParseErrType::NotLvalue, Some(src)) => write!(f, "Left side ('{:?}') is not assignable.", src),
            (ParseErrType::NotLvalue, None) => write!(f, "Left side is not assignable."),
        }
    }
}

pub struct Parser<'a> {
    // TODO: Sucky string representation given that we don't usually need to index further than 2 away
    // but it will have to do.
    tokens: &'a [Token],
    index: usize,
}

impl<'a> Parser<'a> {
    fn has_next(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn match_next_lits<const N: usize>(&mut self, ttypes: [TokenType; N]) -> bool {
        if self.index >= self.tokens.len() {
            return false;
        }

        let res = ttypes.iter().any(|x| *x == self.tokens[self.index].data);
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

    fn check(&mut self, tok: &TokenType) -> bool {
        self.tokens.get(self.index).map(|x| &x.data) == Some(tok)
    }

    fn consume(&mut self, tok: &TokenType) -> Result<(), ParseErr> {

        let data = self.peek();
        if data.map(|x| &x.data) != Some(tok) {
            return Err(ParseErr::new(ParseErrType::ExpectedToken(tok.clone(), data.cloned()), None))
        }
        self.index += 1;

        Ok(())
    }

    fn consume_pair(&mut self, tok: &TokenType, other: &Token) -> Result<(), ParseErr> {

        let data = self.peek();
        if data.map(|x| &x.data) != Some(tok) {
            return Err(ParseErr::new(ParseErrType::ExpectedToken(tok.clone(), data.cloned()), Some(other.clone())))
        }
        self.index += 1;

        Ok(())
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.index - 1]
    }

    pub fn new(tokens: &[Token]) -> Parser {
        Parser {
            index: 0,
            tokens
        }
    }

    // Parsing the actual grammar.
    pub fn program(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let mut res = vec![];
        while self.has_next() {
            res.push(self.declaration()?);
        }
        Ok(res)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseErr> {
        if self.match_next_lits([TokenType::Var]) {
            if let Some(TokenType::Identifier(id)) = self.advance().map(|x| &x.data) {
                let id = id.clone();
                if self.match_next_lits([TokenType::Equal]) {
                    let value = self.equality()?;
                    self.consume(&TokenType::Semicolon)?;
                    Ok(Stmt::Declare(id.into(), Some(value)))
                } else {
                    self.consume(&TokenType::Semicolon)?;
                    Ok(Stmt::Declare(id.into(), None))
                }
            } else {
                Err(ParseErr::new(ParseErrType::NotLvalue, self.peek().cloned()))
            }
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseErr> {
        if self.match_next_lits([TokenType::Print]) {
            let val = self.print_statement();
            val
        } else if self.match_next_lits([TokenType::LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        }
        else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let left_brace = self.previous().clone();
        let mut res = vec![];
        while self.has_next() && !self.check(&TokenType::RightBrace) {
            res.push(self.declaration()?.clone());
        }
        self.consume_pair(&TokenType::RightBrace, &left_brace)?;
        Ok(res)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseErr> {
        let res = self.expression();
        self.consume(&TokenType::Semicolon)?;
        Ok(Stmt::Print(res?))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseErr> {
        let res = self.expression();
        self.consume(&TokenType::Semicolon)?;
        Ok(Stmt::Expr(res?))
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.equality();
        if self.match_next_lits([TokenType::Equal]) {
            let eq = self.previous().clone();
            let val = self.assignment();
            if let Expr::Variable(tok) = *expr?.clone() {
                Ok(Box::new(Expr::Assignment(tok.clone(), val?)))
            } else {
                Err(ParseErr::new(ParseErrType::NotLvalue, Some(eq)))
            }
        } else {
            expr
        }
    }

    fn equality(&mut self) -> ExprResult {
        let mut expr = self.comparison();

        while self.match_next_lits([TokenType::BangEqual, TokenType::EqualEqual]) {
            // TODO: op.clone clones the string literal. Maybe refcount these.
            let op = self.previous().clone().data;
            let right = self.comparison();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn comparison(&mut self) -> ExprResult {
        let mut expr = self.term();
        while self.match_next_lits([TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let op = self.previous().clone().data;
            let right = self.term();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn term(&mut self) -> ExprResult {
        let mut expr = self.factor();

        while self.match_next_lits([TokenType::Plus, TokenType::Minus]) {
            let op = self.previous().clone().data;
            let right = self.factor();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }


        expr
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary();

        while self.match_next_lits([TokenType::Slash, TokenType::Star]) {
            let op = self.previous().clone().data;
            let right = self.unary();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn unary(&mut self) -> Result<ExprRef, ParseErr> {
        if self.match_next_lits([TokenType::Bang, TokenType::Minus]) {
            Ok(Box::new(Expr::Unary(self.previous().data.clone(), self.primary()?)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<ExprRef, ParseErr> {
        let tok = self.advance().ok_or(ParseErr::new(ParseErrType::UnexpectedToken, None))?;
        let res = match &tok.data.clone() {
            TokenType::True => Expr::Literal(Val::Bool(true)),
            TokenType::False => Expr::Literal(Val::Bool(false)),
            TokenType::Nil => Expr::Literal(Val::Nil),
            TokenType::Number(x) => Expr::Literal(Val::Num(*x)),
            TokenType::String(x) => Expr::Literal(Val::String(x.clone())),
            TokenType::LeftParen => {
                let clone = tok.clone();
                let expr = *self.expression()?;
                self.consume_pair(&TokenType::RightParen, &clone)?;
                expr
            }
            TokenType::Identifier(x) => Expr::Variable(x.clone()),
            // TODO: Error reporting, synchronize()
            _ => return Err(ParseErr::new(ParseErrType::UnexpectedToken, Some(self.previous().clone())))
        };

        Ok(Box::new(res))
    }
}
