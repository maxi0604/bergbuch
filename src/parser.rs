use crate::expr::*;
use crate::statement::Stmt;
use crate::token::*;

use std::{fmt, fmt::Display, rc::Rc};

type ExprResult = Result<ExprRef, ParseErr>;

#[derive(Debug, Clone)]
pub enum ParseErrType {
    UnexpectedToken,
    ExpectedToken(TokenType, Option<Token>),
    NotLvalue,
}

#[derive(Debug, Clone)]
pub struct ParseErr {
    data: ParseErrType,
    source: Option<Token>,
}

impl ParseErr {
    fn new(data: ParseErrType, source: Option<Token>) -> Self {
        ParseErr { data, source }
    }
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(src) = &self.source {
            write!(f, "[line {}] ", src.line)?;
        }
        match (&self.data, &self.source) {
            (ParseErrType::UnexpectedToken, Some(src)) => {
                write!(f, "Unexpected token '{:?}'.", src.data)
            }
            (ParseErrType::UnexpectedToken, None) => write!(f, "Unexpected token."),
            (ParseErrType::ExpectedToken(t, Some(act)), Some(src)) => write!(
                f,
                "Expected token '{:?}' to close {:?} [line {}] but got {:?}",
                t, src.data, src.line, act.data
            ),
            (ParseErrType::ExpectedToken(t, None), Some(src)) => write!(
                f,
                "Expected token '{:?}' to close {:?} [line {}] but got no more token",
                t, src.data, src.line
            ),
            (ParseErrType::ExpectedToken(t, None), None) => {
                write!(f, "Expected token '{:?}', got EOF", t)
            }
            (ParseErrType::ExpectedToken(t, Some(act)), None) => write!(
                f,
                "[line {}] Expected token '{:?}' but got {:?}",
                act.line, t, act.data
            ),
            (ParseErrType::NotLvalue, Some(src)) => {
                write!(f, "Left side ('{:?}') is not assignable.", src)
            }
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

    fn consume(&mut self, tok: &TokenType) -> Result<&Token, ParseErr> {
        let data = self.peek();
        if data.map(|x| &x.data) != Some(tok) {
            return Err(ParseErr::new(
                ParseErrType::ExpectedToken(tok.clone(), data.cloned()),
                None,
            ));
        }
        self.index += 1;

        Ok(self.previous())
    }

    // TODO: Hack
    fn consume_identifier(&mut self) -> Result<Rc<str>, ParseErr> {
        let data = self.peek();
        if let Some(TokenType::Identifier(id)) = data.map(|x| &x.data) {
            let ret = id.clone();
            self.index += 1;
            Ok(ret)
        } else {
            Err(ParseErr::new(
                ParseErrType::ExpectedToken(TokenType::Identifier("".into()), data.cloned()),
                None,
            ))
        }

    }

    fn consume_pair(&mut self, tok: &TokenType, other: &Token) -> Result<(), ParseErr> {
        let data = self.peek();
        if data.map(|x| &x.data) != Some(tok) {
            return Err(ParseErr::new(
                ParseErrType::ExpectedToken(tok.clone(), data.cloned()),
                Some(other.clone()),
            ));
        }
        self.index += 1;

        Ok(())
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.index - 1]
    }

    pub fn new(tokens: &[Token]) -> Parser {
        Parser { index: 0, tokens }
    }

    // Parsing the actual grammar.
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let mut res = vec![];
        while self.has_next() {
            res.push(self.declaration()?);
        }
        Ok(res)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseErr> {
        if self.match_next_lits([TokenType::Var]) {
            self.var_declaration()
        } else if self.match_next_lits([TokenType::Fun]) {
            self.function()
        } else if self.match_next_lits([TokenType::Class]) {
            self.class()
        }
        else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseErr> {
        if let Some(TokenType::Identifier(id)) = self.advance().map(|x| &x.data) {
            let id = id.clone();
            if self.match_next_lits([TokenType::Equal]) {
                let value = self.equality()?;
                self.consume(&TokenType::Semicolon)?;
                Ok(Stmt::Declare(id, Some(value)))
            } else {
                self.consume(&TokenType::Semicolon)?;
                Ok(Stmt::Declare(id, None))
            }
        } else {
            Err(ParseErr::new(ParseErrType::NotLvalue, self.peek().cloned()))
        }

    }

    fn class(&mut self) -> Result<Stmt, ParseErr> {
        let id = self.consume_identifier()?;
        let left_brace = self.consume(&TokenType::LeftBrace)?.clone();
        let mut funs = vec![];

        while !self.check(&TokenType::RightBrace) && self.has_next() {
            funs.push(self.function()?);
        }

        self.consume_pair(&TokenType::RightBrace, &left_brace)?;

        Ok(Stmt::Class(id, funs))

    }

    fn function(&mut self) -> Result<Stmt, ParseErr> {
        let id = self.consume_identifier()?;
        let left = self.consume(&TokenType::LeftParen)?.clone();
        let mut params = vec![];
        if !self.check(&TokenType::RightParen) {
            params.push(self.consume_identifier()?);
            while self.match_next_lits([TokenType::Comma]) {
                params.push(self.consume_identifier()?);
            }
        }

        self.consume_pair(&TokenType::RightParen, &left)?;
        self.consume(&TokenType::LeftBrace)?;

        let fun = self.block()?;

        Ok(Stmt::Fun(id, params, fun))
    }

    fn statement(&mut self) -> Result<Stmt, ParseErr> {
        if self.match_next_lits([TokenType::Print]) {
            self.print_statement()
        } else if self.match_next_lits([TokenType::If]) {
            self.if_statement()
        } else if self.match_next_lits([TokenType::While]) {
            self.while_statement()
        } else if self.match_next_lits([TokenType::For]) {
            self.for_statement()
        } else if self.match_next_lits([TokenType::Return]) {
            self.return_statement()
        } else if self.match_next_lits([TokenType::LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseErr> {
        let val = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon)?;

        Ok(Stmt::Return(val))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseErr> {
        let left_brace = self.consume(&TokenType::LeftParen)?.clone();
        let cond = self.expression()?;
        self.consume_pair(&TokenType::RightParen, &left_brace)?;
        let stmt = self.statement()?;
        if self.match_next_lits([TokenType::Else]) {
            let other = self.statement()?;
            Ok(Stmt::If(cond, Box::new(stmt), Some(Box::new(other))))
        } else {
            Ok(Stmt::If(cond, Box::new(stmt), None))
        }
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseErr> {
        let left_paren = self.consume(&TokenType::LeftParen)?.clone();
        let mut result = vec![];
        if self.match_next_lits([TokenType::Var]) {
            result.push(self.var_declaration()?);
        } else if !self.match_next_lits([TokenType::Semicolon]) {
            result.push(self.expression_statement()?);
        }

        let cond = if self.check(&TokenType::Semicolon) {
            Box::new(Expr::Literal(Val::Bool(true)))
        } else {
            self.expression()?
        };

        self.consume(&TokenType::Semicolon)?;

        let increment = if self.check(&TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };


        self.consume_pair(&TokenType::RightParen, &left_paren)?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }

        result.push(Stmt::While(cond, Box::new(body)));
        Ok(Stmt::Block(result))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseErr> {
        let left_brace = self.consume(&TokenType::LeftParen)?.clone();
        let cond = self.expression()?;
        self.consume_pair(&TokenType::RightParen, &left_brace)?;
        let stmt = self.statement()?;
        Ok(Stmt::While(cond, Box::new(stmt)))
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
        let expr = self.logic_or()?;
        if self.match_next_lits([TokenType::Equal]) {
            let val = self.logic_or();
            if let Expr::Variable(tok, _) = (*expr).clone() {
                Ok(Box::new(Expr::Assignment(tok.clone(), 0, val?)))
            } else if let Expr::Get(target, id) = (*expr).clone() {
                Ok(Box::new(Expr::Set(target, id, val?)))
            } else {
                Err(ParseErr::new(ParseErrType::NotLvalue, None))
            }
        } else {
            Ok(expr)
        }
    }

    fn logic_or(&mut self) -> ExprResult {
        let mut expr = self.logic_and();

        while self.match_next_lits([TokenType::Or]) {
            // TODO: op.clone clones the string literal. Maybe refcount these.
            let op = self.previous().clone().data;
            let right = self.logic_and();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr
    }

    fn logic_and(&mut self) -> ExprResult {
        let mut expr = self.equality();

        while self.match_next_lits([TokenType::And]) {
            // TODO: op.clone clones the string literal. Maybe refcount these.
            let op = self.previous().clone().data;
            let right = self.equality();
            expr = Ok(Box::new(Expr::Binary(op, expr?, right?)));
        }

        expr

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
        while self.match_next_lits([
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
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
            Ok(Box::new(Expr::Unary(
                self.previous().data.clone(),
                self.unary()?,
            )))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ExprResult {
        let mut expr = self.primary()?;
        loop {
            if self.match_next_lits([TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_next_lits([TokenType::Dot]) {
                expr = self.get_expr(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: ExprRef) -> ExprResult {
        let mut args = vec![];
        if !self.check(&TokenType::RightParen) {
            args.push(self.expression()?);
            while self.match_next_lits([TokenType::Comma]) {
                args.push(self.expression()?);
            }
        }

        self.consume(&TokenType::RightParen)?;

        Ok(Box::new(Expr::Call(callee, args)))
    }

    fn get_expr(&mut self, target: ExprRef) -> ExprResult {
        let id = self.consume_identifier()?;

        Ok(Box::new(Expr::Get(target, id)))
    }

    fn primary(&mut self) -> Result<ExprRef, ParseErr> {
        let tok = self
            .advance()
            .ok_or(ParseErr::new(ParseErrType::UnexpectedToken, None))?;
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
            TokenType::Identifier(x) => Expr::Variable(x.clone(), 0),
            // TODO: Error reporting, synchronize()
            _ => {
                return Err(ParseErr::new(
                    ParseErrType::UnexpectedToken,
                    Some(self.previous().clone()),
                ))
            }
        };

        Ok(Box::new(res))
    }
}
