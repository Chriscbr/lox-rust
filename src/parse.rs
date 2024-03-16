use crate::{
    ast::{Binary, BinaryOp, Expr, Grouping, Literal, Stmt, Unary, UnaryOp, VarDecl, Variable},
    error::Error,
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<Error>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            let stmt = self.declaration();
            match stmt {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        Ok(stmts)
    }

    fn expr(&mut self) -> Result<Expr, Error> {
        self.equality()
    }

    fn declaration(&mut self) -> Result<Stmt, Error> {
        if self.matches(&[TokenType::Var]) {
            return self.var_declaration();
        }

        self.stmt()
    }

    fn stmt(&mut self) -> Result<Stmt, Error> {
        if self.matches(&[TokenType::Print]) {
            return self.print_statement();
        }

        self.expr_stmt()
    }

    fn print_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expr()?;
        if !self.check(TokenType::Semicolon) {
            return Err(Error::ExpectSemicolonAfterExpression {
                token: self.peek().clone(),
            });
        }
        self.advance();
        Ok(Stmt::Print(expr))
    }

    fn var_declaration(&mut self) -> Result<Stmt, Error> {
        if !self.check(TokenType::Identifier) {
            return Err(Error::ExpectedVariableName {
                token: self.peek().clone(),
            });
        }
        let name = self.advance().lexeme.clone();
        let initializer = if self.matches(&[TokenType::Equal]) {
            Some(self.expr()?)
        } else {
            None
        };

        if !self.check(TokenType::Semicolon) {
            return Err(Error::ExpectSemicolonAfterExpression {
                token: self.peek().clone(),
            });
        }
        self.advance();
        Ok(Stmt::VarDecl(VarDecl { name, initializer }))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.expr()?;
        if !self.check(TokenType::Semicolon) {
            return Err(Error::ExpectSemicolonAfterExpression {
                token: self.peek().clone(),
            });
        }
        self.advance();
        Ok(Stmt::Expr(expr))
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous().ty;
            let right = self.comparison()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while self.matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.previous().ty;
            let right = self.term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.previous().ty;
            let right = self.factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous().ty;
            let right = self.unary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().ty;
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                op: UnaryOp::from(op),
                right: Box::new(right),
            }));
        }

        return self.primary();
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        if self.matches(&[TokenType::False]) {
            return Ok(Expr::Literal(Literal::Bool(false)));
        }

        if self.matches(&[TokenType::True]) {
            return Ok(Expr::Literal(Literal::Bool(true)));
        }

        if self.matches(&[TokenType::Nil]) {
            return Ok(Expr::Literal(Literal::Nil));
        }

        if self.matches(&[TokenType::Number]) {
            let value = str::parse::<f64>(&self.previous().lexeme).expect("invalid float");
            return Ok(Expr::Literal(Literal::Number(value)));
        }

        if self.matches(&[TokenType::String]) {
            let s = &self.previous().lexeme;
            // Trim the surrounding quotes.
            let trimmed = &s[1..s.len() - 1];
            return Ok(Expr::Literal(Literal::String(trimmed.to_string())));
        }

        if self.matches(&[TokenType::Identifier]) {
            return Ok(Expr::Variable(Variable {
                name: self.previous().lexeme.clone(),
            }));
        }

        if self.matches(&[TokenType::LeftParen]) {
            let expr = self.expr()?;
            if !self.check(TokenType::RightParen) {
                return Err(Error::ExpectRightParenAfterExpr {
                    token: self.peek().clone(),
                });
            }
            self.advance();
            return Ok(Expr::Grouping(Grouping {
                expr: Box::new(expr),
            }));
        }

        todo!("expected expression");
    }

    fn matches(&mut self, tys: &[TokenType]) -> bool {
        for ty in tys {
            if self.check(*ty) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, ty: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        return self.peek().ty == ty;
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        return self.previous();
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().ty, TokenType::EOF)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if self.previous().ty == TokenType::Semicolon {
                return;
            }

            match self.peek().ty {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }
}
