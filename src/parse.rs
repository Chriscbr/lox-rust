use std::{fmt::Display, sync::Arc};

use crate::{
    ast::{
        Assign, Binary, BinaryOp, Call, Expr, Function, Grouping, If, Literal, Logical, LogicalOp,
        Print, Return, Stmt, Unary, UnaryOp, VarDecl, Variable, While,
    },
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<(ParseError, Token)>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: vec![],
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, Vec<(ParseError, Token)>> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            let stmt = self.parse_declaration();
            match stmt {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        if self.errors.is_empty() {
            Ok(stmts)
        } else {
            Err(self.errors)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, (ParseError, Token)> {
        self.parse_assignment()
    }

    fn parse_declaration(&mut self) -> Result<Stmt, (ParseError, Token)> {
        if self.matches(&[TokenType::Fun]) {
            return self.parse_function(FunctionKind::Function);
        }

        if self.matches(&[TokenType::Var]) {
            return self.parse_var_decl();
        }

        self.parse_stmt()
    }

    fn parse_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        if self.matches(&[TokenType::For]) {
            return self.parse_for_stmt();
        }

        if self.matches(&[TokenType::If]) {
            return self.parse_if_stmt();
        }

        if self.matches(&[TokenType::Print]) {
            return self.parse_print_stmt();
        }

        if self.matches(&[TokenType::Return]) {
            return self.parse_return_stmt();
        }

        if self.matches(&[TokenType::While]) {
            return self.parse_while_stmt();
        }

        if self.matches(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block(self.parse_block()?));
        }

        self.parse_expr_stmt()
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        if !self.check(TokenType::LeftParen) {
            return Err((ParseError::ExpectedLeftParenAfterFor, self.peek().clone()));
        }
        self.advance();

        let initializer = if self.matches(&[TokenType::Semicolon]) {
            None
        } else if self.matches(&[TokenType::Var]) {
            Some(self.parse_var_decl()?)
        } else {
            Some(self.parse_expr_stmt()?)
        };

        let condition = if !self.check(TokenType::Semicolon) {
            self.parse_expr()?
        } else {
            Expr::Literal(Literal::Bool(true))
        };

        if !self.check(TokenType::Semicolon) {
            return Err((
                ParseError::ExpectedSemicolonAfterLoopCondition,
                self.peek().clone(),
            ));
        }
        self.advance();

        let increment = if !self.check(TokenType::RightParen) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if !self.check(TokenType::RightParen) {
            return Err((
                ParseError::ExpectedRightParenAfterForLoop,
                self.peek().clone(),
            ));
        }
        self.advance();

        let mut body = self.parse_stmt()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }

        body = Stmt::While(While {
            condition: Box::new(condition),
            body: Box::new(body),
        });

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        if !self.check(TokenType::LeftParen) {
            return Err((ParseError::ExpectedLeftParenAfterIf, self.peek().clone()));
        }
        self.advance();

        let condition = self.parse_expr()?;

        if !self.check(TokenType::RightParen) {
            return Err((
                ParseError::ExpectedRightParenAfterIfCondition,
                self.peek().clone(),
            ));
        }
        self.advance();

        let then_branch = self.parse_stmt()?;
        let else_branch = if self.matches(&[TokenType::Else]) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(Stmt::If(If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        }))
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        let expr = self.parse_expr()?;
        if !self.check(TokenType::Semicolon) {
            return Err((
                ParseError::ExpectedSemicolonAfterExpression,
                self.peek().clone(),
            ));
        }
        self.advance();
        Ok(Stmt::Print(Print {
            expr: Box::new(expr),
        }))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        let value = if !self.check(TokenType::Semicolon) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if !self.check(TokenType::Semicolon) {
            return Err((
                ParseError::ExpectedSemicolonAfterReturn,
                self.peek().clone(),
            ));
        }
        self.advance();
        Ok(Stmt::Return(Return { value }))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        if !self.check(TokenType::LeftParen) {
            return Err((ParseError::ExpectedLeftParenAfterWhile, self.peek().clone()));
        }
        self.advance();

        let condition = self.parse_expr()?;

        if !self.check(TokenType::RightParen) {
            return Err((
                ParseError::ExpectedRightParenAfterWhileCondition,
                self.peek().clone(),
            ));
        }
        self.advance();

        let body = self.parse_stmt()?;

        Ok(Stmt::While(While {
            condition: Box::new(condition),
            body: Box::new(body),
        }))
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, (ParseError, Token)> {
        if !self.check(TokenType::Identifier) {
            return Err((ParseError::ExpectedVariableName, self.peek().clone()));
        }
        let name = self.advance().lexeme.clone();
        let initializer = if self.matches(&[TokenType::Equal]) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if !self.check(TokenType::Semicolon) {
            return Err((
                ParseError::ExpectedSemicolonAfterExpression,
                self.peek().clone(),
            ));
        }
        self.advance();
        Ok(Stmt::VarDecl(VarDecl { name, initializer }))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, (ParseError, Token)> {
        let expr = self.parse_expr()?;
        if !self.check(TokenType::Semicolon) {
            return Err((
                ParseError::ExpectedSemicolonAfterExpression,
                self.peek().clone(),
            ));
        }
        self.advance();
        Ok(Stmt::Expr(expr))
    }

    fn parse_function(&mut self, kind: FunctionKind) -> Result<Stmt, (ParseError, Token)> {
        if !self.check(TokenType::Identifier) {
            let token = self.peek().clone();
            match kind {
                FunctionKind::Function => {
                    return Err((ParseError::ExpectedFunctionName, token));
                }
                FunctionKind::Method => {
                    return Err((ParseError::ExpectedMethodName, token));
                }
            }
        }
        let name = self.advance().lexeme.clone();

        if !self.check(TokenType::LeftParen) {
            let token = self.peek().clone();
            return match kind {
                FunctionKind::Function => {
                    Err((ParseError::ExpectedLeftParenAfterFunctionName, token))
                }
                FunctionKind::Method => Err((ParseError::ExpectedLeftParenAfterMethodName, token)),
            };
        }
        self.advance();

        let mut params = vec![];
        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err((ParseError::TooManyParameters, self.peek().clone()));
                }

                if !self.check(TokenType::Identifier) {
                    return Err((ParseError::ExpectedParameterName, self.peek().clone()));
                }
                params.push(self.advance().lexeme.clone());

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        if !self.check(TokenType::RightParen) {
            return Err((
                ParseError::ExpectedRightParenAfterParameters,
                self.peek().clone(),
            ));
        }
        self.advance();

        if !self.check(TokenType::LeftBrace) {
            let token = self.peek().clone();
            match kind {
                FunctionKind::Function => {
                    return Err((ParseError::ExpectedLeftBraceAfterFunction, token));
                }
                FunctionKind::Method => {
                    return Err((ParseError::ExpectedLeftBraceAfterMethod, token));
                }
            }
        }
        self.advance();

        let body = self.parse_block()?;

        Ok(Stmt::Function(Arc::new(Function { name, params, body })))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, (ParseError, Token)> {
        let mut stmts = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let stmt = self.parse_declaration();
            match stmt {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        if !self.check(TokenType::RightBrace) {
            return Err((
                ParseError::ExpectedRightBraceAfterBlock,
                self.peek().clone(),
            ));
        }
        self.advance();
        Ok(stmts)
    }

    fn parse_assignment(&mut self) -> Result<Expr, (ParseError, Token)> {
        let expr = self.parse_or()?;

        if self.matches(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.parse_assignment()?;

            if let Expr::Variable(v) = expr {
                return Ok(Expr::Assign(Assign {
                    name: v.name,
                    value: Box::new(value),
                }));
            }

            return Err((ParseError::InvalidAssignmentTarget, equals.clone()));
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_and()?;

        while self.matches(&[TokenType::Or]) {
            let op = self.previous().ty;
            let right = self.parse_and()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                op: LogicalOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_equality()?;

        while self.matches(&[TokenType::And]) {
            let op = self.previous().ty;
            let right = self.parse_equality()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                op: LogicalOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_comparison()?;

        while self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous().ty;
            let right = self.parse_comparison()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_term()?;

        while self.matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.previous().ty;
            let right = self.parse_term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_factor()?;

        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.previous().ty;
            let right = self.parse_factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_unary()?;

        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous().ty;
            let right = self.parse_unary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: BinaryOp::from(op),
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, (ParseError, Token)> {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().ty;
            let right = self.parse_unary()?;
            return Ok(Expr::Unary(Unary {
                op: UnaryOp::from(op),
                right: Box::new(right),
            }));
        }

        return self.parse_call();
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, (ParseError, Token)> {
        let mut args = vec![];

        if !self.check(TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    return Err((ParseError::TooManyArguments, self.peek().clone()));
                }

                args.push(self.parse_expr()?);
                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        if !self.check(TokenType::RightParen) {
            return Err((
                ParseError::ExpectedRightParenAfterArguments,
                self.peek().clone(),
            ));
        }
        self.advance();

        Ok(Expr::Call(Call {
            callee: Box::new(callee),
            args,
        }))
    }

    fn parse_call(&mut self) -> Result<Expr, (ParseError, Token)> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, (ParseError, Token)> {
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
            let expr = self.parse_expr()?;
            if !self.check(TokenType::RightParen) {
                return Err((ParseError::ExpectedRightParenAfterExpr, self.peek().clone()));
            }
            self.advance();
            return Ok(Expr::Grouping(Grouping {
                expr: Box::new(expr),
            }));
        }

        todo!("Expected expression");
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

enum FunctionKind {
    Function,
    Method,
}

pub enum ParseError {
    ExpectedRightParenAfterExpr,
    ExpectedSemicolonAfterExpression,
    ExpectedVariableName,
    InvalidAssignmentTarget,
    ExpectedRightBraceAfterBlock,
    ExpectedLeftParenAfterIf,
    ExpectedRightParenAfterIfCondition,
    ExpectedLeftParenAfterWhile,
    ExpectedRightParenAfterWhileCondition,
    ExpectedLeftParenAfterFor,
    ExpectedSemicolonAfterLoopCondition,
    ExpectedRightParenAfterForLoop,
    TooManyArguments,
    ExpectedRightParenAfterArguments,
    ExpectedFunctionName,
    ExpectedMethodName,
    ExpectedLeftParenAfterFunctionName,
    ExpectedLeftParenAfterMethodName,
    TooManyParameters,
    ExpectedParameterName,
    ExpectedRightParenAfterParameters,
    ExpectedLeftBraceAfterFunction,
    ExpectedLeftBraceAfterMethod,
    ExpectedSemicolonAfterReturn,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ParseError::ExpectedRightParenAfterExpr => "Expected ')' after expression".to_string(),
            ParseError::ExpectedSemicolonAfterExpression => {
                "Expected ';' after expression".to_string()
            }
            ParseError::ExpectedVariableName => "Expected variable name".to_string(),
            ParseError::InvalidAssignmentTarget => "Invalid assignment target".to_string(),
            ParseError::ExpectedRightBraceAfterBlock => "Expected '}' after block".to_string(),
            ParseError::ExpectedLeftParenAfterIf => "Expected '(' after 'if'".to_string(),
            ParseError::ExpectedRightParenAfterIfCondition => {
                "Expected ')' after if condition".to_string()
            }
            ParseError::ExpectedLeftParenAfterWhile => "Expected '(' after 'while'".to_string(),
            ParseError::ExpectedRightParenAfterWhileCondition => {
                "Expected ')' after while condition".to_string()
            }
            ParseError::ExpectedLeftParenAfterFor => "Expected '(' after 'for'".to_string(),
            ParseError::ExpectedSemicolonAfterLoopCondition => {
                "Expected ';' after loop condition".to_string()
            }
            ParseError::ExpectedRightParenAfterForLoop => "Expected ')' after for loop".to_string(),
            ParseError::TooManyArguments => "Cannot have more than 255 arguments".to_string(),
            ParseError::ExpectedRightParenAfterArguments => {
                "Expected ')' after arguments".to_string()
            }
            ParseError::ExpectedFunctionName => "Expected function name".to_string(),
            ParseError::ExpectedMethodName => "Expected method name".to_string(),
            ParseError::ExpectedLeftParenAfterFunctionName => {
                "Expected '(' after function name".to_string()
            }
            ParseError::ExpectedLeftParenAfterMethodName => {
                "Expected '(' after method name".to_string()
            }
            ParseError::TooManyParameters => "Cannot have more than 255 parameters".to_string(),
            ParseError::ExpectedParameterName => "Expected parameter name".to_string(),
            ParseError::ExpectedRightParenAfterParameters => {
                "Expected ')' after parameters".to_string()
            }
            ParseError::ExpectedLeftBraceAfterFunction => "Expected '{' after function".to_string(),
            ParseError::ExpectedLeftBraceAfterMethod => "Expected '{' after method".to_string(),
            ParseError::ExpectedSemicolonAfterReturn => "Expected ';' after return".to_string(),
        };
        write!(f, "{}", s)
    }
}

pub fn report_parse_errors(errors: Vec<(ParseError, Token)>) {
    for (error, token) in errors {
        match token.ty {
            TokenType::EOF => {
                println!("[line {}] Error at end of file: {}", token.line, error);
            }
            _ => {
                println!(
                    "[line {}] Error at '{}': {}",
                    token.line, token.lexeme, error
                );
            }
        }
    }
}
