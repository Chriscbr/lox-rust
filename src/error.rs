// TODO: separate lex errors, parse errors, etc.

use crate::token::{Token, TokenType};

#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter { line: usize },
    UnterminatedString { line: usize },
    ExpectedRightParenAfterExpr { token: Token },
    ExpectedSemicolonAfterExpression { token: Token },
    ExpectedVariableName { token: Token },
    InvalidAssignmentTarget { token: Token },
    ExpectedRightBraceAfterBlock { token: Token },
    ExpectedLeftParenAfterIf { token: Token },
    ExpectedRightParenAfterIfCondition { token: Token },
    ExpectedLeftParenAfterWhile { token: Token },
    ExpectedRightParenAfterWhileCondition { token: Token },
    ExpectedLeftParenAfterFor { token: Token },
    ExpectedSemicolonAfterLoopCondition { token: Token },
    ExpectedRightParenAfterForLoop { token: Token },
    TooManyArguments { token: Token },
    ExpectedRightParenAfterArguments { token: Token },
    ExpectedFunctionName { token: Token },
    ExpectedMethodName { token: Token },
    ExpectedLeftParenAfterFunctionName { token: Token },
    ExpectedLeftParenAfterMethodName { token: Token },
    TooManyParameters { token: Token },
    ExpectedParameterName { token: Token },
    ExpectedRightParenAfterParameters { token: Token },
    ExpectedLeftBraceAfterFunction { token: Token },
    ExpectedLeftBraceAfterMethod { token: Token },
    ExpectedSemicolonAfterReturn { token: Token },
}

pub fn report_errors(errors: &[Error]) {
    for error in errors {
        match error {
            Error::UnexpectedCharacter { line } => {
                println!("[line {}] Error: unexpected character.", line)
            }
            Error::UnterminatedString { line } => {
                println!("[line {}] Error: unterminated string.", line)
            }
            Error::ExpectedRightParenAfterExpr { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after expression.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after expression.",
                    token.line, token.lexeme,
                ),
            },
            Error::ExpectedSemicolonAfterExpression { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ';' after expression.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ';' after expression.",
                    token.line, token.lexeme,
                ),
            },
            Error::ExpectedVariableName { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected variable name.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected variable name.",
                    token.line, token.lexeme
                ),
            },
            Error::InvalidAssignmentTarget { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Invalid assignment target.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Invalid assignment target.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedRightBraceAfterBlock { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '}}' after block.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '}}' after block.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftParenAfterIf { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '(' after 'if'.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '(' after 'if'.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedRightParenAfterIfCondition { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after if condition.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after if condition.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftParenAfterWhile { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '(' after 'while'.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '(' after 'while'.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedRightParenAfterWhileCondition { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after while condition.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after while condition.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftParenAfterFor { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '(' after 'for'.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '(' after 'for'.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedSemicolonAfterLoopCondition { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ';' after loop condition.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ';' after loop condition.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedRightParenAfterForLoop { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after for loop.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after for loop.",
                    token.line, token.lexeme
                ),
            },
            Error::TooManyArguments { token } => match token.ty {
                TokenType::EOF => {
                    println!("[line {}] Error at end: Too many arguments.", token.line)
                }
                _ => println!(
                    "[line {}] Error at '{}': Too many arguments.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedRightParenAfterArguments { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after arguments.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after arguments.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedFunctionName { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected function name.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected function name.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedMethodName { token } => match token.ty {
                TokenType::EOF => {
                    println!("[line {}] Error at end: Expected method name.", token.line)
                }
                _ => println!(
                    "[line {}] Error at '{}': Expected method name.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftParenAfterFunctionName { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '(' after function name.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '(' after function name.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftParenAfterMethodName { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '(' after method name.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '(' after method name.",
                    token.line, token.lexeme
                ),
            },
            Error::TooManyParameters { token } => match token.ty {
                TokenType::EOF => {
                    println!("[line {}] Error at end: Too many parameters.", token.line)
                }
                _ => println!(
                    "[line {}] Error at '{}': Too many parameters.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedParameterName { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected parameter name.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected parameter name.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedRightParenAfterParameters { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after parameters.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after parameters.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftBraceAfterFunction { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '{{' before function body.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '{{' before function body.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedLeftBraceAfterMethod { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected '{{' before method body.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected '{{' before method body.",
                    token.line, token.lexeme
                ),
            },
            Error::ExpectedSemicolonAfterReturn { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ';' after return.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ';' after return.",
                    token.line, token.lexeme
                ),
            },
        }
    }
}
