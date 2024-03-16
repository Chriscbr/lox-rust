// TODO: separate lex errors, parse errors, etc.

use crate::token::{Token, TokenType};

#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter { line: usize },
    UnterminatedString { line: usize },
    ExpectRightParenAfterExpr { token: Token },
    ExpectSemicolonAfterExpression { token: Token },
    ExpectedVariableName { token: Token },
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
            Error::ExpectRightParenAfterExpr { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after expression.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after expression.",
                    token.line, token.lexeme,
                ),
            },
            Error::ExpectSemicolonAfterExpression { token } => match token.ty {
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
        }
    }
}
