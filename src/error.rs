// TODO: separate lex errors, parse errors, etc.

use crate::token::Token;

#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter { line: usize },
    UnterminatedString { line: usize },
    ExpectRightParenAfterExpression { token: Token },
}
