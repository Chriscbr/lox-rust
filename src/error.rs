#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter { line: usize },
    UnterminatedString { line: usize },
}
