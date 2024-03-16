use crate::{
    error::Error,
    token::{Token, TokenType, KEYWORDS},
};

#[derive(Debug)]
pub struct Scanner {
    source: String, // chars: Vec<char> ?
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    errors: Vec<Error>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            errors: vec![],
        }
    }

    pub fn scan_tokens(mut self) -> (Vec<Token>, Vec<Error>) {
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.current;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenType::EOF, "".to_string(), self.line));
        (self.tokens, self.errors)
    }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let ty = if self.matches('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(ty);
            }
            '=' => {
                let ty = if self.matches('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(ty);
            }
            '<' => {
                let ty = if self.matches('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(ty);
            }
            '>' => {
                let ty = if self.matches('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(ty);
            }
            '/' => {
                if self.matches('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {} // Ignore whitespace.
            '\n' => {
                self.line += 1;
            }
            '"' => {
                self.string();
            }
            _ => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    self.errors
                        .push(Error::UnexpectedCharacter { line: self.line });
                }
            }
        };
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let ty = KEYWORDS.get(text);
        match ty {
            Some(ty) => self.add_token(*ty),
            None => self.add_token(TokenType::Identifier),
        }

        self.add_token(TokenType::Identifier)
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            // Consume the "."
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        self.add_token(TokenType::Number);
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.errors
                .push(Error::UnterminatedString { line: self.line });
        }

        self.advance(); // The closing ".

        self.add_token(TokenType::String);
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.char_at(self.current) != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        return self.char_at(self.current);
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        return self.char_at(self.current + 1);
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn advance(&mut self) -> char {
        let c = self.char_at(self.current);
        self.current += 1;
        c
    }

    fn add_token(&mut self, ty: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(ty, text.to_string(), self.line)); // str -> String
    }

    fn char_at(&self, idx: usize) -> char {
        return char::from(self.source.as_bytes()[idx]); // unsafe conversion
    }
}
