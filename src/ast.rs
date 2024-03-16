use std::fmt::Display;

use crate::token::TokenType;

#[derive(Debug)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expr::Binary(b) => format!("{}", b),
            Expr::Grouping(g) => format!("{}", g),
            Expr::Literal(l) => format!("{}", l),
            Expr::Unary(u) => format!("{}", u),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub struct Binary {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

#[derive(Debug)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

impl Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expr)
    }
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Literal::Number(n) => format!("{}", n),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Bool(b) => format!("{}", b),
            Literal::Nil => format!("nil"),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub struct Unary {
    pub op: UnaryOp,
    pub right: Box<Expr>,
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.right)
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::LtEq => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::GtEq => ">=",
        };
        write!(f, "{}", s)
    }
}

impl From<TokenType> for BinaryOp {
    fn from(ty: TokenType) -> Self {
        match ty {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Star => BinaryOp::Mul,
            TokenType::Slash => BinaryOp::Div,
            TokenType::Equal => BinaryOp::Eq,
            TokenType::BangEqual => BinaryOp::NotEq,
            TokenType::Less => BinaryOp::Lt,
            TokenType::LessEqual => BinaryOp::LtEq,
            TokenType::Greater => BinaryOp::Gt,
            TokenType::GreaterEqual => BinaryOp::GtEq,
            _ => panic!("Invalid token for binary op"),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Negate,
}

impl From<TokenType> for UnaryOp {
    fn from(ty: TokenType) -> Self {
        match ty {
            TokenType::Minus => UnaryOp::Not,
            TokenType::Bang => UnaryOp::Negate,
            _ => panic!("Invalid token for unary op"),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOp::Not => "!",
            UnaryOp::Negate => "-",
        };
        write!(f, "{}", s)
    }
}
