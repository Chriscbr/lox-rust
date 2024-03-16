use std::fmt::Display;

use crate::token::TokenType;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Function(Function),
    Print(Expr),
    VarDecl(VarDecl),
    Block(Vec<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Stmt::Expr(e) => format!("{};", e),
            Stmt::Function(f) => format!("{}", f),
            Stmt::Print(e) => format!("print {};", e),
            Stmt::VarDecl(v) => format!("{}", v),
            Stmt::Block(stmts) => {
                let mut s = String::new();
                s.push_str("{\n");
                for stmt in stmts {
                    s.push_str(&format!("{}\n", stmt));
                }
                s.push_str("}");
                s
            }
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("fun {}(", self.name));
        for (i, param) in self.params.iter().enumerate() {
            s.push_str(&format!("{}", param));
            if i < self.params.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str(") {\n");
        for stmt in &self.body {
            s.push_str(&format!("{}\n", stmt));
        }
        s.push_str("}");
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub initializer: Option<Expr>,
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self.initializer {
            Some(e) => format!("var {} = {};", self.name, e),
            None => format!("var {};", self.name),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Binary),
    Call(Call),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Variable(Variable),
    Assign(Assign),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expr::Binary(b) => format!("{}", b),
            Expr::Call(c) => format!("{}", c),
            Expr::Grouping(g) => format!("{}", g),
            Expr::Literal(l) => format!("{}", l),
            Expr::Unary(u) => format!("{}", u),
            Expr::Variable(v) => format!("{}", v),
            Expr::Assign(a) => format!("{}", a),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("{}(", self.callee));
        for (i, arg) in self.args.iter().enumerate() {
            s.push_str(&format!("{}", arg));
            if i < self.args.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str(")");
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

impl Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expr)
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub right: Box<Expr>,
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Negate,
}

impl From<TokenType> for UnaryOp {
    fn from(ty: TokenType) -> Self {
        match ty {
            TokenType::Minus => UnaryOp::Negate,
            TokenType::Bang => UnaryOp::Not,
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
