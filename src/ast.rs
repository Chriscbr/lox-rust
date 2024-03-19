use std::{fmt::Display, rc::Rc};

use crate::token::TokenType;

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Class(Class),
    Expr(Expr),
    Function(Rc<Function>),
    If(If),
    Print(Print),
    Return(Return),
    VarDecl(VarDecl),
    While(While),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Stmt::Block(stmts) => {
                let mut s = String::new();
                s.push_str("{\n");
                for stmt in stmts {
                    s.push_str(&format!("{}\n", stmt));
                }
                s.push_str("}");
                s
            }
            Stmt::Class(c) => format!("{}", c),
            Stmt::Expr(e) => format!("{};", e),
            Stmt::Function(f) => format!("{}", f),
            Stmt::If(i) => format!("{}", i),
            Stmt::Print(p) => format!("{}", p),
            Stmt::Return(r) => format!("{}", r),
            Stmt::VarDecl(v) => format!("{}", v),
            Stmt::While(w) => format!("{}", w),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub methods: Vec<Rc<Function>>,
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        s.push_str(&format!("class {} {{\n", self.name));
        for method in &self.methods {
            s.push_str(&format!("{}\n", method));
        }
        s.push_str("}");
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
pub struct If {
    pub condition: Box<Expr>,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self.else_branch {
            Some(else_branch) => format!(
                "if ({}) {} else {}",
                self.condition, self.then_branch, else_branch
            ),
            None => format!("if ({}) {}", self.condition, self.then_branch),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Print {
    pub expr: Box<Expr>,
}

impl Display for Print {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "print {};", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Expr>,
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match &self.value {
            Some(e) => format!("return {};", e),
            None => format!("return;"),
        };
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
pub struct While {
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.condition, self.body)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Assign),
    Binary(Binary),
    Call(Call),
    Get(Get),
    Grouping(Grouping),
    Literal(Literal),
    Logical(Logical),
    Set(Set),
    Unary(Unary),
    Variable(Variable),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expr::Assign(a) => format!("{}", a),
            Expr::Binary(b) => format!("{}", b),
            Expr::Call(c) => format!("{}", c),
            Expr::Get(g) => format!("{}", g),
            Expr::Grouping(g) => format!("{}", g),
            Expr::Literal(l) => format!("{}", l),
            Expr::Logical(l) => format!("{}", l),
            Expr::Set(s) => format!("{}", s),
            Expr::Unary(u) => format!("{}", u),
            Expr::Variable(v) => format!("{}", v),
        };
        write!(f, "{}", s)
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
pub struct Get {
    pub object: Box<Expr>,
    pub name: String,
}

impl Display for Get {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.object, self.name)
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
pub struct Logical {
    pub left: Box<Expr>,
    pub op: LogicalOp,
    pub right: Box<Expr>,
}

impl Display for Logical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct Set {
    pub object: Box<Expr>,
    pub name: String,
    pub value: Box<Expr>,
}

impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{} = {}", self.object, self.name, self.value)
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
            TokenType::EqualEqual => BinaryOp::Eq,
            TokenType::BangEqual => BinaryOp::NotEq,
            TokenType::Less => BinaryOp::Lt,
            TokenType::LessEqual => BinaryOp::LtEq,
            TokenType::Greater => BinaryOp::Gt,
            TokenType::GreaterEqual => BinaryOp::GtEq,
            _ => panic!("Invalid token for binary op: {:?}", ty),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LogicalOp {
    And,
    Or,
}

impl From<TokenType> for LogicalOp {
    fn from(ty: TokenType) -> Self {
        match ty {
            TokenType::And => LogicalOp::And,
            TokenType::Or => LogicalOp::Or,
            _ => panic!("Invalid token for logical op: {:?}", ty),
        }
    }
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            LogicalOp::And => "and",
            LogicalOp::Or => "or",
        };
        write!(f, "{}", s)
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
            _ => panic!("Invalid token for unary op: {:?}", ty),
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
