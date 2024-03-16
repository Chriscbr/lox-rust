mod env;
mod value;

use crate::ast::{
    Assign, Binary, BinaryOp, Expr, Grouping, Literal, Stmt, Unary, UnaryOp, Variable,
};

use self::{env::Environment, value::RuntimeValue};

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Print(expr) => {
                let value = self.expr(expr)?;
                println!("{}", value);
            }
            Stmt::Expr(expr) => {
                self.expr(expr)?;
            }
            Stmt::VarDecl(var_decl) => {
                let value = match &var_decl.initializer {
                    Some(expr) => self.expr(expr)?,
                    None => RuntimeValue::Nil,
                };
                self.env.define(&var_decl.name, value);
            }
        }
        Ok(())
    }

    pub fn expr(&mut self, expr: &Expr) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Binary(b) => self.binary(b),
            Expr::Grouping(g) => self.grouping(g),
            Expr::Literal(l) => self.literal(l),
            Expr::Unary(u) => self.unary(u),
            Expr::Variable(v) => self.variable(v),
            Expr::Assign(a) => self.assign(a),
        }
    }

    fn binary(&mut self, binary: &Binary) -> Result<RuntimeValue, RuntimeError> {
        let left = self.expr(&binary.left)?;
        let right = self.expr(&binary.right)?;

        match binary.op {
            BinaryOp::Add => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => {
                    Ok(RuntimeValue::Number(l + r))
                }
                (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                    Ok(RuntimeValue::String(format!("{}{}", l, r)))
                }
                _ => Err(RuntimeError::OperandsMustBeTwoNumbersOrTwoStrings),
            },
            BinaryOp::Sub => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => {
                    Ok(RuntimeValue::Number(l - r))
                }
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
            BinaryOp::Mul => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => {
                    Ok(RuntimeValue::Number(l * r))
                }
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
            BinaryOp::Div => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => {
                    Ok(RuntimeValue::Number(l / r))
                }
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
            BinaryOp::Eq => Ok(RuntimeValue::Bool(self.is_equal(&left, &right))),
            BinaryOp::NotEq => Ok(RuntimeValue::Bool(!self.is_equal(&left, &right))),
            BinaryOp::Lt => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => Ok(RuntimeValue::Bool(l < r)),
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
            BinaryOp::LtEq => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => {
                    Ok(RuntimeValue::Bool(l <= r))
                }
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
            BinaryOp::Gt => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => Ok(RuntimeValue::Bool(l > r)),
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
            BinaryOp::GtEq => match (left, right) {
                (RuntimeValue::Number(l), RuntimeValue::Number(r)) => {
                    Ok(RuntimeValue::Bool(l >= r))
                }
                _ => Err(RuntimeError::OperandsMustBeNumbers),
            },
        }
    }

    fn grouping(&mut self, grouping: &Grouping) -> Result<RuntimeValue, RuntimeError> {
        self.expr(&grouping.expr)
    }

    fn literal(&self, literal: &Literal) -> Result<RuntimeValue, RuntimeError> {
        match literal {
            Literal::Number(n) => Ok(RuntimeValue::Number(*n)),
            Literal::String(s) => Ok(RuntimeValue::String(s.clone())),
            Literal::Bool(b) => Ok(RuntimeValue::Bool(*b)),
            Literal::Nil => Ok(RuntimeValue::Nil),
        }
    }

    fn unary(&mut self, unary: &Unary) -> Result<RuntimeValue, RuntimeError> {
        let right = self.expr(&unary.right)?;

        match unary.op {
            UnaryOp::Negate => match right {
                RuntimeValue::Number(n) => Ok(RuntimeValue::Number(-n)),
                _ => Err(RuntimeError::OperandMustBeNumber),
            },
            UnaryOp::Not => Ok(RuntimeValue::Bool(!self.is_truthy(&right))),
        }
    }

    fn variable(&self, variable: &Variable) -> Result<RuntimeValue, RuntimeError> {
        self.env.get(&variable.name)
    }

    fn assign(&mut self, assign: &Assign) -> Result<RuntimeValue, RuntimeError> {
        let value = self.expr(&assign.value)?;
        self.env.assign(&assign.name, value.clone())?;
        Ok(value)
    }

    fn is_truthy(&self, value: &RuntimeValue) -> bool {
        match value {
            RuntimeValue::Nil => false,
            RuntimeValue::Bool(b) => *b,
            _ => true,
        }
    }

    fn is_equal(&self, a: &RuntimeValue, b: &RuntimeValue) -> bool {
        match (a, b) {
            (RuntimeValue::Nil, RuntimeValue::Nil) => true,
            (RuntimeValue::Number(l), RuntimeValue::Number(r)) => l == r,
            (RuntimeValue::String(l), RuntimeValue::String(r)) => l == r,
            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => l == r,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    OperandsMustBeNumbers,
    OperandsMustBeTwoNumbersOrTwoStrings,
    OperandMustBeNumber,
    UndefinedVariable(String),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::OperandsMustBeNumbers => write!(f, "Operands must be numbers"),
            RuntimeError::OperandsMustBeTwoNumbersOrTwoStrings => {
                write!(f, "Operands must be two numbers or two strings")
            }
            RuntimeError::OperandMustBeNumber => write!(f, "Operand must be a number"),
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable '{}'", name),
        }
    }
}
