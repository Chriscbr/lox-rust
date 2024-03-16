use std::{fmt::Display, sync::Arc};

use crate::ast::Function;

use super::RuntimeError;

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Callable(Callable),
}

#[derive(Debug, Clone)]
pub struct Callable {
    pub arity: usize,
    pub fun: Arc<Function>,
}

impl Callable {
    pub fn new(arity: usize, fun: Arc<Function>) -> Self {
        Self { arity, fun }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            RuntimeValue::Nil => format!("nil"),
            RuntimeValue::Bool(b) => format!("{}", b),
            RuntimeValue::Number(n) => {
                let text = format!("{}", n);
                if text.ends_with(".0") {
                    text[..text.len() - 2].to_string()
                } else {
                    text
                }
            }
            RuntimeValue::String(s) => format!("{}", s),
            RuntimeValue::Callable(c) => format!("<fn {}>", c.fun.name),
        };
        write!(f, "{}", s)
    }
}

pub fn clock(_: &[RuntimeValue]) -> Result<RuntimeValue, RuntimeError> {
    Ok(RuntimeValue::Number(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    ))
}
