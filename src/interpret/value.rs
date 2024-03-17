use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc, sync::Arc};

use super::{env::Environment, Interpreter, RuntimeError};

use crate::ast;

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
    Class(Class),
    Instance(Instance),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: usize,
    pub fun: Arc<ast::Function>,
    pub closure: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(arity: usize, fun: Arc<ast::Function>, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            arity,
            fun,
            closure,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        if args.len() != self.arity {
            return Err(RuntimeError::InvalidArgumentCount);
        }

        let env = Rc::new(RefCell::new(Environment::new(Some(self.closure.clone()))));
        for (i, param) in self.fun.params.iter().enumerate() {
            env.borrow_mut().define(param, args[i].clone())?;
        }

        let prev_inside_function = interpreter.inside_function;
        interpreter.inside_function = true;
        let result = interpreter.execute_block(&self.fun.body, env);
        interpreter.inside_function = prev_inside_function;

        match result {
            Ok(_) => Ok(RuntimeValue::Nil),
            Err(RuntimeError::ReturnValue(value)) => Ok(value),
            Err(e) => Err(e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, RuntimeValue>,
}

impl Class {
    pub fn new(name: String, methods: HashMap<String, RuntimeValue>) -> Self {
        Self { name, methods }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Arc<Class>,
    pub fields: HashMap<String, RuntimeValue>,
}

impl Instance {
    pub fn new(class: Arc<Class>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
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
            RuntimeValue::Function(c) => format!("<fn {}>", c.fun.name),
            RuntimeValue::Class(c) => format!("{}", c.name),
            RuntimeValue::Instance(i) => format!("<instance of {}>", i.class.name),
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
