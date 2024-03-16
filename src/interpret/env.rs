use std::collections::HashMap;

use super::{value::RuntimeValue, RuntimeError};

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, RuntimeValue>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_enclosing(enclosing: &Environment) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing.clone())),
        }
    }

    pub fn define(&mut self, name: &str, value: RuntimeValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get(name);
        }

        Err(RuntimeError::UndefinedVariable(name.to_string()))
    }

    pub fn assign(&mut self, name: &str, value: RuntimeValue) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            return Ok(());
        }

        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.assign(name, value);
        }

        Err(RuntimeError::UndefinedVariable(name.to_string()))
    }
}
