use std::collections::HashMap;

use super::{value::RuntimeValue, RuntimeError};

pub struct Environment {
    values: HashMap<String, RuntimeValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: RuntimeValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        match self.values.get(name) {
            Some(value) => Ok(value.clone()),
            None => Err(RuntimeError::UndefinedVariable(name.to_string())),
        }
    }
}
