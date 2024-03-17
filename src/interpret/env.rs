use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{value::RuntimeValue, RuntimeError};

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, RuntimeValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn define(&mut self, name: &str, value: RuntimeValue) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            return Err(RuntimeError::AlreadyDefined(name.to_string()));
        }
        self.values.insert(name.to_string(), value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(RuntimeError::UndefinedVariable(name.to_string()))
    }

    pub fn assign(&mut self, name: &str, value: RuntimeValue) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            // let container = self.values.get_mut(name).unwrap();
            // *container = Box::new(value);
            self.values.insert(name.to_string(), value);
            return Ok(());
        }

        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.borrow_mut().assign(name, value);
        }

        Err(RuntimeError::UndefinedVariable(name.to_string()))
    }
}
