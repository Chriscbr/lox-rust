use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use super::{value::RuntimeValue, RuntimeError};

#[derive(Debug, Clone)]
pub struct Environment {
    // values are wrapped in Rc/RefCell so that cloning the environment results in a shallow copy
    // where updating a value in one of the hashmaps also updates it in the other
    values: HashMap<String, Rc<RefCell<RuntimeValue>>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn extend(&self, name: &str, value: RuntimeValue) -> Result<Environment, RuntimeError> {
        if self.values.contains_key(name) {
            return Err(RuntimeError::AlreadyDefined(name.to_string()));
        }
        let mut new_env = self.clone();
        new_env
            .values
            .insert(name.to_string(), Rc::new(RefCell::new(value)));
        Ok(new_env)
    }

    pub fn get(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.borrow().clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(RuntimeError::UndefinedVariable(name.to_string()))
    }

    pub fn assign(&self, name: &str, value: RuntimeValue) -> Result<(), RuntimeError> {
        match self.values.get(name) {
            Some(v) => {
                v.replace(value);
                return Ok(());
            }
            None => {
                if let Some(enclosing) = &self.enclosing {
                    return enclosing.borrow_mut().assign(name, value);
                } else {
                    return Err(RuntimeError::UndefinedVariable(name.to_string()));
                }
            }
        }
    }

    pub fn set_global(&mut self, name: &str, value: RuntimeValue) {
        match self.values.get(name) {
            Some(v) => {
                v.replace(value);
            }
            None => {
                self.values
                    .insert(name.to_string(), Rc::new(RefCell::new(value)));
            }
        }
    }

    pub fn is_global(&self) -> bool {
        self.enclosing.is_none()
    }
}
