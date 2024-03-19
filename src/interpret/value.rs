use std::{cell::RefCell, collections::HashMap, fmt::Debug, fmt::Display, rc::Rc, sync::Arc};

use super::{env::Environment, Interpreter, RuntimeError};

use crate::ast;

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
    NativeFunction(NativeFunction),
    Class(Class),
    Instance(Instance),
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub arity: usize,
    pub fun: Rc<fn(&mut Interpreter, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>>,
}

impl NativeFunction {
    pub fn new(
        arity: usize,
        fun: fn(&mut Interpreter, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>,
    ) -> Self {
        Self {
            arity,
            fun: Rc::new(fun),
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
        (self.fun)(interpreter, args)
    }
}

#[derive(Clone)]
pub struct Function {
    pub arity: usize,
    pub fun: Rc<ast::Function>,
    pub closure: Rc<RefCell<Environment>>,
    pub is_init: bool,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("arity", &self.arity)
            .field("fun", &self.fun)
            .finish()
    }
}

impl Function {
    pub fn new(
        arity: usize,
        fun: Rc<ast::Function>,
        closure: Rc<RefCell<Environment>>,
        is_init: bool,
    ) -> Self {
        Self {
            arity,
            fun,
            closure,
            is_init,
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

        let mut env = Rc::new(RefCell::new(Environment::new(Some(self.closure.clone()))));
        for (i, param) in self.fun.params.iter().enumerate() {
            let new_env = env.borrow().extend(param, args[i].clone())?;
            env = Rc::new(RefCell::new(new_env));
        }

        let prev_inside_function = interpreter.inside_function;
        interpreter.inside_function = true;

        let result = interpreter.execute_block(&self.fun.body, env);

        interpreter.inside_function = prev_inside_function;

        if self.is_init {
            match result {
                Ok(_) => Ok(self.closure.borrow().get("this")?),
                Err(RuntimeError::ReturnValue(_)) => Err(RuntimeError::CannotReturnInsideInit),
                Err(e) => Err(e),
            }
        } else {
            match result {
                Ok(_) => Ok(RuntimeValue::Nil),
                Err(RuntimeError::ReturnValue(value)) => Ok(value),
                Err(e) => Err(e),
            }
        }
    }

    pub fn bind(&self, instance: Instance) -> Result<Function, RuntimeError> {
        let env = Environment::new(Some(self.closure.clone()));
        let env = Rc::new(RefCell::new(
            env.extend("this", RuntimeValue::Instance(instance))?,
        ));
        Ok(Function {
            arity: self.arity,
            fun: self.fun.clone(),
            closure: env,
            is_init: self.is_init,
        })
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

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        if args.len() != self.arity() {
            return Err(RuntimeError::InvalidArgumentCount);
        }

        let instance = Instance::new(Arc::new(self.clone()));
        let init = self.methods.get("init");
        if let Some(init) = init {
            if let RuntimeValue::Function(init) = init {
                let binded_fn = init.bind(instance.clone())?;
                binded_fn.call(interpreter, args)?;
            }
        }
        Ok(RuntimeValue::Instance(instance))
    }

    pub fn find_method(&self, name: &str) -> Option<RuntimeValue> {
        self.methods.get(name).cloned()
    }

    pub fn arity(&self) -> usize {
        if let Some(RuntimeValue::Function(init)) = self.methods.get("init") {
            init.arity
        } else {
            0
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Arc<Class>,
    // hashmap is stored in a Rc/RefCell so that cloning an instance results in a shallow copy
    // if there's a use case for a deep copy, consider adding a separate method for that
    pub fields: Rc<RefCell<HashMap<String, Rc<RefCell<RuntimeValue>>>>>,
}

impl Instance {
    pub fn new(class: Arc<Class>) -> Self {
        Self {
            class,
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        if let Some(value) = self.fields.borrow().get(name) {
            return Ok(value.borrow().clone());
        }

        if let Some(method) = self.class.find_method(name) {
            match method {
                RuntimeValue::Function(method) => {
                    return Ok(RuntimeValue::Function(method.bind(self.clone())?));
                }
                _ => panic!("Expected method to be a function"),
            }
        }

        Err(RuntimeError::UndefinedProperty(name.to_string()))
    }

    pub fn set(&mut self, name: &str, value: RuntimeValue) {
        if let Some(field) = self.fields.borrow().get(name) {
            field.replace(value);
            return;
        }
        self.fields
            .borrow_mut()
            .insert(name.to_string(), Rc::new(RefCell::new(value)));
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
            RuntimeValue::NativeFunction(_) => format!("<native fn>"),
            RuntimeValue::Class(c) => format!("{}", c.name),
            RuntimeValue::Instance(i) => format!("{} instance", i.class.name),
        };
        write!(f, "{}", s)
    }
}
