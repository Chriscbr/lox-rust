mod env;
mod value;

use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use crate::ast::{
    Assign, Binary, BinaryOp, Call, Class, Expr, Function, Get, Grouping, If, Literal, Logical,
    LogicalOp, Print, Return, Set, Stmt, Super, This, Unary, UnaryOp, VarDecl, Variable, While,
};

use self::{env::Environment, value::RuntimeValue};

pub struct Interpreter<'a> {
    stdout: Box<dyn Write + 'a>,
    env: Rc<RefCell<Environment>>,
    curr_func: Option<value::Function>,
}

impl<'a> Interpreter<'a> {
    pub fn new(stdout: Box<dyn Write + 'a>) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        globals.borrow_mut().set_global(
            "clock",
            RuntimeValue::NativeFunction(value::NativeFunction::new(0, |_, _| {
                Ok(RuntimeValue::Number(
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_millis() as f64,
                ))
            })),
        );

        Self {
            stdout,
            env: globals.clone(),
            curr_func: None,
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
            Stmt::Block(stmts) => {
                let env = Rc::new(RefCell::new(Environment::new(Some(self.env.clone()))));
                self.execute_block(stmts, env)
            }
            Stmt::Class(class) => self.execute_class(class),
            Stmt::Expr(expr) => self.execute_expr(expr),
            Stmt::Function(func) => self.execute_function_decl(func),
            Stmt::If(i) => self.execute_if(i),
            Stmt::Print(p) => self.execute_print(p),
            Stmt::Return(r) => self.execute_return(r),
            Stmt::VarDecl(var_decl) => self.execute_var_decl(var_decl),
            Stmt::While(w) => self.execute_while(w),
        }?;
        Ok(())
    }

    fn execute_block(
        &mut self,
        stmts: &[Stmt],
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let previous = std::mem::replace(&mut self.env, env);
        for stmt in stmts {
            match self.execute(stmt) {
                Ok(_) => {}
                Err(RuntimeError::ReturnValue(v)) => {
                    self.env = previous;
                    return Err(RuntimeError::ReturnValue(v));
                }
                Err(e) => {
                    self.env = previous;
                    return Err(e);
                }
            }
        }
        self.env = previous;
        Ok(())
    }

    fn execute_class(&mut self, class: &Class) -> Result<(), RuntimeError> {
        let superclass = if let Some(superclass) = &class.superclass {
            let superclass = self.expr(superclass)?;
            match superclass {
                RuntimeValue::Class(c) => Some(Rc::new(c)),
                _ => return Err(RuntimeError::SuperclassMustBeClass),
            }
        } else {
            None
        };

        let mut methods = HashMap::new();
        for method in &class.methods {
            match method {
                Stmt::Function(f) => {
                    let func = RuntimeValue::Function(value::Function::new(
                        f.params.len(),
                        f.clone(),
                        self.env.clone(),
                        None, // Source class will be replaced later
                        f.name == "init",
                    ));
                    methods.insert(f.name.clone(), func);
                }
                _ => unreachable!(),
            }
        }

        let class_value = RuntimeValue::Class(value::Class::new(
            class.name.clone(),
            superclass,
            Rc::new(RefCell::new(methods)),
        ));
        let new_env = self.env.borrow().extend(&class.name, class_value)?;
        self.env = new_env;

        // Replace source_class in methods
        let class_value = self.env.borrow().get(&class.name)?;
        let class = match class_value.clone() {
            RuntimeValue::Class(c) => c,
            _ => unreachable!(),
        };
        for value in class.methods.borrow_mut().values_mut() {
            let func = match value {
                RuntimeValue::Function(f) => f,
                _ => unreachable!(),
            };
            func.source_class.replace(class.clone());
        }

        Ok(())
    }

    fn execute_expr(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        self.expr(expr)?;
        Ok(())
    }

    fn execute_function_decl(&mut self, fun: &Rc<Function>) -> Result<(), RuntimeError> {
        let func = RuntimeValue::Function(value::Function::new(
            fun.params.len(),
            fun.clone(),
            self.env.clone(), // Closure will be replaced later
            None,
            false,
        ));
        if self.env.borrow().is_global() {
            self.env.borrow_mut().set_global(&fun.name, func);
            return Ok(());
        }

        let new_env = self.env.borrow().extend(&fun.name, func)?;
        self.env = new_env;
        let func = self.env.borrow().get(&fun.name)?;
        match func {
            RuntimeValue::Function(mut f) => {
                f.closure = self.env.clone();
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn execute_if(&mut self, i: &If) -> Result<(), RuntimeError> {
        let cond = self.expr(&i.condition)?;
        if cond.is_truthy() {
            self.execute(&i.then_branch)?;
        } else if let Some(else_branch) = &i.else_branch {
            self.execute(else_branch)?;
        }
        Ok(())
    }

    fn execute_print(&mut self, p: &Print) -> Result<(), RuntimeError> {
        let value = self.expr(&p.expr)?;
        writeln!(self.stdout, "{}", value).unwrap();
        Ok(())
    }

    fn execute_return(&mut self, r: &Return) -> Result<(), RuntimeError> {
        if self.curr_func.is_none() {
            return Err(RuntimeError::CannotReturnFromTopLevel);
        }
        let value = match &r.value {
            Some(expr) => self.expr(expr)?,
            None => RuntimeValue::Nil,
        };
        Err(RuntimeError::ReturnValue(value))
    }

    fn execute_var_decl(&mut self, var_decl: &VarDecl) -> Result<(), RuntimeError> {
        let value = match &var_decl.initializer {
            Some(expr) => self.expr(expr)?,
            None => RuntimeValue::Nil,
        };
        if self.env.borrow().is_global() {
            self.env.borrow_mut().set_global(&var_decl.name, value);
            return Ok(());
        }
        let new_env = self.env.borrow().extend(&var_decl.name, value)?;
        self.env = new_env;
        Ok(())
    }

    fn execute_while(&mut self, w: &While) -> Result<(), RuntimeError> {
        let mut cond = self.expr(&w.condition)?;
        while cond.is_truthy() {
            self.execute(&w.body)?;
            cond = self.expr(&w.condition)?;
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Assign(a) => self.assign(a),
            Expr::Binary(b) => self.binary(b),
            Expr::Call(c) => self.call(c),
            Expr::Get(g) => self.get(g),
            Expr::Grouping(g) => self.grouping(g),
            Expr::Literal(l) => self.literal(l),
            Expr::Logical(l) => self.logical(l),
            Expr::Set(s) => self.set(s),
            Expr::Super(s) => self.super_(s),
            Expr::This(t) => self.this(t),
            Expr::Unary(u) => self.unary(u),
            Expr::Variable(v) => self.variable(v),
        }
    }

    fn assign(&mut self, assign: &Assign) -> Result<RuntimeValue, RuntimeError> {
        let value = self.expr(&assign.value)?;
        self.assign_variable(&assign.name, value.clone())?;
        Ok(value)
    }

    fn assign_variable(&mut self, name: &str, value: RuntimeValue) -> Result<(), RuntimeError> {
        if self.env.borrow().is_global() {
            self.env.borrow_mut().set_global(name, value);
            return Ok(());
        }
        self.env.borrow().assign(name, value)
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
            BinaryOp::Eq => Ok(RuntimeValue::Bool(left.eq(&right))),
            BinaryOp::NotEq => Ok(RuntimeValue::Bool(!left.eq(&right))),
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

    fn call(&mut self, call: &Call) -> Result<RuntimeValue, RuntimeError> {
        let callee = self.expr(&call.callee)?;

        let mut args = Vec::new();
        for arg in &call.args {
            args.push(self.expr(arg)?);
        }

        match callee {
            RuntimeValue::Function(f) => f.call(self, args),
            RuntimeValue::NativeFunction(f) => f.call(self, args),
            RuntimeValue::Class(c) => c.call(self, args),
            _ => Err(RuntimeError::NotCallable),
        }
    }

    fn get(&mut self, get: &Get) -> Result<RuntimeValue, RuntimeError> {
        let object = self.expr(&get.object)?;
        match object {
            RuntimeValue::Instance(i) => i.get(&get.name),
            _ => Err(RuntimeError::NotCallable),
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

    fn logical(&mut self, logical: &Logical) -> Result<RuntimeValue, RuntimeError> {
        let left = self.expr(&logical.left)?;

        match logical.op {
            LogicalOp::And => {
                if !left.is_truthy() {
                    return Ok(left);
                }
            }
            LogicalOp::Or => {
                if left.is_truthy() {
                    return Ok(left);
                }
            }
        }

        self.expr(&logical.right)
    }

    fn set(&mut self, set: &Set) -> Result<RuntimeValue, RuntimeError> {
        let object = self.expr(&set.object)?;
        let value = self.expr(&set.value)?;
        match object {
            RuntimeValue::Instance(mut i) => {
                i.set(&set.name, value.clone());
                Ok(value)
            }
            _ => Err(RuntimeError::OnlyInstancesHaveFields),
        }
    }

    fn super_(&mut self, super_: &Super) -> Result<RuntimeValue, RuntimeError> {
        let curr_function = match &self.curr_func {
            Some(f) => f,
            None => return Err(RuntimeError::CantUseSuperOutsideClass),
        };
        let func_class = match &curr_function.source_class {
            Some(c) => c,
            None => return Err(RuntimeError::CantUseSuperOutsideClass),
        };
        let superclass = match &func_class.superclass {
            Some(c) => &*c,
            None => return Err(RuntimeError::CantUseSuperInsideClassWithoutSuperclass),
        };
        let method = match superclass.find_method(&super_.method) {
            Some(RuntimeValue::Function(f)) => f,
            _ => return Err(RuntimeError::UndefinedProperty(super_.method.clone())),
        };
        let this = match self.this(&This) {
            Ok(v) => match v {
                RuntimeValue::Instance(i) => i,
                _ => panic!("this should be an instance"),
            },
            Err(e) => return Err(e),
        };
        Ok(RuntimeValue::Function(method.bind(this)?))
    }

    fn this(&self, _this: &This) -> Result<RuntimeValue, RuntimeError> {
        match self.env.borrow().get("this") {
            Ok(v) => Ok(v),
            Err(RuntimeError::UndefinedVariable(_)) => Err(RuntimeError::CantUseThisOutsideClass),
            Err(e) => Err(e),
        }
    }

    fn unary(&mut self, unary: &Unary) -> Result<RuntimeValue, RuntimeError> {
        let right = self.expr(&unary.right)?;

        match unary.op {
            UnaryOp::Negate => match right {
                RuntimeValue::Number(n) => Ok(RuntimeValue::Number(-n)),
                _ => Err(RuntimeError::OperandMustBeNumber),
            },
            UnaryOp::Not => Ok(RuntimeValue::Bool(!right.is_truthy())),
        }
    }

    fn variable(&self, variable: &Variable) -> Result<RuntimeValue, RuntimeError> {
        self.env.borrow().get(&variable.name)
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    OperandsMustBeNumbers,
    OperandsMustBeTwoNumbersOrTwoStrings,
    OperandMustBeNumber,
    UndefinedVariable(String),
    InvalidArgumentCount,
    NotCallable,
    AlreadyDefined(String),
    CannotReturnFromTopLevel,
    UndefinedProperty(String),
    OnlyInstancesHaveFields,
    CantUseThisOutsideClass,
    CannotReturnInsideInit,
    SuperclassMustBeClass,
    CantUseSuperOutsideClass,
    CantUseSuperInsideClassWithoutSuperclass,
    // Special runtime error for returning values from a function
    ReturnValue(RuntimeValue),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::OperandsMustBeNumbers => write!(f, "Operands must be numbers."),
            RuntimeError::OperandsMustBeTwoNumbersOrTwoStrings => {
                write!(f, "Operands must be two numbers or two strings.")
            }
            RuntimeError::OperandMustBeNumber => write!(f, "Operand must be a number."),
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable '{}'.", name),
            RuntimeError::InvalidArgumentCount => write!(f, "Invalid argument count."),
            RuntimeError::NotCallable => write!(f, "Can only call functions and classes."),
            RuntimeError::AlreadyDefined(name) => {
                write!(
                    f,
                    "Already a variable with the name '{}' in this scope.",
                    name
                )
            }
            RuntimeError::CannotReturnFromTopLevel => {
                write!(f, "Cannot return from top-level code.")
            }
            RuntimeError::UndefinedProperty(name) => {
                write!(f, "Undefined property '{}'.", name)
            }
            RuntimeError::OnlyInstancesHaveFields => {
                write!(f, "Only instances have fields.")
            }
            RuntimeError::CantUseThisOutsideClass => {
                write!(f, "Can't use 'this' outside of a class.")
            }
            RuntimeError::CannotReturnInsideInit => {
                write!(f, "Cannot return a value from an initializer.")
            }
            RuntimeError::SuperclassMustBeClass => {
                write!(f, "Superclass must be a class.")
            }
            RuntimeError::CantUseSuperOutsideClass => {
                write!(f, "Can't use 'super' outside of a class.")
            }
            RuntimeError::CantUseSuperInsideClassWithoutSuperclass => {
                write!(f, "Can't use 'super' inside a class without a superclass.")
            }
            RuntimeError::ReturnValue(_) => panic!("Internal error: unhandled return value"),
        }
    }
}
