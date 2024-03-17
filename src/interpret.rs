mod env;
mod value;

use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

use crate::ast::{
    Assign, Binary, BinaryOp, Call, Class, Expr, Function, Grouping, If, Literal, Logical,
    LogicalOp, Print, Return, Stmt, Unary, UnaryOp, VarDecl, Variable, While,
};

use self::{env::Environment, value::RuntimeValue};

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    inside_function: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        // globals.define(
        //     "clock",
        //     RuntimeValue::Callable(Callable {
        //         arity: 0,
        //         closure: value::clock,
        //     }),
        // );

        Self {
            env: globals.clone(),
            globals,
            inside_function: false,
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        // dbg!(&stmt);
        // dbg!(&self.env);
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
        let mut methods = HashMap::new();
        for method in &class.methods {
            let value = RuntimeValue::Function(value::Function::new(
                method.params.len(),
                method.clone(),
                self.env.clone(),
            ));
            methods.insert(method.name.clone(), value);
        }
        let class_value = RuntimeValue::Class(value::Class::new(class.name.clone(), methods));
        let new_env = self.env.borrow().extend(&class.name, class_value)?;
        self.env = Rc::new(RefCell::new(new_env));
        Ok(())
    }

    fn execute_expr(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        self.expr(expr)?;
        Ok(())
    }

    fn execute_function_decl(&mut self, fun: &Arc<Function>) -> Result<(), RuntimeError> {
        // let func = RuntimeValue::Function(value::Function::new(
        //     fun.params.len(),
        //     fun.clone(),
        //     self.env.clone(), // this will be replaced when the function is called
        // ));
        // let new_env = self.env.borrow().define(&fun.name, func)?;
        // self.env = Rc::new(RefCell::new(new_env));
        // self.env.borrow().assign(&fun.name, func)?;
        // let func = self.env.borrow().get(&fun.name)?;
        // match func {
        //     RuntimeValue::Function(mut f) => {
        //         f.closure = self.env.clone();
        //     }
        //     _ => unreachable!(),
        // }
        // Ok(())

        let func = RuntimeValue::Function(value::Function::new(
            fun.params.len(),
            fun.clone(),
            self.env.clone(),
        ));
        if self.env.borrow().is_global() {
            self.env.borrow_mut().set_global(&fun.name, func);
            return Ok(());
        }

        let new_env = self.env.borrow().extend(&fun.name, func)?;
        self.env = Rc::new(RefCell::new(new_env));
        let func = self.env.borrow().get(&fun.name)?;
        match func {
            RuntimeValue::Function(mut f) => {
                f.closure = self.env.clone();
            }
            _ => unreachable!(),
        }
        Ok(())

        // let temp = RuntimeValue::Nil;
        // let new_env = self.env.borrow().extend(&fun.name, temp)?;
        // self.env = Rc::new(RefCell::new(new_env));
        // let func = RuntimeValue::Function(value::Function::new(
        //     fun.params.len(),
        //     fun.clone(),
        //     self.env.clone(),
        // ));
        // self.env.borrow().assign(&fun.name, func)?;
        // let func = self.env.borrow().get(&fun.name)?;
        // match func {
        //     RuntimeValue::Function(mut f) => {
        //         f.closure = self.env.clone();
        //     }
        //     _ => unreachable!(),
        // }
        // Ok(())
    }

    fn execute_if(&mut self, i: &If) -> Result<(), RuntimeError> {
        let cond = self.expr(&i.condition)?;
        if self.is_truthy(&cond) {
            self.execute(&i.then_branch)?;
        } else if let Some(else_branch) = &i.else_branch {
            self.execute(else_branch)?;
        }
        Ok(())
    }

    fn execute_print(&mut self, p: &Print) -> Result<(), RuntimeError> {
        let value = self.expr(&p.expr)?;
        println!("{}", value);
        Ok(())
    }

    fn execute_return(&mut self, r: &Return) -> Result<(), RuntimeError> {
        if !self.inside_function {
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
        self.env = Rc::new(RefCell::new(new_env));
        Ok(())
    }

    fn execute_while(&mut self, w: &While) -> Result<(), RuntimeError> {
        let mut cond = self.expr(&w.condition)?;
        while self.is_truthy(&cond) {
            self.execute(&w.body)?;
            cond = self.expr(&w.condition)?;
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Binary(b) => self.binary(b),
            Expr::Call(c) => self.call(c),
            Expr::Grouping(g) => self.grouping(g),
            Expr::Literal(l) => self.literal(l),
            Expr::Logical(l) => self.logical(l),
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

    fn call(&mut self, call: &Call) -> Result<RuntimeValue, RuntimeError> {
        let callee = self.expr(&call.callee)?;

        let mut args = Vec::new();
        for arg in &call.args {
            args.push(self.expr(arg)?);
        }

        match callee {
            RuntimeValue::Function(f) => f.call(self, args),
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
                if !self.is_truthy(&left) {
                    return Ok(left);
                }
            }
            LogicalOp::Or => {
                if self.is_truthy(&left) {
                    return Ok(left);
                }
            }
        }

        self.expr(&logical.right)
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
        self.lookup_variable(&variable.name)
    }

    fn assign(&mut self, assign: &Assign) -> Result<RuntimeValue, RuntimeError> {
        let value = self.expr(&assign.value)?;
        self.assign_variable(&assign.name, value.clone())?;
        Ok(value)
    }

    fn lookup_variable(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        self.env.borrow().get(name)
        // if let Ok(value) = self.env.borrow().get(name) {
        //     Ok(value)
        // } else {
        //     self.globals.borrow().get(name)
        // }
    }

    fn assign_variable(&mut self, name: &str, value: RuntimeValue) -> Result<(), RuntimeError> {
        if self.env.borrow().is_global() {
            self.env.borrow_mut().set_global(name, value);
            return Ok(());
        }
        self.env.borrow().assign(name, value)
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
    InvalidArgumentCount,
    NotCallable,
    AlreadyDefined(String),
    CannotReturnFromTopLevel,
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
            RuntimeError::ReturnValue(_) => panic!("Internal error: unhandled return value"),
        }
    }
}
