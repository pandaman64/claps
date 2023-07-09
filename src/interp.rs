use std::{collections::HashMap, rc::Rc};

use crate::{grammar::*, BinOp};

#[derive(Debug)]
pub struct Env<'p> {
    pub parent: Option<&'p Env<'p>>,
    pub vars: HashMap<String, Value>,
}

impl Env<'_> {
    fn lookup(&self, var: &str) -> Option<&Value> {
        self.vars.get(var).or_else(|| self.parent?.lookup(var))
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Number(i64),
    Fun { params: Vec<String>, body: Rc<Expr> },
}

pub fn eval(env: &Env, expr: &Expr) -> Value {
    match expr {
        Expr::Var(var) => env
            .lookup(&var.ident)
            .unwrap_or_else(|| panic!("variable {} not found", var.ident))
            .clone(),
        Expr::Number { value } => Value::Number(*value),
        Expr::Greater {
            left,
            op: _op,
            right,
        } => {
            let left = eval(env, left);
            let right = eval(env, right);
            match (left, right) {
                (Value::Number(left), Value::Number(right)) => Value::Boolean(left > right),
                _ => panic!("type error"),
            }
        }
        Expr::Add { left, op, right } => {
            let left = eval(env, left);
            let right = eval(env, right);
            match (left, right) {
                (Value::Number(left), Value::Number(right)) => match op {
                    BinOp::Add => Value::Number(left + right),
                    BinOp::Minus => Value::Number(left - right),
                    _ => unreachable!("invalid operator {:?}", op),
                },
                _ => panic!("type error"),
            }
        }
        Expr::Mul {
            left,
            op: _op,
            right,
        } => {
            let left = eval(env, left);
            let right = eval(env, right);
            match (left, right) {
                (Value::Number(left), Value::Number(right)) => Value::Number(left * right),
                _ => panic!("type error"),
            }
        }
        Expr::Call { fun, args, .. } => {
            let fun = eval(env, fun);
            let args = args.iter().map(|arg| eval(env, arg)).collect::<Vec<_>>();
            match fun {
                Value::Fun { params, body } => {
                    assert!(params.len() == args.len());
                    let new_env = Env {
                        parent: Some(env),
                        vars: params.into_iter().zip(args).collect(),
                    };
                    eval(&new_env, &body)
                }
                _ => panic!("type error"),
            }
        }
        Expr::Let {
            name, value, body, ..
        } => {
            let value = eval(env, value);
            let new_env = Env {
                parent: Some(env),
                vars: HashMap::from([(name.ident.clone(), value)]),
            };
            eval(&new_env, body)
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            let cond = eval(env, cond);
            match cond {
                Value::Boolean(true) => eval(env, then_branch),
                Value::Boolean(false) => eval(env, else_branch),
                _ => panic!("type error"),
            }
        }
    }
}

pub fn run(program: &Program) -> Value {
    let env = Env {
        parent: None,
        // bind each top-level function to its function name
        vars: HashMap::from_iter(program.definitions.iter().map(|def| match def {
            Definition::Fun {
                name, args, body, ..
            } => (
                name.ident.clone(),
                Value::Fun {
                    params: args.iter().map(|arg| arg.ident.clone()).collect(),
                    body: Rc::new(body.clone()),
                },
            ),
        })),
    };
    eval(
        &env,
        &Expr::Call {
            fun: Box::new(Expr::Var(Ident {
                ident: "main".to_string(),
            })),
            args: vec![],
            _open_paren: (),
            _close_paren: (),
        },
    )
}
