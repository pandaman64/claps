#[rust_sitter::grammar("claps")]
mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub struct Language {
        pub definitions: Vec<Definition>,
    }

    #[derive(Debug)]
    pub enum Definition {
        Fun {
            #[rust_sitter::leaf(text = "fun")]
            _fun: (),
            name: Ident,
            #[rust_sitter::leaf(text = "(")]
            _open_paren: (),
            #[rust_sitter::delimited(#[rust_sitter::leaf(text = ",")] ())]
            args: Vec<Ident>,
            #[rust_sitter::leaf(text = ")")]
            _close_paren: (),
            #[rust_sitter::leaf(text = "{")]
            _open_body: (),
            body: Expr,
            #[rust_sitter::leaf(text = "}")]
            _close_body: (),
        },
    }

    #[derive(Debug, Clone)]
    pub enum Expr {
        Var(Ident),
        Number {
            #[rust_sitter::leaf(pattern = r"\d+", transform = |s| s.parse().unwrap())]
            value: i64,
        },
        #[rust_sitter::prec_left(1)]
        Add {
            left: Box<Expr>,
            #[rust_sitter::leaf(text = "+")]
            _op: (),
            right: Box<Expr>,
        },
        #[rust_sitter::prec_left(2)]
        Mul {
            left: Box<Expr>,
            #[rust_sitter::leaf(text = "*")]
            _op: (),
            right: Box<Expr>,
        },
        #[rust_sitter::prec(3)]
        Call {
            fun: Box<Expr>,
            #[rust_sitter::leaf(text = "(")]
            _open_paren: (),
            #[rust_sitter::delimited(#[rust_sitter::leaf(text = ",")] ())]
            args: Vec<Expr>,
            #[rust_sitter::leaf(text = ")")]
            _close_paren: (),
        },
        Let {
            #[rust_sitter::leaf(text = "let")]
            _let: (),
            name: Ident,
            #[rust_sitter::leaf(text = "=")]
            _eq: (),
            value: Box<Expr>,
            #[rust_sitter::leaf(text = ";")]
            _semicolon: (),
            body: Box<Expr>,
        },
    }

    #[derive(Debug, Clone)]
    pub struct Ident {
        #[rust_sitter::leaf(pattern = "[a-zA-Z_][a-zA-Z0-9_]*", transform = str::to_string)]
        pub ident: String,
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

mod interp {
    use std::{collections::HashMap, rc::Rc};

    use crate::grammar::*;

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
        Number(i64),
        Fun { params: Vec<String>, body: Rc<Expr> },
    }

    pub fn run(env: &Env, expr: &Expr) -> Value {
        match expr {
            Expr::Var(var) => env
                .lookup(&var.ident)
                .unwrap_or_else(|| panic!("variable {} not found", var.ident))
                .clone(),
            Expr::Number { value } => Value::Number(*value),
            Expr::Add { left, _op, right } => {
                let left = run(env, left);
                let right = run(env, right);
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Value::Number(left + right),
                    _ => panic!("type error"),
                }
            }
            Expr::Mul { left, _op, right } => {
                let left = run(env, left);
                let right = run(env, right);
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Value::Number(left * right),
                    _ => panic!("type error"),
                }
            }
            Expr::Call {
                fun,
                args,
                ..
            } => {
                let fun = run(env, fun);
                let args = args.iter().map(|arg| run(env, arg)).collect::<Vec<_>>();
                match fun {
                    Value::Fun { params, body } => {
                        assert!(params.len() == args.len());
                        let new_env = Env {
                            parent: Some(env),
                            vars: params.into_iter().zip(args).collect(),
                        };
                        run(&new_env, &body)
                    }
                    _ => panic!("type error"),
                }
            }
            Expr::Let {
                name,
                value,
                body,
                ..
            } => {
                let value = run(env, value);
                let new_env = Env {
                    parent: Some(env),
                    vars: HashMap::from([(name.ident.clone(), value)]),
                };
                run(&new_env, body)
            }
        }
    }
}

use std::{collections::HashMap, rc::Rc};

fn main() {
    let program = grammar::parse("fun main() { let x = 1 + 2 * 3; x }").unwrap();
    // println!("{:#?}", program);
    let env = interp::Env {
        parent: None,
        // bind each top-level function to its function name
        vars: HashMap::from_iter(program.definitions.iter().map(|def| match def {
            grammar::Definition::Fun { name, args, body, .. } => (
                name.ident.clone(),
                interp::Value::Fun {
                    params: args.iter().map(|arg| arg.ident.clone()).collect(),
                    body: Rc::new(body.clone()),
                },
            ),
        })),
    };
    let result = interp::run(&env, &grammar::Expr::Call {
        fun: Box::new(grammar::Expr::Var(grammar::Ident { ident: "main".to_string() })),
        args: vec![],
        _open_paren: (),
        _close_paren: (),
    });
    println!("{:?}", result);
}
