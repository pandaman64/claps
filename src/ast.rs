use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Greater,
}

impl BinOp {
    pub fn from_str(op: &str) -> Self {
        match op {
            "+" => Self::Add,
            "-" => Self::Minus,
            "*" => Self::Mul,
            ">" => Self::Greater,
            _ => panic!("unknown operator {}", op),
        }
    }

    pub fn var_prefix(&self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Minus => "minus",
            Self::Mul => "mul",
            Self::Greater => "greater",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    original: Rc<str>,
    id: usize,
}

#[derive(Default, Debug)]
pub struct IdentGenerator {
    next_id: usize,
}

impl IdentGenerator {
    pub fn fresh_var(&mut self, prefix: &str) -> Ident {
        let ident = Ident {
            original: Rc::from(prefix),
            id: self.next_id,
        };
        self.next_id += 1;
        ident
    }
}

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug)]
pub enum Definition {
    Fun {
        name: Ident,
        args: Vec<Ident>,
        body: Expr,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Ident),
    Number {
        value: i64,
    },
    BinOp {
        left: Rc<Expr>,
        op: BinOp,
        right: Rc<Expr>,
    },
    Call {
        fun: Rc<Expr>,
        args: Vec<Expr>,
    },
    Let {
        name: Ident,
        value: Rc<Expr>,
        body: Rc<Expr>,
    },
    If {
        cond: Rc<Expr>,
        then_branch: Rc<Expr>,
        else_branch: Rc<Expr>,
    },
}

struct Env<'parent, 'ident> {
    parent: Option<&'parent Env<'parent, 'ident>>,
    vars: HashMap<&'ident crate::grammar::Ident, Ident>,
}

impl Env<'_, '_> {
    fn lookup(&self, ident: &crate::grammar::Ident) -> Option<&Ident> {
        self.vars.get(ident).or_else(|| self.parent?.lookup(ident))
    }
}

pub fn alpha_conversion<'ident>(
    generator: &mut IdentGenerator,
    program: &'ident crate::grammar::Program,
) -> (HashMap<&'ident crate::grammar::Ident, Ident>, Program) {
    let vars = program
        .definitions
        .iter()
        .map(|def| match def {
            crate::grammar::Definition::Fun {
                name, ..
            } => (name, generator.fresh_var(&name.ident)),
        })
        .collect();
    let env = Env { parent: None, vars };
    let definitions = program
        .definitions
        .iter()
        .map(|def| alpha_conversion_definition(generator, &env, def))
        .collect();
    (env.vars, Program { definitions })
}

fn alpha_conversion_definition(
    generator: &mut IdentGenerator,
    env: &Env<'_, '_>,
    def: &crate::grammar::Definition,
) -> Definition {
    match def {
        crate::grammar::Definition::Fun {
            name, args, body, ..
        } => {
            let converted_name = env.lookup(name).unwrap();
            let new_env = Env {
                parent: Some(env),
                vars: HashMap::from_iter(
                    args.iter()
                        .map(|arg| (arg, generator.fresh_var(&arg.ident))),
                ),
            };
            let body = alpha_conversion_expr(generator, &new_env, body);
            Definition::Fun {
                name: converted_name.clone(),
                args: args
                    .iter()
                    .map(|arg| new_env.lookup(arg).unwrap().clone())
                    .collect(),
                body,
            }
        }
    }
}

fn alpha_conversion_expr(
    generator: &mut IdentGenerator,
    env: &Env<'_, '_>,
    expr: &crate::grammar::Expr,
) -> Expr {
    match expr {
        crate::grammar::Expr::Var(ident) => {
            let ident = env.lookup(ident).expect("variable not found");
            Expr::Var(ident.clone())
        }
        crate::grammar::Expr::Number { value } => Expr::Number { value: *value },
        crate::grammar::Expr::Greater { left, op, right }
        | crate::grammar::Expr::Add { left, op, right }
        | crate::grammar::Expr::Mul { left, op, right } => {
            let left = alpha_conversion_expr(generator, env, left);
            let right = alpha_conversion_expr(generator, env, right);
            Expr::BinOp {
                left: Rc::new(left),
                op: *op,
                right: Rc::new(right),
            }
        }
        crate::grammar::Expr::Call { fun, args, .. } => {
            let fun = alpha_conversion_expr(generator, env, fun);
            let args = args
                .iter()
                .map(|arg| alpha_conversion_expr(generator, env, arg))
                .collect();
            Expr::Call {
                fun: Rc::new(fun),
                args,
            }
        }
        crate::grammar::Expr::Let {
            _let,
            name,
            value,
            body,
            ..
        } => {
            let converted_name = generator.fresh_var(&name.ident);

            let value = alpha_conversion_expr(generator, env, value);
            let new_env = Env {
                parent: Some(env),
                vars: HashMap::from_iter([(name, converted_name.clone())]),
            };
            let body = alpha_conversion_expr(generator, &new_env, body);
            Expr::Let {
                name: converted_name,
                value: Rc::new(value),
                body: Rc::new(body),
            }
        }
        crate::grammar::Expr::If {
            cond,
            then_branch,
            _close_then_body,
            else_branch,
            ..
        } => {
            let cond = alpha_conversion_expr(generator, env, cond);
            let then_branch = alpha_conversion_expr(generator, env, then_branch);
            let else_branch = alpha_conversion_expr(generator, env, else_branch);
            Expr::If {
                cond: Rc::new(cond),
                then_branch: Rc::new(then_branch),
                else_branch: Rc::new(else_branch),
            }
        }
    }
}
