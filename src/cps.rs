use crate::ast::{BinOp, Definition, Expr, Ident, IdentGenerator, Program};

#[derive(Debug)]
pub enum CValue {
    Int(i64),
    Var(Ident),
    Label(Ident),
}

#[derive(Debug)]
pub struct CDefinition {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: CExpr,
}

#[derive(Debug)]
pub enum CExpr {
    App {
        fun: CValue,
        args: Vec<CValue>,
    },
    Fix {
        defs: Vec<CDefinition>,
        body: Box<CExpr>,
    },
    If {
        cond: CValue,
        then_branch: Box<CExpr>,
        else_branch: Box<CExpr>,
    },
    PrimOp(PrimOp),
}

#[derive(Debug)]
pub enum PrimOp {
    BinOp {
        op: BinOp,
        left: CValue,
        right: CValue,
        var: Ident,
        cont: Box<CExpr>,
    },
}

#[derive(Debug)]
pub struct Converter {
    ident_generator: IdentGenerator,
}

impl Converter {
    pub fn new(ident_generator: IdentGenerator) -> Self {
        Self { ident_generator }
    }

    fn fresh_var(&mut self, prefix: &str) -> Ident {
        self.ident_generator.fresh_var(prefix)
    }

    pub fn convert(&mut self, program: &Program) -> Vec<CDefinition> {
        program
            .definitions
            .iter()
            .map(|def| self.convert_definition(def))
            .collect()
    }

    fn convert_definition(&mut self, def: &Definition) -> CDefinition {
        match def {
            Definition::Fun {
                name, args, body, ..
            } => {
                // Continuation for this function
                let fun_cont_var = self.fresh_var("k_fun");

                let body = self.convert_expr(body, |_, body| CExpr::App {
                    fun: CValue::Var(fun_cont_var.clone()),
                    args: vec![body],
                });

                CDefinition {
                    name: name.clone(),
                    args: {
                        let mut args: Vec<_> = args.clone();
                        args.push(fun_cont_var);
                        args
                    },
                    body,
                }
            }
        }
    }

    fn convert_expr<F: FnOnce(&mut Converter, CValue) -> CExpr>(
        &mut self,
        expr: &Expr,
        cont: F,
    ) -> CExpr {
        match expr {
            Expr::Var(ident) => cont(self, CValue::Var(ident.clone())),
            Expr::Number { value } => cont(self, CValue::Int(*value)),
            Expr::BinOp { left, op, right } => self.convert_binop(*op, left, right, cont),
            Expr::Call { fun, args, .. } => {
                // Define a continuation function that takes the result of the function call and passes it to the given cont.
                let call_cont_var = self.fresh_var("k_call");
                let call_cont_arg = self.fresh_var("k_call_result");

                CExpr::Fix {
                    defs: vec![CDefinition {
                        name: call_cont_var.clone(),
                        args: vec![call_cont_arg.clone()],
                        body: cont(self, CValue::Var(call_cont_arg)),
                    }],
                    body: Box::new(self.convert_expr(fun, move |this, fun| {
                        this.convert_call(fun, CValue::Var(call_cont_var), args, vec![])
                    })),
                }
            }
            Expr::Let {
                name, value, body, ..
            } => self.convert_let(name, value, body, cont),
            Expr::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => self.convert_if(cond, then_branch, else_branch, cont),
        }
    }

    fn convert_binop<F: FnOnce(&mut Converter, CValue) -> CExpr>(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
        cont: F,
    ) -> CExpr {
        self.convert_expr(left, move |this, left| {
            this.convert_expr(right, move |this, right| {
                let var = this.fresh_var(op.var_prefix());
                CExpr::PrimOp(PrimOp::BinOp {
                    op,
                    left,
                    right,
                    var: var.clone(),
                    cont: Box::new(cont(this, CValue::Var(var))),
                })
            })
        })
    }

    fn convert_call(
        &mut self,
        fun: CValue,
        k_call: CValue,
        args: &[Expr],
        mut args_accum: Vec<CValue>,
    ) -> CExpr {
        match args {
            [] => CExpr::App {
                fun,
                args: {
                    args_accum.push(k_call);
                    args_accum
                },
            },
            [arg, rest @ ..] => self.convert_expr(arg, move |this, arg| {
                args_accum.push(arg);
                this.convert_call(fun, k_call, rest, args_accum)
            }),
        }
    }

    fn convert_let<F: FnOnce(&mut Converter, CValue) -> CExpr>(
        &mut self,
        name: &Ident,
        value: &Expr,
        body: &Expr,
        cont: F,
    ) -> CExpr {
        CExpr::Fix {
            defs: vec![{
                let let_cont_arg = self.fresh_var("k_let");
                CDefinition {
                    name: name.clone(),
                    args: vec![let_cont_arg.clone()],
                    body: self.convert_expr(value, move |_, value| CExpr::App {
                        fun: CValue::Var(let_cont_arg),
                        args: vec![value],
                    }),
                }
            }],
            body: Box::new(self.convert_expr(body, cont)),
        }
    }

    fn convert_if<F: FnOnce(&mut Converter, CValue) -> CExpr>(
        &mut self,
        cond: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
        cont: F,
    ) -> CExpr {
        self.convert_expr(cond, move |this, cond| {
            // Define a continuation function that takes a boolean and passes it to the given cont.
            // NOTE: we need to introduce a new variable to bound the size of converted form.
            let cont_var = this.fresh_var("k_if");
            let cont_arg = this.fresh_var("k_if_arg");
            let k = CDefinition {
                name: cont_var.clone(),
                args: vec![cont_arg.clone()],
                body: cont(this, CValue::Var(cont_arg)),
            };

            // Introduced the continuation function, and let each branch call it with the result.
            CExpr::Fix {
                defs: vec![k],
                body: Box::new(CExpr::If {
                    cond,
                    then_branch: Box::new(this.convert_expr(then_branch, {
                        let cont_var = cont_var.clone();
                        move |_, then_branch| CExpr::App {
                            fun: CValue::Var(cont_var),
                            args: vec![then_branch],
                        }
                    })),
                    else_branch: Box::new(this.convert_expr(else_branch, move |_, else_branch| {
                        CExpr::App {
                            fun: CValue::Var(cont_var),
                            args: vec![else_branch],
                        }
                    })),
                }),
            }
        })
    }
}
