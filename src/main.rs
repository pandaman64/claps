use crate::ast::IdentGenerator;

#[rust_sitter::grammar("claps")]
mod grammar {
    use crate::ast::BinOp;

    #[rust_sitter::language]
    #[derive(Debug)]
    pub struct Program {
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
        #[rust_sitter::prec_left(2)]
        Greater {
            left: Box<Expr>,
            #[rust_sitter::leaf(pattern = r">", transform = BinOp::from_str)]
            op: BinOp,
            right: Box<Expr>,
        },
        #[rust_sitter::prec_left(3)]
        Add {
            left: Box<Expr>,
            #[rust_sitter::leaf(pattern = r"\+|-", transform = BinOp::from_str)]
            op: BinOp,
            right: Box<Expr>,
        },
        #[rust_sitter::prec_left(4)]
        Mul {
            left: Box<Expr>,
            #[rust_sitter::leaf(pattern = r"\*", transform = BinOp::from_str)]
            op: BinOp,
            right: Box<Expr>,
        },
        #[rust_sitter::prec(5)]
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
        If {
            #[rust_sitter::leaf(text = "if")]
            _if: (),
            cond: Box<Expr>,
            #[rust_sitter::leaf(text = "{")]
            _open_then_body: (),
            then_branch: Box<Expr>,
            #[rust_sitter::leaf(text = "}")]
            _close_then_body: (),
            #[rust_sitter::leaf(text = "else")]
            _else: (),
            #[rust_sitter::leaf(text = "{")]
            _open_else_body: (),
            else_branch: Box<Expr>,
            #[rust_sitter::leaf(text = "}")]
            _close_else_body: (),
        },
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

mod ast;
mod cps;
mod interp;
mod pretty;

fn main() {
    let program = grammar::parse(
        r#"
fun is_odd(x) {
    if x > 0 {
        is_even(x - 1)
    } else {
        1 > 0
    }
}

fun is_even(x) {
    if x > 0 {
        is_odd(x - 1)
    } else {
        0 > 1
    }
}

fun times(x, y) {
    x * y
}

fun main() {
    let x = 1 + 2 * 3;
    if is_even(x) {
        times(x, x)
    } else {
        x
    }
}
"#,
    )
    .unwrap();
    // println!("{:#?}", program);
    println!("{:?}", interp::run(&program));

    let mut ident_generator = IdentGenerator::default();

    let (toplevel_map, program) = ast::alpha_conversion(&mut ident_generator, &program);
    // TODO: add breakable spaces and debug
    println!("{}", program.pretty().to_string(60));

    let cps_definitions = cps::Converter::new(ident_generator).convert(&program);
    // println!("{:#?}", cps_definitions);
}
