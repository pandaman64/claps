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
            #[rust_sitter::delimited(#[rust_sitter::leaf(text = ";")] ())]
            body: Vec<Expr>,
            #[rust_sitter::leaf(text = "}")]
            _close_body: (),
        }
    }

    #[derive(Debug)]
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
        Call {
            fun: Box<Expr>,
            #[rust_sitter::leaf(text = "(")]
            _open_paren: (),
            #[rust_sitter::delimited(#[rust_sitter::leaf(text = ",")] ())]
            args: Vec<Expr>,
            #[rust_sitter::leaf(text = ")")]
            _close_paren: (),
        },
    }
    
    #[derive(Debug)]
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

fn main() {
    println!("{:?}", grammar::parse("fun main() { 1 + 2 * 3; f(v) }"));
}
