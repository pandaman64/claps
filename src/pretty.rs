//! Implementation of Wadler's pretty printer, following "Strictly Pretty" by Christian Lindig for strictness.
use std::fmt::Write;

#[derive(Debug, Default)]
struct ByteCounter {
    bytes: usize,
}

impl ByteCounter {
    pub fn new() -> Self {
        Self { bytes: 0 }
    }

    pub fn len(&self) -> usize {
        self.bytes
    }
}

impl std::fmt::Write for ByteCounter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.bytes += s.len();
        Ok(())
    }
}

#[derive(Clone)]
pub enum List<T> {
    Nil,
    Cons(T, Box<List<T>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Flat,
    Break,
}

#[derive(Clone, Copy)]
pub struct Text<'a> {
    pub length: usize,
    pub text: &'a (dyn std::fmt::Display + 'a),
}

impl<'a> Text<'a> {
    pub fn from_display(text: &'a (dyn std::fmt::Display + 'a)) -> Self {
        let mut counter = ByteCounter::new();
        write!(counter, "{}", text).unwrap();
        Self {
            length: counter.len(),
            text,
        }
    }
}

#[derive(Clone)]
pub enum Doc<'a> {
    Nil,
    Cons(Box<Doc<'a>>, Box<Doc<'a>>),
    Nest(usize, Box<Doc<'a>>),
    Text(Text<'a>),
    Break(Text<'a>),
    Group(Box<Doc<'a>>),
}

impl Doc<'_> {
    pub fn nest(n: usize, doc: Doc<'_>) -> Doc<'_> {
        Doc::Nest(n, Box::new(doc))
    }

    pub fn space() -> Doc<'static> {
        Doc::Break(Text::from_display(&" "))
    }

    pub fn group(doc: Doc<'_>) -> Doc<'_> {
        Doc::Group(Box::new(doc))
    }

    pub fn seq<'a, I: DoubleEndedIterator<Item = Doc<'a>>>(iter: I) -> Doc<'a> {
        iter.rfold(Doc::Nil, |acc, doc| match (acc, doc) {
            (Doc::Nil, doc) => doc,
            (acc, Doc::Nil) => acc,
            (acc, doc) => Doc::Cons(Box::new(doc), Box::new(acc)),
        })
    }
}

pub enum SDoc<'a> {
    Nil,
    Text(Text<'a>, Box<SDoc<'a>>),
    Line(usize, Box<SDoc<'a>>),
}

impl std::fmt::Display for SDoc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SDoc::Nil => Ok(()),
            SDoc::Text(text, rest) => {
                write!(f, "{}", text.text)?;
                rest.fmt(f)
            }
            SDoc::Line(indent, rest) => {
                writeln!(f)?;
                for _ in 0..*indent {
                    write!(f, " ")?;
                }
                rest.fmt(f)
            }
        }
    }
}

fn fits(width: usize, used: usize, docs: List<(usize, Mode, &Doc<'_>)>) -> bool {
    if used > width {
        false
    } else {
        match docs {
            List::Nil => true,
            List::Cons(head, rest) => match head {
                (_, _, Doc::Nil) => fits(width, used, *rest),
                (i, m, Doc::Cons(a, b)) => fits(
                    width,
                    used,
                    List::Cons((i, m, a), Box::new(List::Cons((i, m, b), rest))),
                ),
                (i, m, Doc::Nest(j, d)) => fits(width, used, List::Cons((i + j, m, d), rest)),
                (_, _, Doc::Text(text)) => fits(width, used + text.length, *rest),
                (i, Mode::Flat, Doc::Break(text)) => fits(width, i + text.length, *rest),
                (_, Mode::Break, Doc::Break(_text)) => unreachable!(),
                (i, _, Doc::Group(d)) => fits(width, used, List::Cons((i, Mode::Flat, d), rest)),
            },
        }
    }
}

// format() in the paper
fn simplify<'a>(width: usize, used: usize, docs: List<(usize, Mode, &Doc<'a>)>) -> SDoc<'a> {
    match docs {
        List::Nil => SDoc::Nil,
        List::Cons(head, rest) => match head {
            (_, _, Doc::Nil) => simplify(width, used, *rest),
            (i, m, Doc::Cons(a, b)) => simplify(
                width,
                used,
                List::Cons((i, m, a), Box::new(List::Cons((i, m, b), rest))),
            ),
            (i, m, Doc::Nest(j, d)) => simplify(width, used, List::Cons((i + j, m, d), rest)),
            (_, _, Doc::Text(text)) => {
                SDoc::Text(*text, Box::new(simplify(width, used + text.length, *rest)))
            }
            (_, Mode::Flat, Doc::Break(text)) => {
                SDoc::Text(*text, Box::new(simplify(width, used + text.length, *rest)))
            }
            (i, Mode::Break, Doc::Break(_text)) => {
                SDoc::Line(i, Box::new(simplify(width, i, *rest)))
            }
            // TODO: I don't like clone here
            (i, _, Doc::Group(d)) => {
                if fits(width, used, List::Cons((i, Mode::Flat, d), rest.clone())) {
                    simplify(width, used, List::Cons((i, Mode::Flat, d), rest))
                } else {
                    simplify(width, used, List::Cons((i, Mode::Break, d), rest))
                }
            }
        },
    }
}

pub fn format<'a>(width: usize, doc: &'a Doc<'a>) -> impl std::fmt::Display + 'a {
    simplify(
        width,
        0,
        List::Cons((0, Mode::Flat, doc), Box::new(List::Nil)),
    )
}
