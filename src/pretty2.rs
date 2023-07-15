use std::fmt::Write;

pub struct DocSeq<'a> {
    docs: Vec<Doc<'a>>,
}

impl<'a> From<Vec<Doc<'a>>> for DocSeq<'a> {
    fn from(docs: Vec<Doc<'a>>) -> Self {
        Self { docs }
    }
}

impl DocSeq<'_> {
    /// Check if the the sequence fits in the given width, when rendered in a single line.
    fn fits(&self, width: usize) -> Result<usize, ()> {
        let mut used = 0;
        for doc in self.docs.iter() {
            match doc {
                Doc::Text(text) => used += text.length,
                Doc::Breakable(text) => used += text.length,
                Doc::Nest(_, seq) => used += seq.fits(width - used)?,
                Doc::Group(seq) => used += seq.fits(width - used)?,
            }
            if used > width {
                return Err(());
            }
        }
        Ok(used)
    }

    fn do_format<W: Write>(
        &self,
        writer: &mut W,
        width: usize,
        initial_used: usize,
        indent: usize,
        mode: Mode,
    ) -> Result<usize, std::fmt::Error> {
        let mut used = initial_used;

        for doc in self.docs.iter() {
            match doc {
                Doc::Text(text) => {
                    write!(writer, "{}", text.text)?;
                    used += text.length;
                }
                Doc::Breakable(text) => match mode {
                    Mode::Flat => {
                        write!(writer, "{}", text.text)?;
                        used += text.length;
                    }
                    Mode::Break => {
                        writeln!(writer)?;
                        for _ in 0..indent {
                            write!(writer, " ")?;
                        }
                        used = indent;
                    }
                },
                Doc::Nest(i, seq) => {
                    used = seq.do_format(writer, width, initial_used, indent + i, mode)?;
                }
                Doc::Group(seq) => {
                    if used <= width && seq.fits(width - used).is_ok() {
                        used = seq.do_format(writer, width, initial_used, indent, Mode::Flat)?;
                    } else {
                        used = seq.do_format(writer, width, initial_used, indent, Mode::Break)?;
                    }
                }
            }
        }

        Ok(used)
    }

    pub fn format<W: Write>(&self, writer: &mut W, width: usize) -> Result<usize, std::fmt::Error> {
        // Inline `Self(vec![Doc::Group(self.clone())]).do_format(writer, width, 0, 0, Mode::Flat)`.
        if self.fits(width).is_ok() {
            self.do_format(writer, width, 0, 0, Mode::Flat)
        } else {
            self.do_format(writer, width, 0, 0, Mode::Break)
        }
    }

    pub fn to_string(&self, width: usize) -> String {
        let mut ret = String::new();
        // Writing to String doesn't fail.
        self.format(&mut ret, width).unwrap();
        ret
    }
}

pub enum Doc<'a> {
    Text(Text<'a>),
    Breakable(Text<'a>),
    Nest(usize, DocSeq<'a>),
    Group(DocSeq<'a>),
}

impl<'a> Doc<'a> {
    pub fn text(text: impl Into<Text<'a>>) -> Self {
        Self::Text(text.into())
    }

    pub fn breakable(text: impl Into<Text<'a>>) -> Self {
        Self::Breakable(text.into())
    }

    pub fn space() -> Self {
        Self::breakable(&" ")
    }

    pub fn nest(indent: usize, seq: impl Into<DocSeq<'a>>) -> Self {
        Self::Nest(indent, seq.into())
    }

    pub fn group(seq: impl Into<DocSeq<'a>>) -> Self {
        Self::Group(seq.into())
    }
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

impl<'a> From<&'a &'static str> for Text<'a> {
    fn from(text: &'a &'static str) -> Self {
        Self {
            length: text.len(),
            text,
        }
    }
}

impl<'a> From<&'a (dyn std::fmt::Display + 'a)> for Text<'a> {
    fn from(text: &'a (dyn std::fmt::Display + 'a)) -> Self {
        let mut counter = ByteCounter::new();
        write!(counter, "{}", text).unwrap();
        Self {
            length: counter.len(),
            text,
        }
    }
}

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple() {
        let doc = DocSeq::from(vec![
            Doc::text(&"hello"),
            Doc::space(),
            Doc::text(&"world"),
            Doc::space(),
            Doc::text(&"foobar"),
        ]);
        assert_eq!(doc.to_string(80), "hello world foobar");
        assert_eq!(doc.to_string(18), "hello world foobar");
        assert_eq!(doc.to_string(17), "hello\nworld\nfoobar");
        assert_eq!(doc.to_string(10), "hello\nworld\nfoobar");
    }

    #[test]
    fn test_nest() {
        let doc = DocSeq::from(vec![
            Doc::text(&"hello-world"),
            Doc::nest(
                2,
                vec![
                    Doc::space(),
                    Doc::text(&"foobar"),
                    Doc::space(),
                    Doc::text(&"baz"),
                ],
            ),
        ]);
        assert_eq!(doc.to_string(80), "hello-world foobar baz");
        assert_eq!(doc.to_string(22), "hello-world foobar baz");
        assert_eq!(doc.to_string(21), "hello-world\n  foobar\n  baz");
        assert_eq!(doc.to_string(10), "hello-world\n  foobar\n  baz");
    }

    #[test]
    fn test_nest_group() {
        let doc = DocSeq::from(vec![
            Doc::text(&"hello-world"),
            Doc::nest(
                2,
                vec![
                    Doc::space(),
                    Doc::group(vec![Doc::text(&"foobar"), Doc::space(), Doc::text(&"baz")]),
                ],
            ),
        ]);
        assert_eq!(doc.to_string(80), "hello-world foobar baz");
        assert_eq!(doc.to_string(22), "hello-world foobar baz");
        assert_eq!(doc.to_string(21), "hello-world\n  foobar baz");
        assert_eq!(doc.to_string(12), "hello-world\n  foobar baz");
        assert_eq!(doc.to_string(11), "hello-world\n  foobar\n  baz");
        assert_eq!(doc.to_string(10), "hello-world\n  foobar\n  baz");
    }

    #[test]
    fn test_bracket() {
        let doc = DocSeq::from(vec![
            Doc::text(&"foo"),
            Doc::text(&"("),
            Doc::nest(
                2,
                vec![
                    Doc::breakable(&""),
                    Doc::text(&"100"),
                    Doc::text(&","),
                    Doc::space(),
                    Doc::text(&"200"),
                    Doc::text(&","),
                    Doc::space(),
                    Doc::text(&"300"),
                ],
            ),
            Doc::breakable(&""),
            Doc::text(&")"),
        ]);
        assert_eq!(doc.to_string(80), "foo(100, 200, 300)");
        assert_eq!(doc.to_string(18), "foo(100, 200, 300)");
        assert_eq!(doc.to_string(17), "foo(\n  100,\n  200,\n  300\n)");
        assert_eq!(doc.to_string(10), "foo(\n  100,\n  200,\n  300\n)");
    }

    #[test]
    fn test_group_after_long_text() {
        let doc = DocSeq::from(vec![
            Doc::text(&"hello-world"),
            Doc::group(vec![Doc::space(), Doc::text(&"foobar")]),
        ]);
        assert_eq!(doc.to_string(10), "hello-world\nfoobar");
    }
}
