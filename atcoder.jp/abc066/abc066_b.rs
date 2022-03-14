#![allow(unused_macros, dead_code, unused_mut, unused_variables)]

#[macro_use]
mod reader {
    use std::io::{BufRead, BufReader, Read};
    use std::iter::Peekable;
    use std::str::{FromStr, SplitWhitespace};
    use std::mem::transmute;

    #[derive(Debug)]
    pub struct Reader<R: Read> {
        tokens: Peekable<SplitWhitespace<'static>>,
        line: Box<str>,
        reader: BufReader<R>,
    }

    impl<R: Read> Reader<R> {
        pub fn new(r: R) -> Reader<R> {
            Reader {
                tokens: "".split_whitespace().peekable(),
                line: "".to_string().into_boxed_str(),
                reader: BufReader::new(r),
            }
        }

        /// read line if needed
        fn prepare(&mut self) {
            while self.tokens.peek().is_none() {
                let mut line = String::new();
                let n = self.reader.read_line(&mut line).unwrap();
                if n == 0 {
                    return; // EOF
                }
                self.line = line.into_boxed_str();
                self.tokens = unsafe { transmute::<_, &'static str>(&*self.line) }.split_whitespace().peekable();
            }
        }

        pub fn next<T: FromStr>(&mut self) -> T {
            self.prepare();
            self.tokens.next().unwrap().parse().ok().unwrap()
        }
    }

    macro_rules! re {
        // Vec<$type>
        ($r:expr, [$type:ty; $len:expr]) => (
            (0..$len).map(|_| re!($r, $type)).collect::<Vec<$type>>()
        );
        // $type
        ($r:expr, $type:ty) => ($r.next::<$type>());
        // any type
        ($r:expr) => ($r.next());
    }
}

#[macro_use]
mod writer {
    use std::fmt::Display;
    use std::io::{BufWriter, Write};

    //#region Writable
    pub trait Writable<Mode> {
        fn write_to<W: Write>(self, w: &mut W, fmt: &Format);
    }

    #[non_exhaustive]
    pub struct One;

    #[non_exhaustive]
    pub struct Many;

    #[non_exhaustive]
    pub struct Slice;

    impl<T: Display> Writable<One> for T {
        fn write_to<W: Write>(self, w: &mut W, fmt: &Format) {
            write!(w, "{}{}", self, fmt.end).unwrap()
        }
    }

    impl<I> Writable<Many> for I where I: Iterator, I::Item: Display {
        fn write_to<W: Write>(mut self, w: &mut W, fmt: &Format) {
            if let Some(v) = self.next() {
                write!(w, "{}", v).unwrap()
            } else { return; }
            for v in self {
                write!(w, "{}{}", fmt.sep, v).unwrap();
            }
            write!(w, "{}", fmt.end).unwrap();
        }
    }

    impl<T: Display> Writable<Slice> for &[T] {
        fn write_to<W: Write>(self, w: &mut W, fmt: &Format) {
            self.iter().write_to(w, fmt);
        }
    }
    //#endregion Writable

    //#region Writer
    #[derive(Debug)]
    pub struct Writer<W: Write> {
        writer: BufWriter<W>,
    }

    #[derive(Debug, Clone)]
    pub struct Format<'a> {
        sep: &'a str,
        end: &'a str,
    }

    impl Default for Format<'_> {
        fn default() -> Self {
            Self {
                sep: " ",
                end: "\n",
            }
        }
    }

    impl<W: Write> Writer<W> {
        pub fn new(w: W) -> Self {
            Self { writer: BufWriter::new(w) }
        }
        pub fn write<M, T: Writable<M>>(&mut self, v: T) {
            v.write_to(&mut self.writer, &Format::default())
        }
    }
    //#endregion Writer
    macro_rules! wr {
        ($w:expr, $v:expr)=>($w.write($v))
    }
    macro_rules! yneos {
        ($w:expr, $v:expr) => ($w.write(if $v { "yes" } else { "no" }));
    }
}

use std::{fs::File, io::{Read, Write}};
use reader::Reader;
use writer::Writer;

fn solve<R: Read, W: Write>(mut r: Reader<R>, mut w: Writer<W>) {
    let s: String = re!(r);
    let l = s.len();
    for (a, b) in (2..l).step_by(2).rev().map(|i| (i / 2, i)) {
        if s[..a] == s[a..b] {
            return wr!(w,b);
        }
    }
}

#[cfg(debug_assertions)]
fn main() {
    solve(
        Reader::new(File::open("input.txt").unwrap()),
        // Writer::new(File::create("output.txt").unwrap()),
        Writer::new(std::io::stdout()),
    )
}

#[cfg(not(debug_assertions))]
fn main() {
    let stdin = std::io::stdin();
    solve(
        Reader::new(stdin.lock()),
        Writer::new(std::io::stdout()),
    );
}
