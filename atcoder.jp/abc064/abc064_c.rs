#![allow(unused_macros, dead_code, unused_mut, unused_variables, non_snake_case, non_upper_case_globals)]

//#region mod
#[macro_use]
mod reader {
    use std::io::{BufRead, BufReader, Read};
    use std::iter::Peekable;
    use std::str::{FromStr, SplitWhitespace};
    use std::mem::transmute;

    #[derive(Debug)]
    pub struct Reader<R: Read> {
        pub reader: BufReader<R>,
        tokens: Peekable<SplitWhitespace<'static>>,
        line: Box<str>,
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

        pub fn i(&mut self) -> i32 { self.next::<i32>() }
        pub fn us(&mut self) -> usize { self.next::<usize>() }
        pub fn str(&mut self) -> String { self.next::<String>() }
        pub fn bytes(&mut self) -> Vec<u8> { self.next::<String>().into_bytes() }
    }

    macro_rules! re {
        // re!(r, [i32;20]) -> Vec<i32> (with len 20)
        ($r:expr, [$type:ty; $len:expr]) => ((0..$len).map(|_| re!($r, $type)).collect::<Vec<$type>>());
        // re!(r, usize) -> usize
        ($r:expr, $type:ty) => ($r.next::<$type>());
        // re!(r, usize, i32, String) -> (usize, i32, String)
        ($r:expr, $($type:ty),*) => (($(re!($r, $type)),*));
    }
}

mod writer {
    use std::fmt::Display;
    use std::io::{BufWriter, Write};

    //#region Writable
    pub trait Writable<Mode> {
        fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str);
    }

    #[non_exhaustive]
    pub struct Slice;

    #[non_exhaustive]
    pub struct Many;

    #[non_exhaustive]
    pub struct One;

    impl<T: Display> Writable<Slice> for &[T] {
        fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str) {
            self.iter().write_to(w, sep, end);
        }
    }

    impl<I> Writable<Many> for I where I: Iterator, I::Item: Display {
        fn write_to<W: Write>(mut self, w: &mut W, sep: &str, end: &str) {
            if let Some(val) = self.next() {
                write!(w, "{}", val).unwrap();
            } else { return; }

            self.for_each(|val| write!(w, "{}{}", sep, val).unwrap());
            write!(w, "{}", end).unwrap();
        }
    }

    impl<T: Display> Writable<One> for T {
        fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str) {
            write!(w, "{}{}", self, end).unwrap()
        }
    }
    //#endregion Writable

    //#region Writer
    #[derive(Debug)]
    pub struct Writer<W: Write> {
        pub writer: BufWriter<W>,
    }

    impl<W: Write> Writer<W> {
        pub fn new(w: W) -> Self { Self { writer: BufWriter::new(w) } }

        pub fn yneos(&mut self, b: bool) {
            self.writer.write((if b { "yes" } else { "no" }).as_bytes()).unwrap();
        }

        pub fn wr<M, T: Writable<M>>(&mut self, val: T) {
            //! write with no separator
            val.write_to(&mut self.writer, "", "\n");
        }
        pub fn ws<M, T: Writable<M>>(&mut self, val: T) {
            //! write space-separated<br>
            //! `ws(&[a,b,c][..])` to write multiple variables
            val.write_to(&mut self.writer, " ", "\n")
        }
        pub fn write<M, T: Writable<M>>(&mut self, val: T, sep: &str, end: &str) {
            val.write_to(&mut self.writer, sep, end);
        }
    }
}
//#endregion mod

use std::{fs::File, io::{Read, Write}};
use std::collections::HashSet;
use reader::Reader;
use writer::Writer;

//#region constant
const d8: [(i32, i32); 8] = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];
//#endregion constant

fn solve<R: Read, W: Write>(mut re: Reader<R>, mut wr: Writer<W>) {
    let mut b = 0;
    let mut h = HashSet::new();
    re!(re,[usize;re.us()]).iter().cloned().map(|m| m / 400).for_each(
        |m| if m < 8 { h.insert(m); } else { b += 1 }
    );
    let a = h.len();
    wr.ws(&[a.max(1), a + b][..]);
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
    let (stdin, stdout) = (std::io::stdin(), std::io::stdout());
    solve(
        Reader::new(stdin.lock()),
        Writer::new(stdout.lock()),
    );
}
