// 2023-04-22 https://atcoder.jp/contests/abc296/tasks/abc296_a
#![allow(unused_imports, unused_macros, unused_variables, unused_mut, dead_code)]

use std::io::{Read, Write};

use itertools::Itertools;
use reader::Reader;
use writer::Writer;

// https://github.com/statiolake/proconio-rs/blob/master/proconio/src/source/line.rs
#[macro_use]
mod reader {
    use std::{
        any::type_name,
        io::{BufRead, BufReader, Read},
        iter::Peekable,
        mem::transmute,
        str::{FromStr, SplitWhitespace},
    };

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
                let n = self.reader.read_line(&mut line).expect("Failed to read line!");
                if n == 0 {
                    return; /* EOF */
                }

                self.line = line.into_boxed_str();
                self.tokens = unsafe { transmute::<_, &'static str>(&*self.line) }
                    .split_whitespace()
                    .peekable();
            }
        }

        /// get next token
        pub fn next<T: FromStr>(&mut self) -> T {
            self.prepare();
            match self.tokens.next() {
                Some(token) => match token.parse() {
                    Ok(value) => value,
                    Err(..) => panic!("Cannot parse {} as {}", token, type_name::<T>()),
                },
                None => panic!("Token is empty while trying to read {}", type_name::<T>()),
            }
        }

        pub fn i(&mut self) -> i64 {
            self.next::<i64>()
        }
        pub fn f(&mut self) -> f64 {
            self.next::<f64>()
        }
        pub fn u(&mut self) -> usize {
            self.next::<usize>()
        }
        pub fn u1(&mut self) -> usize {
            self.next::<usize>().checked_sub(1).expect("Attempted read 0 as usize1")
        }
        pub fn c(&mut self) -> char {
            self.next::<char>()
        }
        pub fn s(&mut self) -> String {
            self.next::<String>()
        }
        pub fn b(&mut self) -> Vec<u8> {
            self.next::<String>().into_bytes()
        }
        pub fn ignore(&mut self) {
            self.next::<String>();
        }
    }

    // read string as bytes
    macro_rules! rb {
        ($re:expr) => {
            $re.next::<String>().bytes()
        };
    }

    macro_rules! r {
        // read iter, e.g. r!(re, [i32;n]).collect::<HashSet<_>>()
        ($re:expr, [$type:ty; $len:expr]) => ((0..$len).map(|_| $re.next::<$type>()));
        // read tuple, e.g. r!(re, usize, i32, String)
        ($re:expr, $($type:ty),+) => (($($re.next::<$type>()),+));
        // read tuple (infered types) e.g. r!(re, 3);
        ($re:expr, $n:expr) => ((0..$n).map(|_| $re.next()).collect_tuple().expect("Unable to collect tuple"));
    }
}

#[macro_use]
mod writer {
    use std::{
        fmt::Display,
        io::{BufWriter, Write},
    };

    //#region Writable Trait
    pub trait Writable<Mode> {
        fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str);
    }

    #[non_exhaustive]
    pub struct One;

    impl<T: Display> Writable<One> for T {
        fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str) {
            write!(w, "{}{}", self, end).unwrap();
        }
    }

    #[non_exhaustive]
    pub struct Iter;

    impl<I> Writable<Iter> for I
    where
        I: Iterator,
        I::Item: Display,
    {
        fn write_to<W: Write>(mut self, w: &mut W, sep: &str, end: &str) {
            if let Some(val) = self.next() {
                write!(w, "{}", val).unwrap();
            } else {
                return;
            }

            self.for_each(|val| write!(w, "{}{}", sep, val).unwrap());
            write!(w, "{}", end).unwrap();
        }
    }

    #[non_exhaustive]
    pub struct Slice;

    impl<T: Display> Writable<Slice> for &[T] {
        fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str) {
            self.iter().write_to(w, sep, end);
        }
    }
    //#endregion Writable Trait

    #[derive(Debug)]
    pub struct Writer<W: Write> {
        pub writer: BufWriter<W>,
    }

    impl<W: Write> Writer<W> {
        pub fn new(w: W) -> Self {
            Self { writer: BufWriter::new(w) }
        }
        /// write "Yes\n" or "No\n" given boolean
        pub fn y(&mut self, b: bool) {
            self.writer.write_all(if b { "Yes\n" } else { "No\n" }.as_bytes()).unwrap();
        }
        /// no sep, end with '\n'
        pub fn w<M, T: Writable<M>>(&mut self, v: T) {
            v.write_to(&mut self.writer, "", "");
        }
        /// no sep, end with '\n'
        pub fn n<M, T: Writable<M>>(&mut self, v: T) {
            v.write_to(&mut self.writer, "", "\n");
        }
        /// '\n' separated, end with '\n'
        pub fn nn<M, T: Writable<M>>(&mut self, v: T) {
            v.write_to(&mut self.writer, "\n", "\n");
        }
        /// write with '\n' and flush
        pub fn nf<M, T: Writable<M>>(&mut self, v: T) {
            v.write_to(&mut self.writer, "", "\n");
            self.writer.flush().expect("Failed to flush!");
        }
        /// space sep, end with '\n'
        pub fn sn<M, T: Writable<M>>(&mut self, v: T) {
            v.write_to(&mut self.writer, " ", "\n");
        }
        /// in case the task asks for some wacky sep or end
        pub fn wr<M, T: Writable<M>>(&mut self, v: T, sep: &str, end: &str) {
            v.write_to(&mut self.writer, sep, end);
        }
    }

    macro_rules! ws {
        // e.g. ws!(wr, 10, -50, "wot");
        ($wr:expr, $first:expr, $($val:expr),*) => {
            // write multiple vars, space sep, end with '\n'
            write!($wr.writer, "{}", $first).unwrap();
            ($(write!($wr.writer, " {}", $val).unwrap()),*);
            write!($wr.writer, "\n").unwrap();
        };
    }
    macro_rules! wb {
        ($wr:expr, $($bytes:expr),*) => {
            // write &[u8] consecutively (no sep) and end with '\n'
            ($($wr.writer.write(&$bytes).unwrap()),*);
            write!($wr.writer, "\n").unwrap();
        };
    }
}

#[cfg(debug_assertions)]
fn main() {
    use std::fs::File;
    solve(
        Reader::new(File::open("input.txt").unwrap()),
        Writer::new(std::io::stdout()),
        // Writer::new(File::create("output.txt").unwrap()),
    )
}

#[cfg(not(debug_assertions))]
fn main() {
    let (stdin, stdout) = (std::io::stdin(), std::io::stdout());
    solve(Reader::new(stdin.lock()), Writer::new(stdout.lock()));
}

// const d8: [(i32, i32); 8] = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];

fn solve<R: Read, W: Write>(mut re: Reader<R>, mut wr: Writer<W>) {
    re.ignore();
    wr.y(!rb!(re).tuple_windows().any(|(a, b)| a == b));
}
