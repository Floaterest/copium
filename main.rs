#![allow(unused_macros, unused_variables, unused_mut, dead_code)]

use std::io::{Read, Write};

use reader::Reader;
use writer::Writer;

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
                    Ok(token) => token,
                    Err(..) => panic!("Cannot parse {token} as {}", type_name::<T>()),
                },
                None => panic!("Token is empty while trying to read {}", type_name::<T>()),
            }
        }

        pub fn i(&mut self) -> isize {
            self.next::<isize>()
        }
        pub fn u(&mut self) -> usize {
            self.next::<usize>()
        }
        pub fn u1(&mut self) -> usize {
            self.u().checked_sub(1).expect("Attempted read 0 as usize1")
        }
        pub fn c(&mut self) -> char {
            self.next::<char>()
        }
        pub fn s(&mut self) -> String {
            self.next::<String>()
        }
        pub fn f(&mut self) -> f64 {
            self.next::<f64>()
        }
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
        /// write "YES\n" or "NO\n" given boolean
        pub fn y(&mut self, b: bool) {
            self.writer.write_all(if b { "Yes\n" } else { "No\n" }.as_bytes()).unwrap();
        }
        /// no sep, end with '\n'
        pub fn w<M, T: Writable<M>>(&mut self, val: T) {
            val.write_to(&mut self.writer, "", "");
        }
        /// no sep, end with '\n'
        pub fn n<M, T: Writable<M>>(&mut self, val: T) {
            val.write_to(&mut self.writer, "", "\n");
        }
        /// '\n' separated, end with '\n'
        pub fn nn<M, T: Writable<M>>(&mut self, val: T) {
            val.write_to(&mut self.writer, "\n", "\n");
        }
        /// write with '\n' and flush
        pub fn nf<M, T: Writable<M>>(&mut self, val: T) {
            val.write_to(&mut self.writer, "", "\n");
            self.writer.flush().expect("Failed to flush!");
        }
        /// space sep, end with '\n'
        pub fn sn<M, T: Writable<M>>(&mut self, val: T) {
            val.write_to(&mut self.writer, " ", "\n");
        }
        /// in case the task asks for some wacky sep or end
        pub fn wr<M, T: Writable<M>>(&mut self, val: T, sep: &str, end: &str) {
            val.write_to(&mut self.writer, sep, end);
        }
    }

    macro_rules! wsn {
        // e.g. wsn!(wr, 10, -50, "wot");
        ($wr:expr, $first:expr, $($val:expr),*) => {
            // write multiple vars, space sep, end with '\n'
            write!($wr.writer, "{}", $first).unwrap();
            ($(write!($wr.writer, " {}", $val).unwrap()),*);
            write!($wr.writer, "\n").unwrap();
        };
    }
    macro_rules! wbn {
        ($wr:expr, $($bytes:expr),*) => {
            // write &[u8] consecutively (no sep) and end with '\n'
            ($($wr.writer.write(&$bytes).unwrap()),*);
            write!($wr.writer, "\n").unwrap();
        };
    }
}

// const d8: [(i32, i32); 8] = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];

fn solve<R: Read, W: Write>(mut re: Reader<R>, mut wr: Writer<W>) {
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
