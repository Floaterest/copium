// https://atcoder.jp/contests/abc244/tasks/abc244_a
#![allow(unused_macros, dead_code, unused_mut, unused_variables, non_snake_case, non_upper_case_globals)]

use std::io::{Read, Write};

use cp::{reader::Reader, writer::Writer};

#[macro_use]
pub mod cp {
    #[macro_use]
    pub mod reader {
        use std::io::{BufRead, BufReader, Read};
        use std::iter::Peekable;
        use std::mem::transmute;
        use std::str::{FromStr, SplitWhitespace};

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
                    if n == 0 { return; /* EOF */ }

                    self.line = line.into_boxed_str();
                    self.tokens = unsafe {
                        transmute::<_, &'static str>(&*self.line)
                    }.split_whitespace().peekable();
                }
            }

            pub fn token<T: FromStr>(&mut self) -> T {
                self.prepare();
                self.tokens.next().unwrap().parse().ok().unwrap()
            }

            pub fn i(&mut self) -> i32 { self.token::<i32>() }
            pub fn ii(&mut self) -> i64 { self.token::<i64>() }
            pub fn f(&mut self) -> f32 { self.token::<f32>() }
            pub fn us(&mut self) -> usize { self.token::<usize>() }
            pub fn bytes(&mut self) -> Vec<u8> { self.token::<String>().into_bytes() }
        }

        macro_rules! r {
            // read iter, e.g. re!(r, [i32;20]).collect::<HashSet<_>>()
            ($r:expr, [$type:ty; $len:expr]) => ((0..$len).map(|_| $r.token::<$type>()));
            // read tuple, e.g. re!(r, usize, i32, String)
            ($r:expr, $($type:ty),+) => (($($r.token::<$type>()),+));
        }
    }

    #[macro_use]
    pub mod writer {
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
                write!(w, "{}{}", self, end).unwrap();
            }
        }
        //#endregion Writable

        //#region Writer
        #[derive(Debug)]
        pub struct Writer<W: Write> {
            pub writer: BufWriter<W>,
        }

        impl<W: Write> Writer<W> {
            pub fn new(w: W) -> Self {
                Self {
                    writer: BufWriter::new(w),
                }
            }

            pub fn y(&mut self, b: bool) {
                self.writer.write_all((if b { "YES\n" } else { "NO\n" }).as_bytes()).unwrap();
            }

            pub fn w<M, T: Writable<M>>(&mut self, val: T) {
                val.write_to(&mut self.writer, "", "");
            }
            pub fn n<M, T: Writable<M>>(&mut self, val: T) {
                //! no sep, end with '\n'
                val.write_to(&mut self.writer, "", "\n");
            }
            pub fn sn<M, T: Writable<M>>(&mut self, val: T) {
                //! space sep, end with '\n'
                val.write_to(&mut self.writer, " ", "\n");
            }
            pub fn wr<M, T: Writable<M>>(&mut self, val: T, sep: &str, end: &str) {
                val.write_to(&mut self.writer, sep, end);
            }
        }
        //#endregion Writer

        macro_rules! wsn {
            // write multiple vars, space sep, end with '\n'
            ($w:expr, $first:expr, $($val:expr),*) => {
                $w.w($first);
                ($(write!($w.writer, " {}", $val).unwrap()),*);
                $w.writer.write(&[b'\n']).unwrap();
            };
        }
        macro_rules! wbn {
            // write &[u8] consecutively (no sep) and end with '\n'
            ($w:expr, $($bytes:expr),*) => {
                ($($w.writer.write(&$bytes).unwrap()),*);
                $w.writer.write(&[b'\n']).unwrap();
            };
        }
    }
}

//#region constant
const d8: [(i32, i32); 8] = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];
//#endregion constant

fn solve<R: Read, W: Write>(mut re: Reader<R>, mut wr: Writer<W>) {
    re.us();
    wr.n(*re.bytes().last().unwrap() as char);
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
