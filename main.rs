#![allow(unused_macros, dead_code, unused_mut, unused_variables, non_snake_case, non_upper_case_globals)]

use std::io::{Read, Write};

use copium::{reader::Reader, writer::Writer};

#[macro_use]
pub mod copium {
    #[macro_use]
    pub mod reader {
        use std::io::{BufRead, BufReader, Read};
        use std::iter::Peekable;
        use std::mem::transmute;
        use std::str::{FromStr, SplitWhitespace};
        use std::any::type_name;

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
                    if n == 0 { return; /* EOF */ }

                    self.line = line.into_boxed_str();
                    self.tokens = unsafe {
                        transmute::<_, &'static str>(&*self.line)
                    }.split_whitespace().peekable();
                }
            }

            pub fn token<T: FromStr>(&mut self) -> T {
                self.prepare();
                match self.tokens.next() {
                    Some(token) => match token.parse() {
                        Ok(value) => value,
                        Err(..) => panic!("Cannot parse {} as {}", token, type_name::<T>())
                    },
                    None => panic!("Token is empty while trying to read {}", type_name::<T>())
                }
            }

            pub fn i(&mut self) -> i32 { self.token::<i32>() }
            pub fn ii(&mut self) -> i64 { self.token::<i64>() }
            pub fn f(&mut self) -> f32 { self.token::<f32>() }
            pub fn us(&mut self) -> usize { self.token::<usize>() }
            pub fn bytes(&mut self) -> Vec<u8> { self.token::<String>().into_bytes() }
            pub fn str(&mut self) -> String { self.token::<String>() }
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
        pub struct One;

        #[non_exhaustive]
        pub struct Many;

        #[non_exhaustive]
        pub struct Slice;

        impl<T: Display> Writable<One> for T {
            fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str) {
                write!(w, "{}{}", self, end).unwrap();
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

        impl<T: Display> Writable<Slice> for &[T] {
            fn write_to<W: Write>(self, w: &mut W, sep: &str, end: &str) {
                self.iter().write_to(w, sep, end);
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
                Self { writer: BufWriter::new(w) }
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
            pub fn nn<M, T: Writable<M>>(&mut self, val: T) {
                //! no sep, end with '\n'
                val.write_to(&mut self.writer, "\n", "\n");
            }
            pub fn nf<M, T: Writable<M>>(&mut self, val: T) {
                //! write with '\n' and flush
                val.write_to(&mut self.writer, "", "\n");
                self.writer.flush().unwrap();
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
    // read an usize (re.i(), re.f(), etc)
    let n: usize = re.us();
    // read multiple values of different types
    let (i, f) = r!(re, i32, f32);
    // read string as bytes
    let bytes: Vec<u8> = re.bytes();
    // read string
    let text: String = re.str();
    // read n items, collect to Vec
    let vec: Vec<_> = r!(re,[i32;n]).collect();
    // read, map, collect to HashSet
    let set: HashSet<_> = r!(re,[u32;n]).map(|n| n * 2).collect();

    // if true { "YES\n" } else { "NO\n" }
    wr.y(n == vec.len());
    // space separated, end with '\n'
    wsn!(wr, i, f);
    // write each byte as char, no sep, end with '\n'
    wbn!(wr, bytes);
    // no sep, end with '\n'
    wr.n(set.iter());
    // '\n' separated, end with '\n'
    wr.nn(set.iter());
    // no sep, end with '\n', flush output (e.g. for interactive problems)
    wr.nf(i as f32 + f);
    // space separated, end with '\n'
    wr.sn(vec.iter());

    // ご武運を
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
