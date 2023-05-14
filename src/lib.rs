#![allow(unknown_lints, special_module_name)]

mod main;

#[cfg(test)]
mod read_tests {
    use super::*;
    use main::reader::Reader;

    #[test]
    fn read_atom() {
        let mut re = Reader::new("20 1 -10 2.5 c str".as_bytes());
        let (u, u1, i, f, c, s) = (re.u(), re.u1(), re.i(), re.f(), re.c(), re.s());
        assert_eq!(u, 20);
        assert_eq!(u1, 0);
        assert_eq!(i, -10);
        assert_eq!(f, 2.5);
        assert_eq!(c, 'c');
        assert_eq!(s, "str");
    }
    #[test]
    fn read_macro() {
        let mut re = Reader::new("20 1 -10 2.5 c str".as_bytes());
        let (u, u1, i, f, c, s) = r!(re, u, u1, i, f, c, s);
        assert_eq!(u, 20);
        assert_eq!(u1, 0);
        assert_eq!(i, -10);
        assert_eq!(f, 2.5);
        assert_eq!(c, 'c');
        assert_eq!(s, "str");
    }
    #[test]
    fn read_iter() {
        let mut re = Reader::new("1 2 3 4 5".as_bytes());
        let v: Vec<_> = r!(re, [u; 5]).collect();
        assert_eq!(v, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn read_vec() {
        let mut re = Reader::new("1 -2 3 -4 5 -6".as_bytes());
        assert_eq!(rv!(re, [i; 6]), [1, -2, 3, -4, 5, -6]);
    }
}
