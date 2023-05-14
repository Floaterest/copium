#![allow(unknown_lints, special_module_name)]

mod main;

#[cfg(test)]
mod read_tests {
    use super::*;
    use main::reader::Reader;
    use std::collections::{BinaryHeap, HashSet, VecDeque};

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
        let mut re = Reader::new("-1 -2 -3 -4 -5 -6".as_bytes());
        assert_eq!(rv!(re, [i; 6]), [-1, -2, -3, -4, -5, -6]);
    }

    #[test]
    fn read_vecs() {
        let bytes = "-1 -2 -3 -4 -5 -6".as_bytes();
        let mut re = Reader::new(bytes);
        assert_eq!(rv!(re, [[i; 3]; 2]), [[-1, -2, -3], [-4, -5, -6]]);
        // let mut re = Reader::new(bytes);
        // assert_eq!(rv!(re, [i; 3], [i; 3]), ([1, -2, 3], [-4, 5, -6]));
    }

    #[test]
    fn read_hash_set() {
        let mut re = Reader::new("1 2 3 2 1".as_bytes());
        assert_eq!(rs!(re, [u; 5]), (1..=3).collect());
    }

    #[test]
    fn read_vec_deque() {
        let mut re = Reader::new("a b c d e f".as_bytes());
        assert_eq!(rd!(re, [c; 6]), ['a', 'b', 'c', 'd', 'e', 'f']);
    }

    #[test]
    fn read_binary_heap() {
        let mut re = Reader::new("6 -5 4 -3 2 -1".as_bytes());
        let mut heap = rh!(re, [i; 6]);
        let mut expected = vec![-5, -3, -1, 2, 4, 6];
        while !heap.is_empty() {
            assert_eq!(heap.pop(), expected.pop());
        }
    }
}
