// https://leetcode.com/problems/longest-increasing-subsequence/
struct Solution;

impl Solution {
    pub fn length_of_lis(ns: Vec<i32>) -> i32 {
        let mut arr = Vec::with_capacity(ns.len());
        for n in &ns {
            match arr.binary_search(n) {
                Ok(_) => {}
                Err(i) => {
                    if i == arr.len() {
                        arr.push(*n);
                    } else {
                        arr[i] = *n;
                    }
                }
            }
        }
        arr.len() as i32
    }
}

fn main() {
    assert_eq!(Solution::length_of_lis(vec![10, 9, 2, 5, 3, 7, 101, 18]), 4);
    assert_eq!(Solution::length_of_lis(vec![0, 1, 0, 3, 2, 3]), 4);
    assert_eq!(Solution::length_of_lis(vec![7, 7, 7, 7, 7, 7, 7]), 1);
}
