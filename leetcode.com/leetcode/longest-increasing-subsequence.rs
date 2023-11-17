// https://leetcode.com/problems/longest-increasing-subsequence/
struct Solution;
impl Solution {
    pub fn length_of_lis(ns: Vec<i32>) -> i32 {
        let mut dp = vec![1; ns.len()];
        for k in 0..ns.len() {
            dp[k] = 1 + dp
                .iter()
                .take(k)
                .enumerate()
                .filter(|&(i, _)| ns[i] < ns[k])
                .max_by_key(|&(_, n)| n)
                .unwrap_or((0, &0))
                .1;
            dbg!(&dp);
        }
        *dp.iter().max().unwrap_or(&0)
    }
}
fn main() {
    assert_eq!(Solution::length_of_lis(vec![10, 9, 2, 5, 3, 7, 101, 18]), 4);
    assert_eq!(Solution::length_of_lis(vec![0, 1, 0, 3, 2, 3]), 4);
    assert_eq!(Solution::length_of_lis(vec![7, 7, 7, 7, 7, 7, 7]), 1);
}
