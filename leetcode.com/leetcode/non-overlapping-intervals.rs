// https://leetcode.com/problems/non-overlapping-intervals/description/
struct Solution;

impl Solution {
    pub fn erase_overlap_intervals(mut intervals: Vec<Vec<i32>>) -> i32 {
        let mut n = 0;
        let mut f = i32::MIN;
        intervals.sort_by_key(|i| i[1]);
        for interval in &intervals {
            if interval[0] >= f {
                n += 1;
                f = interval[1];
            }
        }
        (intervals.len() - n) as i32
    }
}

fn main() {
    let a = [[1, 100], [11, 22], [1, 11], [2, 12]].into_iter().map(|ns| ns.into()).collect();
    assert_eq!(Solution::erase_overlap_intervals(a), 2);
}
