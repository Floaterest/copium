// https://leetcode.com/problems/non-overlapping-intervals/description/
struct Solution;

impl Solution {
    pub fn erase_overlap_intervals(mut intervals: Vec<Vec<i32>>) -> i32 {
        intervals.sort_by_key(|i| i[1]);
        intervals
            .iter()
            .fold(
                (0, i32::MIN),
                |(n, f), interval| {
                    if interval[0] >= f {
                        (n, interval[1])
                    } else {
                        (n + 1, f)
                    }
                },
            )
            .0
    }
}

fn main() {
    let a = [[1, 100], [11, 22], [1, 11], [2, 12]].into_iter().map(|ns| ns.into()).collect();
    assert_eq!(Solution::erase_overlap_intervals(a), 2);
}
