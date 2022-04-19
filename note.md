# Algorithms
## Dijkstra

- Example code
  - [abc192_e.rs](./atcoder.jp/abc192/abc192_e.rs)
-  Dijkstra’s algorithm as example of Rust's BinaryHeap on [doc.rust-lang.org](https://doc.rust-lang.org/std/collections/binary_heap/index.html#examples)

# Rust
<details open><summary>lower/upper bound</summary>

## Given
- `a: &[T]`: an **increasing** finite sequence 
- `x: T` such that `min(a) <= x <= max(a)`
- `(lower, upper): (usize, usize)` such that `lower <= upper`

## Goal
find length of `{ x ∈ a : lower <= x <= upper }`

## Solution
```rs
// lower <= x <= upper ==>  a[left] <= x < a[right]
let right = a.binary_search(&(upper + 1)).unwrap_or_else(|i| i);
let left = a.binary_search(&lower).unwrap_or_else(|i| i);
let len: usize =  right - left;
```

</details>
