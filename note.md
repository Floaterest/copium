# Problems
## Dijkstra
<details open><summary>tl;dr</summary>

- `BinaryHeap` from [doc.rust-lang.org](https://doc.rust-lang.org/std/collections/binary_heap/index.html#examples)
- code from [`abc192_e.rs`](./atcoder.jp/abc192/abc192_e.rs)
</details>

## Lower/Upper bound
<details open><summary>tl;dr</summary>

- `binary_search` from [doc.rust-lang.org](https://doc.rust-lang.org/std/primitive.slice.html#method.binary_search)
- code from [`abc248_d.rs`](./atcoder.jp/abc248/abc248_b.rs)
</details>

<details><summary>not lazy; wanna read</summary>

### Given
- `a: &[T]`: an **increasing** finite sequence 
- `x: T` such that `min(a) <= x <= max(a)`
- `(lower, upper): (usize, usize)` such that `lower <= upper`

### Goal
find length of `{ x ∈ a : lower <= x <= upper }`

### Solution
```rs
// lower <= x <= upper ==>  a[left] <= x < a[right]
let right = a.binary_search(&(upper + 1)).unwrap_or_else(|i| i);
let left = a.binary_search(&lower).unwrap_or_else(|i| i);
let len =  right - left;
```

</details>