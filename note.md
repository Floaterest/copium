# Problems
## Dijkstra
<details open><summary>tl;dr</summary>

- `BinaryHeap` from [doc.rust-lang.org](https://doc.rust-lang.org/std/collections/binary_heap/index.html#examples)
- code from [`abc192_e.rs`](./atcoder.jp/abc192/abc192_e.rs#L181)
</details>


<details><summary>not lazy; wanna read</summary>

### Given
- `graph: Vec<Vec<(usize,usize)>>` adjacency list of `(node, weight)`
- `x: usize` start point
- `y: usize` end point

### Goal
find shortest path from `x` to `y`

### Solution
```rs
// set all distance to MAX
let mut d = vec![!0; graph.len()];
// set dist(x, x) to 0
d[x] = 0;
// create priority queue
let mut pq = BinaryHeap::from([Reverse((0, x))]);
// check the node with the lowest weight first (min-heap)
while let Some(Reverse((w, wu))) = pq.pop() {
    // if reach destination (shortest)
    if u == y { return Some(wu); }
    // if current path is not the shortest
    if wu > d[u] { continue; }
    //          new weight for x -> u -> v
    graph[u].iter().map(|&(v, wv)| (v, wu + wv)).for_each(|(v, wv)| {
        // if new weight is shorter
        if wv < d[v] {
            pq.push(Reverse((wv, v)));
            d[v] = wv;
        }
    });
}
None
```

</details>

## Lower/Upper bound
<details open><summary>tl;dr</summary>

- `binary_search` from [doc.rust-lang.org](https://doc.rust-lang.org/std/primitive.slice.html#method.binary_search)
- code from [`abc248_d.rs`](./atcoder.jp/abc248/abc248_d.rs#L184)
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