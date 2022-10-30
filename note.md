<!--
## DP
- [`abc211_c.rs`](./atcoder.jp/abc211/abc211_c.rs)
-->

# Natural Number

## Ceil Integer Division

given $p\in\mathbb N$ and $q\in\mathbb Z^+$

```rs
⌈p/q⌉ = (p + q - 1) / q
```

# Graph Theory

## Dijkstra
> use `BinaryHeap` from [doc.rust-lang.org](https://doc.rust-lang.org/std/collections/binary_heap/index.html#examples)

`graph` is adjacency list of `(node, weight)`

```rs
fn dijkstra(graph: Vec<Vec<(usize, usize)>>, start: usize, end: usize) -> Option<usize> {
    //! find shortest distance from `start` to `end`
    // set all distance (from start) to MAX
    let mut dist: Vec<usize> = vec![!0; graph.len()];
    // dist(start, start) is 0
    dist[start] = 0;
    let mut heap = BinaryHeap::from([Reverse((0, start))]);
    // check the node with the lowest dist first (min-heap)
    while let Some(Reverse((du, u))) = heap.pop() {
        // reach destination (shortest)
        if u == end { return Some(du); }
        // current path is not the shortest
        if du > dist[u] { continue; }
        // dist(start, v) = dist(start, u) + dist(u, v)
        graph[u].iter().map(|&(v, uv)| (v, du + uv)).for_each(|(v, dv)| {
            // if new weight is shorter
            if dv < dist[v] {
                heap.push(Reverse((dv, v)));
                dist[v] = dv;
            }
        });
    }
    None
}
```

examples
  - [`abc192_e.rs`](./atcoder.jp/abc192/abc192_e.rs#L181)


## Lower/Upper bound

- `binary_search` from [doc.rust-lang.org](https://doc.rust-lang.org/std/primitive.slice.html#method.binary_search)
- code from [`abc248_d.rs`](./atcoder.jp/abc248/abc248_d.rs#L184)

<details><summary>Explanation</summary>

### Given
- `a: &[T]` an **increasing** finite sequence
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