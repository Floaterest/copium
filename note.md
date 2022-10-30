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

given

- digraph $G=(V,E)$ where $V=\lbrace i\in\mathbb N:i\lt n\rbrace$ for some $n$ as the size of the graph
- function $w:V\times V\to\mathbb N$ where $w(u,v)$ is the weight from $u$ to $v$

let `graph` be a weighted adjacency list of $G$, `graph[u]` is `Vec<(v, w)>` where $\texttt{w}=w(u,v)$

```rs
fn dijkstra(graph: &Vec<Vec<(usize, usize)>>, start: usize, end: usize) -> Option<usize> {
    //! find shortest path from `start` to `end`

    // set all weights (from start) to MAX
    let mut w: Vec<usize> = vec![!0; graph.len()];
    // w(start, start) is 0
    w[start] = 0;
    let mut heap = BinaryHeap::from([Reverse((0, start))]);
    // check the node with the lowest weight first (min-heap)
    while let Some(Reverse((wu, u))) = heap.pop() {
        // reach destination (shortest)
        if u == end { return Some(wu); }
        // current path is not the shortest
        if wu > w[u] { continue; }
        // w(start, v) = w(start, u) + w(u, v)
        graph[u].iter().map(|&(v, uv)| (v, wu + uv)).for_each(|(v, wv)| {
            // if new weight is less
            if wv < w[v] {
                heap.push(Reverse((wv, v)));
                w[v] = wv;
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