<!--
## DP
- [`abc211_c.rs`](./atcoder.jp/abc211/abc211_c.rs)
-->

# Algorithm

## Ceil Integer Division

for $p\in\mathbb N$ and $q\in\mathbb Z^+$

```
⌈p/q⌉ = (p + q - 1) / q
```


## Binary Search
> when you are not allowed to use Rust
```rs
fn binary_search<T: Ord>(slice: &[T], target: T) -> Result<usize, usize> {
    // slice.binary_search(&target)
    let (mut left, mut right) = (0, slice.len());
    while left < right {
        let mid = (left + right) / 2;
        if slice[mid] < target {
            left = mid + 1;
        } else if slice[mid] > target {
            right = mid;
        } else {
            return Ok(mid);
        }
    }
    Err(left)
}
```


## Lower/Upper Bound
> use [`binary_search`](https://doc.rust-lang.org/std/primitive.slice.html#method.binary_search) (if `x` is not found, it returns index of smallest item in slice that is bigger than `x`)

given
- $(a_i)_{i=1}^N$ as **increasing** sequence of $\mathbb Z$
- $L,U\in\mathbb Z$ st $1\le L\le U\le N$

find $r-l$ where $\forall l\le j\lt r\quad L\le a_j\le U$
- i.e. size of subsequence bounded by $L$ and $U$


```rs
fn bounded(a: &[i32], lower: i32, upper: i32) -> usize {
    use std::convert::identity;
    // lower <= x <= upper ==>  a[left] <= x < a[right]
    let left = a.binary_search(&lower).unwrap_or_else(identity);
    // +1 because if upper == a[i] for some i,
    // then we want `right` to be i + 1 to satisfy j < r
    let right = a.binary_search(&(upper + 1)).unwrap_or_else(identity);
    right - left
}
```

examples
  - [`abc192_e.rs`](./atcoder.jp/abc192/abc192_e.rs#L181)


## Number Theory

## Graph Theory

### Dijkstra
> use [`BinaryHeap`](https://doc.rust-lang.org/std/collections/binary_heap/index.html#examples)

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
