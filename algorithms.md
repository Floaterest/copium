<!-- omit in toc -->
# Table of Contents


# Graph
## Dijkstra
> find the shortest paths between nodes in a graph ([wiki](https://en.wikipedia.org/wiki/Dijkstra's_algorithm))

<ul>
<li>
note

- can't deal with negative weights
</li>

<li>
uses

- `vector<pair<int,int>>[]` to represent graph
- `priority_queue` to find shortest distance
</li>

<li>

visualisation (src: wiki)<br>![](https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif)
</li>

<li>
<details>
<summary>code</summary>

<details>
<summary>input (same graph from the animation above)</summary>

- first line
    - `6` nodes in total
    - start at node `0`
    - end at node `4`
- the rest
    - `u v weight_from_u_to_v`
```
6 0 4

0 1 7
0 2 9
0 5 14
1 3 15
1 2 10
2 3 11
2 5 2
5 4 9
3 4 6
```
</details>

<details>
<summary>output</summary>

- `dist` is distance from start node to all nodes
```
node	dist
0       0
1       7
2       9
3       20
4       20
5       11
```
</details>

```cpp
#include <bits/stdc++.h>

using namespace std;

#define pi pair<int,int>
#define vi vector<int>
#define vpi vector<pi>
#define eb emplace_back
#define mp make_pair

int main(){
    // num of vertices, start node, end node
    int nv, a, b;
    cin >> nv >> a >> b;

    // read graph
    vpi g[nv];
    int u, v, w;
    while(cin >> u >> v >> w){
        g[u].eb(v, w);
        g[v].eb(u, w);
    }

    // set all distances to max
    vi d(nv, INT_MAX);

    // create pq, less distance = more priority
    priority_queue<pi, vpi, greater<>> pq;

    // start at a with dist of 0
    d[a] = 0;
    pq.push(mp(a, d[a]));

    while(not pq.empty()){
        // pop node u from graph
        u = pq.top().first;
        pq.pop();

        // for each adjacent node v from u
        for(pi &p: g[u]){
            v = p.first, w = p.second;
            // if u to v is shorter than the prev record
            if(d[u] + w < d[v]){
                // update distance to v
                d[v] = d[u] + w;
                pq.push(mp(v, d[v]));
            }
        }
    }

    // print all distances from start node
    cout << "node\tdist\n";
    nv = (int)d.size();
    for(int i = 0; i < nv; i++){
        cout << i << "\t\t" << d[i] << '\n';
    }
}
```
</details>
</li>
</ul>
