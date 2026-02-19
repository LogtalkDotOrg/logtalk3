---

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---

# `graphs`

The `graphs` library implements predicates for handling directed/undirected
weighted/unweighted graphs. Graphs are represented using a dictionary where
keys are vertices and values are sorted lists of neighbors (vertex names for
unweighted graphs, `Vertex-Weight` pairs for weighted graphs).

The four graph types are implemented as parametric objects where the parameter
specifies the dictionary implementation to use (e.g., `avltree`, `rbtree`,
`bintree`, or `splaytree`). This allows for easy experimentation (e.g., for
performance analysis) with different dictionary backends. Non-parametric
objects are also provided for each graph type using the `avltree` dictionary
implementation.

The common protocol for all graph types is defined in `graph_protocol`.
There are also `directed_graph_protocol` and `undirected_graph_protocol`
for protocols specific to directed and undirected graphs, respectively.
Additional predicates specific to each graph type (e.g., `transpose/2`,
`topological_sort/2`, `min_path/5`, `min_tree/3`) are declared in the
individual graph objects.

## API documentation

Open the [../../apis/library_index.html#graphs](../../apis/library_index.html#graphs)
link in a web browser.

## Loading

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(graphs(loader)).

## Testing

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(graphs(tester)).

This library provides also a `graph_types` category for type-checking and
arbitrary generation of graph-related terms.

## Usage

### Graph types

- `unweighted_directed_graph(Dictionary)` - Unweighted directed graph
- `unweighted_undirected_graph(Dictionary)` - Unweighted undirected graph
- `weighted_directed_graph(Dictionary)` - Weighted directed graph
- `weighted_undirected_graph(Dictionary)` - Weighted undirected graph

### Creating graphs

Create an empty graph:

    | ?- unweighted_directed_graph(avltree)::new(Graph).

Create a graph from a list of edges:

    | ?- unweighted_directed_graph(avltree)::new([1-2, 2-3, 3-1], Graph).

Create a graph from vertices and edges:

    | ?- unweighted_directed_graph(avltree)::new([1,2,3,4], [1-2, 2-3], Graph).

For weighted graphs, edges use the format `(V1-V2)-Weight`:

    | ?- weighted_directed_graph(avltree)::new([(a-b)-5, (b-c)-3], Graph).

### Querying graphs

    | ?- unweighted_directed_graph(avltree)::new([1-2, 2-3], Graph),
         unweighted_directed_graph(avltree)::vertices(Graph, Vertices),
         unweighted_directed_graph(avltree)::edges(Graph, Edges),
         unweighted_directed_graph(avltree)::neighbors(1, Graph, Neighbors).

### Undirected graphs

Undirected edges are stored internally as two directed edges. The `edges/2`
predicate returns each undirected edge only once (with `V1 @=< V2`). For
undirected graphs, `neighbors/3` excludes self-loops.
