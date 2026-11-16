________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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
________________________________________________________________________


`flow_networks`
===============

This library implements network flow algorithms over capacitated directed
graphs (flow networks): maximum flow, minimum cut, and minimum cost flow.

A flow network is built from a list of `arc(From, To, Capacity)`,
`arc(From, To, Capacity, Cost)`, or
`arc(From, To, Lower, Capacity, Cost)` terms. Vertices can be any ground
term, lower bounds and capacities must be non-negative integers with the
lower bound no greater than the capacity, and costs must be integers (the
default cost and lower bound are zero). Parallel, antiparallel, and self-loop
arcs are allowed. Self-loops do not affect maximum flow, but negative-cost
self-loops are saturated by minimum-cost flow algorithms.

Maximum-flow solver terminals and node-capacity vertices must also be ground.
Partially instantiated vertices raise an `instantiation_error`.

The network is an opaque term. Internally, each arc is stored as a residual
pair (a forward arc and a reverse arc with the negated cost), which avoids
the need for special handling of parallel or antiparallel arcs. The objects
are parametric on the dictionary object used for the representation (e.g.,
`avltree`, `rbtree`, `bintree`, or `splaytree`, as in the `graphs` library).
The same dictionary object must be used to construct a network and to run
the algorithms on it. Non-parametric objects using `avltree` are provided.


API documentation
-----------------

Open the [../../apis/library_index.html#flow_networks](../../apis/library_index.html#flow_networks)
file in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(flow_networks(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(flow_networks(tester)).

The randomized tests use `fast_random(as183)` with deterministic seeds,
restoring the generator state after each randomized test.


API overview
------------

- `flow_network(Dictionary)` and `flow_network`: provide construction
  (`new/2`, `from_graph/2`) and inspection (`vertices/2`, `arcs/2`,
  `decompose_flow/3`, `flow_cost/3`, `flow_balances/3`, `cut_capacity/3`)
  predicates, plus flow verification and `valid_cut/5`.

- `edmonds_karp(Dictionary)` and `edmonds_karp`: provide predicates for
  maximum flow and minimum cut using shortest augmenting paths (simple
  reference implementation), with multiple-source/sink, node-capacity,
  and minimum-cut variants.

- `dinic(Dictionary)` and `dinic`: provide predicates for maximum flow
  and minimum cut using level graphs and blocking flows (faster), with
  multiple-source/sink and node-capacity and minimum-cut variants.

- `successive_shortest_paths(Dictionary)` and `successive_shortest_paths`:
  provide predicates for minimum cost flow with supplies and demands and
  minimum cost maximum flow. Negative arc costs, including negative cost
  cycles, are supported.

The `max_flow_protocol` and `min_cost_flow_protocol` protocols define the
algorithm APIs. The `flow_support` and `max_flow_common` categories contain
the code shared by the algorithm objects.


Usage
-----

Maximum flow and minimum cut:

	| ?- flow_network::new([arc(s,a,10), arc(s,b,10), arc(a,b,2), arc(a,t,8), arc(b,t,10)], Network),
	     dinic::max_flow(Network, s, t, Value, Flow).
	Value = 18,
	Flow = [flow(1,s,a,8), flow(2,s,b,10), flow(4,a,t,8), flow(5,b,t,10)].

	| ?- flow_network::new([arc(s,a,10), arc(s,b,10), arc(a,b,2), arc(a,t,8), arc(b,t,10)], Network),
	     dinic::min_cut(Network, s, t, Value, Cut).
	Value = 18,
	Cut = cut([a,b,s], [t], [arc(a,t,8), arc(b,t,10)]).

Minimum cost flow with supplies (positive) and demands (negative):

	| ?- flow_network::new([arc(s,a,4,2), arc(s,b,2,2), arc(a,b,2,1), arc(a,t,3,3), arc(b,t,5,1)], Network),
	     successive_shortest_paths::min_cost_flow(Network, [s-3, t-(-3)], Cost, Flow).
	Cost = 10,
	Flow = [flow(1,s,a,1), flow(2,s,b,2), flow(3,a,b,1), flow(5,b,t,3)].

The `min_cost_flow/4` predicate fails if there is no feasible flow and
supports lower-bounded arcs and circulation feasibility. The
`min_cost_max_flow/6` predicate computes a minimum cost flow among the
maximum flows from a source to a sink. Maximum-flow and minimum-cut
predicates, including `min_cost_max_flow/6`, currently require zero lower
bounds and throw a `domain_error(zero_lower_bounds, Network)` otherwise.
Flows are returned as lists of
`flow(ArcId, From, To, Amount)` terms for the arcs carrying flow, in the order
of the network arcs, including self-loops carrying flow. `ArcId` is the
one-based position of the arc in the input list and distinguishes parallel
arcs.
The `min_cost_flow/5` and `min_cost_max_flow/7` variants also return final
vertex potentials as `Vertex-Potential` pairs.

For example, the following lower-bounded arc requires at least two units of
flow, while the supplies request three:

	| ?- flow_network::new([arc(s,t,2,5,1)], Network),
	     successive_shortest_paths::min_cost_flow(Network, [s-3, t-(-3)], Cost, Flow).
	Cost = 3,
	Flow = [flow(1,s,t,3)].

Multiple sources and sinks are supported directly; returned flows still refer
to the original network arcs:

	| ?- flow_network::new([arc(s1,a,2), arc(s2,a,3), arc(a,t,5)], Network),
	     dinic::multi_source_max_flow(Network, [s1,s2], [t], Value, Flow).
	Value = 5,
	Flow = [flow(1,s1,a,2), flow(2,s2,a,3), flow(3,a,t,5)].

Intermediate vertex capacities can be specified as `Vertex-Capacity` pairs.
Vertices without an explicit capacity are effectively unbounded, and source
and sink capacities are not constrained:

	| ?- flow_network::new([arc(s,a,5), arc(s,b,5), arc(a,t,5), arc(b,t,5)], Network),
	     dinic::node_capacity_max_flow(Network, s, t, [a-1, b-2], Value, Flow).
	Value = 3.

The regular `min_cut/5` returns the source-closest minimum cut.
`sink_closest_min_cut/5` returns the other extremal cut, `unique_min_cut/4`
reports whether the cut is unique, and `min_cuts/5` lazily enumerates all
minimum cuts on backtracking. The latter uses residual strongly connected
components to enumerate valid vertex partitions without materializing all
cuts in advance.

A feasible flow can be decomposed into weighted paths and cycles. Path
endpoints are inferred from the flow balances, and arc IDs preserve the
identity of parallel arcs:

	| ?- flow_network::(
             new([arc(s,a,2), arc(a,t,2), arc(a,b,1), arc(b,a,1)], Network),
	         decompose_flow(Network, [flow(1,s,a,2), flow(2,a,t,2), flow(3,a,b,1), flow(4,b,a,1)], Parts)
         ).
	Parts = [path([s,a,t],[1,2],2), cycle([a,b,a],[3,4],1)].

Use `valid_flow/3` to check per-arc capacities and supply/demand balances,
or `valid_st_flow/5` for a source-to-sink flow. `valid_min_cost_flow/4`
additionally checks the reported cost and independently verifies that the
residual graph has no negative-cost cycle. Its `valid_min_cost_flow/5`
variant checks a supplied potential certificate instead.

Use `flow_cost/3` and `flow_balances/3` to inspect a flow independently of
supplies or optimality. Both require unique valid arc IDs, matching ground
endpoints, positive integer amounts, and satisfaction of every lower and
upper arc bound. Omitted arcs carry zero flow and must have zero lower
bounds. Flow entries may be in any order. Invalid inputs raise errors.
Balances are sorted `Vertex-NetOutflow` pairs for every network vertex,
including zero balances. A self-loop contributes to cost, but not net outflow:

	| ?- flow_network::(
	         new([arc(s,t,3,2), arc(s,s,2,-1)], Network),
	         flow_cost(Network, [flow(1,s,t,2), flow(2,s,s,1)], Cost),
	         flow_balances(Network, [flow(1,s,t,2), flow(2,s,s,1)], Balances)
	     ).
	Cost = 3,
	Balances = [s-2,t-(-2)].

Use `cut_capacity/3` to compute the capacity of arcs leaving a vertex set.
It accepts vertices in any order, including empty or full sets, but rejects
duplicates and unknown vertices. `valid_cut/5` verifies a ground cut term,
including a complete disjoint partition, terminal membership, crossing arcs,
and reported capacity. Vertex lists must be sorted and duplicate-free, and
positive-capacity crossing arcs must appear in network input order.
Parallel arcs count individually; self-loops and zero-capacity arcs are
excluded. A valid cut need not be a minimum cut:

	| ?- flow_network::(
	         new([arc(s,a,3), arc(a,t,1)], Network),
	         cut_capacity(Network, [s], Capacity),
	         valid_cut(Network, s, t, Capacity, cut([s], [a,t], [arc(s,a,3)]))
	     ).
	Capacity = 3.

Both cut predicates require zero lower bounds. `cut_capacity/3` throws
`domain_error(zero_lower_bounds, Network)` otherwise; `valid_cut/5` fails
for that case, as for other invalid candidates.

The assignment problem can be solved as a minimum cost flow: use one vertex
per worker and per job, unit capacity arcs from a source to the workers and
from the jobs to a sink, and one arc per worker-job pair with the assignment
cost.


Algorithms
----------

- Edmonds-Karp: O(V*E^2).
- Dinic: O(V^2*E). Vertices that cannot push the requested amount of flow
  are recorded as dead for the rest of the phase, replacing the current-arc
  pointers of imperative implementations.
- Successive shortest paths: Dijkstra with node potentials (reduced costs)
  and a binary heap. Arcs with negative cost are saturated beforehand so
  that all residual costs are non-negative; this also handles negative
  cost cycles. A super source and a super sink are used to route supplies
  and demands.


Limitations
-----------

- Integer capacities and costs only (avoids floating-point residual drift).
- No infinite capacities; use a large integer.
- Persistent dictionaries add a logarithmic factor and a larger constant
  when compared with array-based implementations.
- The vertices `'$flow_super_source'` and `'$flow_super_sink'` are reserved
  for `min_cost_flow/4` and `min_cost_max_flow/6`.
