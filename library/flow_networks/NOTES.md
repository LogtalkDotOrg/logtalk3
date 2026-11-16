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
  (`new/2-3`, `from_graph/2`) and inspection (`vertices/2`, `arcs/2`,
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

- `capacity_scaling(Dictionary)` and `capacity_scaling`: provide the same
  maximum-flow and cut API using capacity-scaling augmenting paths.

- `gomory_hu(Dictionary)` and `gomory_hu`: build all-pairs minimum-cut trees
  for networks with symmetric aggregate capacities.

- `piecewise_costs(Dictionary)` and `piecewise_costs`: expand convex integer
  marginal-cost segments, including lower bounds, into ordinary linear arcs.

- `time_expanded_network(Dictionary)` and `time_expanded_network`: build
  finite discrete-time networks from explicit travel and storage schedules.

The `max_flow_protocol` and `min_cost_flow_protocol` protocols define the
algorithm APIs. The `flow_support` and `max_flow_common` categories contain
the code shared by the algorithm objects.


Usage
-----

Use the `new(Vertices, Arcs, Network)` predicate when isolated vertices matter.
Explicit vertices are combined with arc endpoints, sorted and deduplicated. For
example, `flow_network::new([s,t], [], Network)` has two isolated vertices
and maximum flow zero. The `from_graph/2` predicate also preserves isolated
vertices.

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


Convex piecewise-linear costs
----------------------------

`piecewise_costs::build(Vertices, CostArcs, Network, Mapping)` expands each
`arc(From,To,Lower,Segments)` into ordinary linear-cost arcs. Each
`segment(Width,MarginalCost)` has a positive integer width, and marginal
costs must be nondecreasing integers, including negative values. The upper
bound is the sum of widths and the convex cost at zero is zero. Lower
bounds are assigned to the cheapest prefix of the segments.

	| ?- piecewise_costs::build([s,t],
	         [arc(s,t,3,[segment(2,-2),segment(3,1)])], Network, Mapping),
	     successive_shortest_paths::min_cost_flow(
	         Network, [s-3,t-(-3)], Cost, ExpandedFlow),
	     piecewise_costs::collapse_flow(Network, Mapping, ExpandedFlow, Flow).
	Cost = -3,
	ExpandedFlow = [flow(1,s,t,2),flow(2,s,t,1)],
	Flow = [flow(1,s,t,3)].

Original arc IDs follow cost-arc input order; expanded IDs follow segment
order. Parallel original arcs remain distinct. Keep the opaque mapping
with its expanded network; projection checks its public vertices/arcs
snapshot and validates expanded flow capacities. It sums segment amounts
and omits zero original flows.

The existing minimum-cost solver needs no changes to handle the expanded
problem. An arbitrary feasible expanded flow, however, may use expensive
segments while cheaper capacity remains unused. Projection rejects this
noncanonical allocation because its linear cost would overstate the original
convex cost. Distributions across equal-cost segments are accepted.
Mandatory lower-bound cost is already included in the solver's reported
cost; do not subtract it. The original convex arcs are not themselves a
core flow network, and projected flows should not be checked against the
expanded network's arc IDs.

Construction is linear in total segment count before dictionary costs.
This models convex integer piecewise-linear costs only, not concave costs,
fixed charges, arbitrary nonlinear functions or floating-point slopes.


All-pairs minimum cuts
---------------------

Gomory-Hu trees apply to undirected capacities, represented here by directed
networks whose aggregate capacity from each vertex to another equals the
aggregate capacity in the reverse direction. Parallel-arc multiplicities
need not match. Costs and self-loops do not affect cuts. Asymmetric
capacities and nonzero lower bounds are rejected, not silently transformed.

	| ?- flow_network::new([arc(a,b,3),arc(b,a,3),arc(b,c,2),arc(c,b,2)], Network),
	     gomory_hu::tree(Network, Tree),
	     gomory_hu::min_cut_value(Tree, a, c, Value).
	Tree = tree([a,b,c], [edge(b,a,3),edge(c,b,2)]),
	Value = 2.

The tree uses exactly `n-1` Dinic minimum-cut calls for `n >= 1`. The minimum
edge on a tree path gives the original pairwise cut value; removing a tree
edge gives a partition with that edge's cut capacity in the original network.
Disconnected components are linked by zero-weight edges. Empty and singleton
networks are supported; value queries require distinct existing vertices.
Tree queries validate the tree and build adjacency in O(V*log(V)) with balanced
dictionaries, without an all-pairs table or LCA index.


Time-expanded networks
----------------------

`time_expanded_network::build(Vertices, Horizon, Travels, Storage, Network, Mapping)`
creates every `time(Vertex,T)` for `T` from zero through the horizon,
including isolated copies. Each input
`travel(From,To,Departure,Duration,Lower,Capacity,Cost)` creates one
departure-specific arc. Positive integer durations, non-negative integer
capacities/lower bounds and integer costs can vary between departures. Each
`store(Vertex,Time,Lower,Capacity,Cost)` creates a holdover arc to the next
time step. Omitting an interval means no waiting capacity, not infinity.

	| ?- time_expanded_network::build(
	         [depot,shop], 2,
	         [travel(depot,shop,1,1,0,2,3)],
	         [store(depot,0,0,2,1)], Network, _),
	     successive_shortest_paths::min_cost_flow(
	         Network, [time(depot,0)-2,time(shop,2)-(-2)], Cost, Flow).
	Cost = 8,
	Flow = [flow(1,time(depot,1),time(shop,2),2),
	        flow(2,time(depot,0),time(depot,1),2)].

Travel entries come first in the arc order, followed by storage entries.
The result `mapping(TravelEntries,StorageEntries)` contains
`travel(ArcId,EntryId,From,To,Departure,Arrival)` and
`store(ArcId,EntryId,Vertex,Time,Arrival)` entries, with one-based IDs in
each input schedule. All endpoints must be declared base vertices; invalid
times, zero durations and arrivals beyond the horizon raise errors.

For inventory, specify supplies and demands at exact time copies. For
evacuation by a deadline, add collection arcs from eligible destination
time copies to a fresh terminal, using a justified finite capacity bound,
and call maximum flow. Storage limits a single holding interval, not a
physical vertex's capacity shared across all times. Nonzero lower bounds
require supply-based minimum-cost flow; maximum-flow APIs still reject them.

Explicit schedules support time-dependent travel and costs without callbacks.
Constant schedules can be generated as input lists. Zero-duration travel,
continuous time and automatic horizon selection are not supported. Network
size is O(V*(H+1) + number of travel entries + number of storage entries).


Application reductions
----------------------

The `flow_networks_modeling` example demonstrates maximum-weight
closure/project selection, binary image segmentation and baseball elimination,
including decoded solutions, finite-capacity bounds and elimination
certificates. They are executable examples, not application-specific library
APIs.


Algorithms
----------

- Edmonds-Karp: O(V*E^2).
- Dinic: O(V^2*E). Vertices that cannot push the requested amount of flow
  are recorded as dead for the rest of the phase, replacing the current-arc
  pointers of imperative implementations.
- Capacity scaling: O(E^2*log(U)) for maximum capacity U, before dictionary
  costs. Starts with the largest power of two no greater than U and halves
  the residual-capacity threshold down to one. Large capacities alone do
  not imply a speedup over Dinic, whose bound is independent of U. Small
  sparse, dense and bipartite comparisons on GNU Prolog favored Dinic,
  increasingly so for larger capacity ranges. Use scaling as an explicit
  alternative, not a default performance optimization. Min-cost scaling
  requires a separate algorithm and is not provided.
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
