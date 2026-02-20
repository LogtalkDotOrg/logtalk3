%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(weighted_undirected_graph(_Dictionary_),
	imports((weighted_graph_common(_Dictionary_), undirected_graph_common))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Weighted undirected graph predicates using a dictionary representation. Each edge is stored in both directions. Edge weights use a pair representation (``Vertex-Weight`` in neighbor lists, ``(Vertex1-Vertex2)-Weight`` for edge lists). The parametric object parameter is the dictionary to use for the graph representation.',
		parnames is ['Dictionary']
	]).

	% Additional public predicates specific to weighted undirected graphs

	:- public(degree/3).
	:- mode(degree(+vertex, +graph, -integer), zero_or_one).
	:- info(degree/3, [
		comment is 'Returns the degree (number of edges incident to the vertex) of ``Vertex`` in ``Graph``.',
		argnames is ['Vertex', 'Graph', 'Degree']
	]).

	:- public(is_connected/1).
	:- mode(is_connected(+graph), zero_or_one).
	:- info(is_connected/1, [
		comment is 'True if ``Graph`` is connected (all vertices are reachable from any vertex).',
		argnames is ['Graph']
	]).

	:- public(connected_components/2).
	:- mode(connected_components(+graph, -list(list)), one).
	:- info(connected_components/2, [
		comment is 'Returns the list of connected components (each a list of vertices).',
		argnames is ['Graph', 'Components']
	]).

	:- public(min_tree/3).
	:- mode(min_tree(+graph, -graph, -number), zero_or_one).
	:- info(min_tree/3, [
		comment is 'Constructs a minimum spanning tree and returns its total weight.',
		argnames is ['Graph', 'Tree', 'Cost']
	]).

	:- public(max_tree/3).
	:- mode(max_tree(+graph, -graph, -number), zero_or_one).
	:- info(max_tree/3, [
		comment is 'Constructs a maximum spanning tree and returns its total weight.',
		argnames is ['Graph', 'Tree', 'Cost']
	]).

	% Dictionary operations

	:- uses(_Dictionary_, [
		new/1 as dict_new/1,
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		keys/2 as dict_keys/2,
		as_list/2 as dict_as_list/2
	]).

	:- uses(list, [
		length/2, member/2, msort/2, subtract/3
	]).

	% === Edges (undirected: deduplicate) ===

	edges(Graph, Edges) :-
		dict_as_list(Graph, Pairs),
		^^wpairs_to_edges(Pairs, AllEdges),
		canonical_wedges(AllEdges, Edges).

	% === Edge operations (undirected: edges stored in both directions) ===

	add_edge(Graph, Vertex1, Vertex2, Weight, NewGraph) :-
		% Ensure Vertex1 is in graph
		(	dict_lookup(Vertex1, WNs1, Graph) ->
			^^winsert_neighbor(WNs1, Vertex2, Weight, NewWNs1)
		;	NewWNs1 = [Vertex2-Weight]
		),
		dict_insert(Graph, Vertex1, NewWNs1, NewGraph0),
		% Also add reverse direction
		(	dict_lookup(Vertex2, WNs2, NewGraph0) ->
			^^winsert_neighbor(WNs2, Vertex1, Weight, NewWNs2)
		;	NewWNs2 = [Vertex1-Weight]
		),
		dict_insert(NewGraph0, Vertex2, NewWNs2, NewGraph).

	delete_edge(Graph, Vertex1, Vertex2, Weight, NewGraph) :-
		(	dict_lookup(Vertex1, WNs1, Graph),
			^^wremove_neighbor(WNs1, Vertex2, Weight, NewWNs1) ->
			dict_insert(Graph, Vertex1, NewWNs1, NewGraph0),
			(	dict_lookup(Vertex2, WNs2, NewGraph0),
				^^wremove_neighbor(WNs2, Vertex1, _, NewWNs2) ->
				dict_insert(NewGraph0, Vertex2, NewWNs2, NewGraph)
			;	NewGraph = NewGraph0
			)
		;	NewGraph = Graph
		).

	% === Degree ===

	degree(Vertex, Graph, Degree) :-
		dict_lookup(Vertex, WNeighbors, Graph),
		length(WNeighbors, Degree).

	% === Connectivity ===

	is_connected(Graph) :-
		dict_keys(Graph, Vertices),
		Vertices = [Vertex| _],
		^^reachable(Vertex, Graph, Reachable),
		length(Vertices, NV),
		length(Reachable, NR),
		NV =:= NR.

	connected_components(Graph, Components) :-
		dict_keys(Graph, Vertices),
		connected_components_loop(Vertices, Graph, [], Components).

	% === Min Path (Dijkstra) — from weighted_graph_common ===
	% === Max Path (DFS) — from weighted_graph_common ===

	% === Minimum Spanning Tree (Kruskal) ===

	min_tree(Graph, Tree, TotalCost) :-
		edges(Graph, Edges),
		msort_wedges(Edges, SortedEdges),
		dict_keys(Graph, Vertices),
		init_uf(Vertices, UF0),
		kruskal(SortedEdges, UF0, [], TotalCost, 0, TreeEdges),
		^^new(Tree0),
		add_vertices_from_list(Vertices, Tree0, Tree1),
		add_tree_edges(TreeEdges, Tree1, Tree).

	% === Maximum Spanning Tree (Kruskal with reversed sort) ===

	max_tree(Graph, Tree, TotalCost) :-
		edges(Graph, Edges),
		msort_wedges_desc(Edges, SortedEdges),
		dict_keys(Graph, Vertices),
		init_uf(Vertices, UF0),
		kruskal(SortedEdges, UF0, [], TotalCost, 0, TreeEdges),
		^^new(Tree0),
		add_vertices_from_list(Vertices, Tree0, Tree1),
		add_tree_edges(TreeEdges, Tree1, Tree).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% Remove duplicate undirected edges: keep only (V1-V2)-W where V1 @< V2
	canonical_wedges([], []).
	canonical_wedges([(Vertex1-Vertex2)-Weight| Edges], Result) :-
		(	Vertex1 @< Vertex2 ->
			Result = [(Vertex1-Vertex2)-Weight| Rest],
			canonical_wedges(Edges, Rest)
		;	canonical_wedges(Edges, Result)
		).

	% --- Connected components ---

	connected_components_loop([], _, Components, Components).
	connected_components_loop([Vertex| Vertices], Graph, Components0, Components) :-
		(	vertex_in_component(Vertex, Components0) ->
			connected_components_loop(Vertices, Graph, Components0, Components)
		;	^^reachable(Vertex, Graph, Reachable),
			subtract(Vertices, Reachable, Remaining),
			connected_components_loop(Remaining, Graph, [Reachable| Components0], Components)
		).

	vertex_in_component(Vertex, [Component| Components]) :-
		(	member(Vertex, Component) ->
			true
		;	vertex_in_component(Vertex, Components)
		).

	% --- Kruskal's algorithm ---

	% Sort edges by weight (ascending for min, descending for max)
	msort_wedges(Edges, Sorted) :-
		wattach_weights(Edges, WeightedEdges),
		msort(WeightedEdges, SortedWeighted),
		wdetach_weights(SortedWeighted, Sorted).

	msort_wedges_desc(Edges, Sorted) :-
		wattach_neg_weights(Edges, WeightedEdges),
		msort(WeightedEdges, SortedWeighted),
		wdetach_neg_weights(SortedWeighted, Sorted).

	wattach_weights([], []).
	wattach_weights([(Vertex1-Vertex2)-Weight| Edges], [Weight-((Vertex1-Vertex2)-Weight)| Weighted]) :-
		wattach_weights(Edges, Weighted).

	wdetach_weights([], []).
	wdetach_weights([_-Edge| Weighted], [Edge| Edges]) :-
		wdetach_weights(Weighted, Edges).

	wattach_neg_weights([], []).
	wattach_neg_weights([(Vertex1-Vertex2)-Weight| Edges], [NegWeight-((Vertex1-Vertex2)-Weight)| Weighted]) :-
		NegWeight is -Weight,
		wattach_neg_weights(Edges, Weighted).

	wdetach_neg_weights([], []).
	wdetach_neg_weights([_-Edge| Weighted], [Edge| Edges]) :-
		wdetach_neg_weights(Weighted, Edges).

	kruskal([], _, TreeEdges, Cost, Cost, TreeEdges).
	kruskal([(Vertex1-Vertex2)-Weight| Edges], UF0, Acc, TotalCost, CostAcc, TreeEdges) :-
		find(Vertex1, UF0, Root1, UF1),
		find(Vertex2, UF1, Root2, UF2),
		(	Root1 == Root2 ->
			kruskal(Edges, UF2, Acc, TotalCost, CostAcc, TreeEdges)
		;	union(Root1, Root2, UF2, UF3),
			NewCostAcc is CostAcc + Weight,
			kruskal(Edges, UF3, [(Vertex1-Vertex2)-Weight| Acc], TotalCost, NewCostAcc, TreeEdges)
		).

	% --- Union-Find data structure ---

	init_uf(Vertices, UF) :-
		dict_new(UF0),
		uf_init_vertices(Vertices, UF0, UF).

	uf_init_vertices([], UF, UF).
	uf_init_vertices([Vertex| Vertices], UF0, UF) :-
		dict_insert(UF0, Vertex, Vertex-0, UF1),
		uf_init_vertices(Vertices, UF1, UF).

	find(Vertex, UF0, Root, UF) :-
		dict_lookup(Vertex, Parent-Rank, UF0),
		(	Parent == Vertex ->
			Root = Vertex,
			UF = UF0
		;	find(Parent, UF0, Root, UF1),
			dict_insert(UF1, Vertex, Root-Rank, UF)
		).

	union(Root1, Root2, UF0, UF) :-
		dict_lookup(Root1, Root1-Rank1, UF0),
		dict_lookup(Root2, Root2-Rank2, UF0),
		(	Rank1 < Rank2 ->
			dict_insert(UF0, Root1, Root2-Rank1, UF)
		;	Rank1 > Rank2 ->
			dict_insert(UF0, Root2, Root1-Rank2, UF)
		;	NewRank is Rank1 + 1,
			dict_insert(UF0, Root2, Root1-Rank2, UF1),
			dict_insert(UF1, Root1, Root1-NewRank, UF)
		).

	% --- Build tree from edges ---

	add_vertices_from_list([], Tree, Tree).
	add_vertices_from_list([Vertex| Vertices], Tree0, Tree) :-
		^^add_vertex(Tree0, Vertex, Tree1),
		add_vertices_from_list(Vertices, Tree1, Tree).

	add_tree_edges([], Tree, Tree).
	add_tree_edges([(Vertex1-Vertex2)-Weight| Edges], Tree0, Tree) :-
		add_edge(Tree0, Vertex1, Vertex2, Weight, Tree1),
		add_tree_edges(Edges, Tree1, Tree).

:- end_object.


:- object(weighted_undirected_graph,
	extends(weighted_undirected_graph(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Weighted undirected graph predicates using the AVL tree dictionary representation. Each edge is stored in both directions. Edge weights use a pair representation (Vertex-Weight in neighbor lists, (V1-V2)-Weight for edge lists).'
	]).

:- end_object.
