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


:- object(weighted_directed_graph(_Dictionary_),
	imports((weighted_graph_common(_Dictionary_), directed_graph_common))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Weighted directed graph predicates using a dictionary representation. Edge weights use a pair representation (Vertex-Weight in neighbor lists, (Vertex1-Vertex2)-Weight for edge lists). The parametric object parameter is the dictionary to use for the graph representation.',
		parnames is ['Dictionary']
	]).

	% Additional public predicates specific to weighted directed graphs

	:- public(min_paths/3).
	:- mode(min_paths(+vertex, +graph, -graph), zero_or_one).
	:- info(min_paths/3, [
		comment is 'Computes shortest path tree from ``Vertex1`` to all reachable vertices.',
		argnames is ['Vertex1', 'Graph', 'PathTree']
	]).

	% Dictionary operations

	:- uses(_Dictionary_, [
		new/1 as dict_new/1,
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		keys/2 as dict_keys/2,
		as_list/2 as dict_as_list/2,
		as_dictionary/2 as dict_as_dictionary/2
	]).

	:- uses(pairs, [
		keys/2 as strip_weights/2
	]).

	% === Edges ===

	edges(Graph, Edges) :-
		dict_as_list(Graph, Pairs),
		^^wpairs_to_edges(Pairs, Edges).

	% === Edge operations (directed: one direction only) ===

	add_edge(Graph, Vertex1, Vertex2, Weight, NewGraph) :-
		(	dict_lookup(Vertex1, WNeighbors, Graph) ->
			^^winsert_neighbor(WNeighbors, Vertex2, Weight, NewWNeighbors)
		;	NewWNeighbors = [Vertex2-Weight]
		),
		dict_insert(Graph, Vertex1, NewWNeighbors, NewGraph0),
		(	dict_lookup(Vertex2, _, NewGraph0) ->
			NewGraph = NewGraph0
		;	dict_insert(NewGraph0, Vertex2, [], NewGraph)
		).

	delete_edge(Graph, Vertex1, Vertex2, Weight, NewGraph) :-
		(	dict_lookup(Vertex1, WNeighbors, Graph),
			^^wremove_neighbor(WNeighbors, Vertex2, Weight, NewWNeighbors) ->
			dict_insert(Graph, Vertex1, NewWNeighbors, NewGraph)
		;	NewGraph = Graph
		).

	% === Transpose ===

	transpose(Graph, NewGraph) :-
		dict_as_list(Graph, Pairs),
		dict_new(NewGraph0),
		add_empty_vertices_from_pairs(Pairs, NewGraph0, NewGraph1),
		add_transposed_wedges(Pairs, NewGraph1, NewGraph).

	% === Transitive Closure ===

	transitive_closure(Graph, Closure) :-
		dict_keys(Graph, Vertices),
		wwarshall(Vertices, Graph, Closure).

	% === Symmetric Closure ===

	symmetric_closure(Graph, Closure) :-
		edges(Graph, Edges),
		invert_wedges(Edges, InvertedEdges),
		^^add_edges(Graph, InvertedEdges, Closure).

	% === Topological Sort ===

	topological_sort(Graph, Sorted) :-
		% Strip weights for topological ordering
		dict_as_list(Graph, WPairs),
		strip_wpairs(WPairs, UPairs),
		dict_as_dictionary(UPairs, UGraph),
		dict_keys(UGraph, Vertices),
		zeros(Vertices, Counts0),
		count_edges(UPairs, Vertices, Counts0, Counts1),
		select_zeros(Counts1, Vertices, Zeros),
		top_sort_loop(Zeros, Sorted, [], UGraph, Vertices, Counts1).

	% === Min Path (Dijkstra) — from weighted_graph_common ===
	% === Max Path (DFS) — from weighted_graph_common ===

	% === Min Paths (single source shortest paths) ===

	min_paths(Vertex1, Graph, PathTree) :-
		dict_new(Dist0),
		dict_insert(Dist0, Vertex1, 0-Vertex1, Dist1),
		dijkstra_all([0-Vertex1], Graph, Dist1, FinalDist),
		dict_as_list(FinalDist, DistPairs),
		build_path_tree_edges(DistPairs, Vertex1, TreeEdges),
		dict_new(T0),
		add_vertices_from_dist(DistPairs, T0, T1),
		add_tree_edges(TreeEdges, T1, PathTree).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- Transpose ---

	add_empty_vertices_from_pairs([], NewGraph, NewGraph).
	add_empty_vertices_from_pairs([Vertex-_| Pairs], Graph, NewGraph) :-
		(	dict_lookup(Vertex, _, Graph) ->
			add_empty_vertices_from_pairs(Pairs, Graph, NewGraph)
		;	dict_insert(Graph, Vertex, [], NewGraph0),
			add_empty_vertices_from_pairs(Pairs, NewGraph0, NewGraph)
		).

	add_transposed_wedges([], NewGraph, NewGraph).
	add_transposed_wedges([Vertex-WNs| Pairs], Graph, NewGraph) :-
		add_reversed_wneighbors(WNs, Vertex, Graph, NewGraph0),
		add_transposed_wedges(Pairs, NewGraph0, NewGraph).

	add_reversed_wneighbors([], _, NewGraph, NewGraph).
	add_reversed_wneighbors([N-W| WNs], V, Graph, NewGraph) :-
		(	dict_lookup(N, NWNs, Graph) ->
			^^winsert_neighbor(NWNs, V, W, NewNWNs),
			dict_insert(Graph, N, NewNWNs, NewGraph0)
		;	dict_insert(Graph, N, [V-W], NewGraph0)
		),
		add_reversed_wneighbors(WNs, V, NewGraph0, NewGraph).

	% --- Warshall for weighted ---

	wwarshall([], Graph, Graph).
	wwarshall([Vertex| Vertices], Graph, Closure) :-
		dict_lookup(Vertex, VWNeighbors, Graph),
		dict_as_list(Graph, Pairs),
		wwarshall_update_pairs(Pairs, Vertex, VWNeighbors, NewPairs),
		dict_as_dictionary(NewPairs, NewGraph),
		wwarshall(Vertices, NewGraph, Closure).

	wwarshall_update_pairs([], _, _, []).
	wwarshall_update_pairs([X-XWNs| Pairs], V, VWNs, [X-NewXWNs| NewPairs]) :-
		(	^^wfind(XWNs, V, W) ->
			wadd_through(VWNs, W, XWNs, NewXWNs)
		;	NewXWNs = XWNs
		),
		wwarshall_update_pairs(Pairs, V, VWNs, NewPairs).

	wadd_through([], _, WNs, WNs).
	wadd_through([N-W1|VWNs], W0, XWNs, NewXWNs) :-
		NewW is W0 + W1,
		winsert_if_better(XWNs, N, NewW, XWNs1),
		wadd_through(VWNs, W0, XWNs1, NewXWNs).

	winsert_if_better([], N, W, [N-W]).
	winsert_if_better([V0-W0|Rest], N, W, Result) :-
		compare(Order, N, V0),
		(	Order == (<) ->
			Result = [N-W, V0-W0|Rest]
		;	Order == (=) ->
			(	W < W0 ->
				Result = [N-W|Rest]
			;	Result = [V0-W0|Rest]
			)
		;	Result = [V0-W0|Rest1],
			winsert_if_better(Rest, N, W, Rest1)
		).

	% --- Invert weighted edges ---

	invert_wedges([], []).
	invert_wedges([(Vertex1-Vertex2)-W|Edges], [(Vertex2-Vertex1)-W|InvertedEdges]) :-
		invert_wedges(Edges, InvertedEdges).

	% --- Topological sort helpers ---

	strip_wpairs([], []).
	strip_wpairs([Vertex-WNeighbors| WPairs], [Vertex-Neighbors| Pairs]) :-
		strip_weights(WNeighbors, Neighbors),
		strip_wpairs(WPairs, Pairs).

	zeros([], []).
	zeros([_| Vertices], [0| Zeros]) :-
		zeros(Vertices, Zeros).

	count_edges([], _, Counts, Counts).
	count_edges([_-Neighbors|Pairs], Vertices, Counts0, Counts2) :-
		incr_list(Neighbors, Vertices, Counts0, Counts1),
		count_edges(Pairs, Vertices, Counts1, Counts2).

	incr_list([], _, Counts, Counts).
	incr_list([Vertex1| Neighbors], [Vertex2| Vertices], [M| Counts0], [N| Counts1]) :-
		Vertex1 == Vertex2,
		!,
		N is M + 1,
		incr_list(Neighbors, Vertices, Counts0, Counts1).
	incr_list(Neighbors, [_| Vertices], [N| Counts0], [N| Counts1]) :-
		incr_list(Neighbors, Vertices, Counts0, Counts1).

	select_zeros([], [], []).
	select_zeros([0| Counts], [Vertex| Vertices], [Vertex| Zeros]) :-
		!,
		select_zeros(Counts, Vertices, Zeros).
	select_zeros([_| Counts], [_| Vertices], Zeros) :-
		select_zeros(Counts, Vertices, Zeros).

	top_sort_loop([], Sorted0, Sorted0, _, _, _).
	top_sort_loop([Zero| Zeros], [Zero| Sorted], Sorted0, UGraph, Vertices, Counts1) :-
		dict_lookup(Zero, Neighbors, UGraph),
		decr_list(Neighbors, Vertices, Counts1, Counts2, Zeros, NewZeros),
		top_sort_loop(NewZeros, Sorted, Sorted0, UGraph, Vertices, Counts2).

	decr_list([], _, Counts, Counts, Zeros, Zeros).
	decr_list([Vertex1| Neighbors], [Vertex2| Vertices], [1| Counts1], [0| Counts2], Zi, Zo) :-
		Vertex1 == Vertex2,
		!,
		decr_list(Neighbors, Vertices, Counts1, Counts2, [Vertex2| Zi], Zo).
	decr_list([Vertex1| Neighbors], [Vertex2| Vertices], [N| Counts1], [M| Counts2], Zi, Zo) :-
		Vertex1 == Vertex2,
		!,
		M is N - 1,
		decr_list(Neighbors, Vertices, Counts1, Counts2, Zi, Zo).
	decr_list(Neighbors, [_| Vertices], [N| Counts1], [N| Counts2], Zi, Zo) :-
		decr_list(Neighbors, Vertices, Counts1, Counts2, Zi, Zo).

	% --- Dijkstra all-pairs (for min_paths) ---

	dijkstra_all([], _, Dist, Dist).
	dijkstra_all([D-V|Queue], Graph, Dist, FinalDist) :-
		dict_lookup(V, BestD-_, Dist),
		(	D > BestD ->
			dijkstra_all(Queue, Graph, Dist, FinalDist)
		;	(	dict_lookup(V, WNeighbors, Graph) ->
				^^relax_neighbors(WNeighbors, V, D, Queue, Dist, NewQueue, NewDist)
			;	NewQueue = Queue,
				NewDist = Dist
			),
			dijkstra_all(NewQueue, Graph, NewDist, FinalDist)
		).

	% --- Min paths tree building ---

	build_path_tree_edges([], _, []).
	build_path_tree_edges([V-(_-Prev)|DistPairs], Source, TreeEdges) :-
		(	V == Source ->
			build_path_tree_edges(DistPairs, Source, TreeEdges)
		;	Prev == none ->
			build_path_tree_edges(DistPairs, Source, TreeEdges)
		;	TreeEdges = [(Prev-V)-1| RestEdges],
			build_path_tree_edges(DistPairs, Source, RestEdges)
		).

	add_vertices_from_dist([], Tree, Tree).
	add_vertices_from_dist([Vertex-_| DistPairs], Tree0, Tree) :-
		(	dict_lookup(Vertex, _, Tree0) ->
			add_vertices_from_dist(DistPairs, Tree0, Tree)
		;	dict_insert(Tree0, Vertex, [], Tree1),
			add_vertices_from_dist(DistPairs, Tree1, Tree)
		).

	add_tree_edges([], Tree, Tree).
	add_tree_edges([(Vertex1-Vertex2)-Weight| Edges], Tree0, Tree) :-
		add_edge(Tree0, Vertex1, Vertex2, Weight, Tree1),
		add_tree_edges(Edges, Tree1, Tree).

:- end_object.


:- object(weighted_directed_graph,
	extends(weighted_directed_graph(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Weighted directed graph predicates using the AVL tree dictionary representation. Edge weights use a pair representation (Vertex-Weight in neighbor lists, (Vertex1-Vertex2)-Weight for edge lists).'
	]).

:- end_object.
