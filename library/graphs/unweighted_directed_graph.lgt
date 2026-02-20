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


:- object(unweighted_directed_graph(_Dictionary_),
	implements((unweighted_graph_protocol, directed_graph_protocol)),
	imports((graph_common, directed_graph_common))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Unweighted directed graph predicates using a dictionary representation. The parametric object parameter is the dictionary to use for the graph representation.',
		parnames is ['Dictionary']
	]).

	% Additional public predicates specific to unweighted directed graphs

	:- public(compose/3).
	:- mode(compose(+graph, +graph, -graph), one).
	:- info(compose/3, [
		comment is 'Composes ``NewGraph`` by connecting the drains of ``LeftGraph`` to the sources of ``RightGraph``.',
		argnames is ['LeftGraph', 'RightGraph', 'NewGraph']
	]).

	:- public(union/3).
	:- mode(union(+graph, +graph, -graph), one).
	:- info(union/3, [
		comment is 'Unifies ``UnionGraph`` with the union of ``Graph1`` and ``Graph2``.',
		argnames is ['Graph1', 'Graph2', 'UnionGraph']
	]).

	:- public(topological_sort/3).
	:- mode(topological_sort(+graph, +list(vertex), -list(vertex)), one).
	:- info(topological_sort/3, [
		comment is 'Difference list version of ``topological_sort/2`` where ``Sorted0`` is the tail of ``Sorted``.',
		argnames is ['Graph', 'Sorted0', 'Sorted']
	]).

	:- public(leaves/2).
	:- mode(leaves(+graph, -list(vertex)), one).
	:- info(leaves/2, [
		comment is 'Unifies ``Leaves`` with a sorted list of vertices with no outgoing edges.',
		argnames is ['Graph', 'Leaves']
	]).

	:- public(transitive_reduction/2).
	:- mode(transitive_reduction(+graph, -graph), one).
	:- info(transitive_reduction/2, [
		comment is 'Computes the transitive reduction of the graph. An edge Vertex1-Vertex2 is in the reduction iff it is in the graph and there is no other path of length >= 2 from Vertex1 to Vertex2.',
		argnames is ['Graph', 'Reduction']
	]).

	% Dictionary operations (aliased to avoid name clashes)

	:- uses(_Dictionary_, [
		new/1 as dict_new/1,
		empty/1 as dict_empty/1,
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		delete/4 as dict_delete/4,
		keys/2 as dict_keys/2,
		as_list/2 as dict_as_list/2,
		as_dictionary/2 as dict_as_dictionary/2
	]).

	:- uses(list, [
		length/2, reverse/2,
		member/2 as list_member/2
	]).

	:- uses(pairs, [
		keys/2, transpose/2 as invert_edges/2
	]).

	% Sorted set operations

	:- uses(set, [
		insert/3 as set_insert/3,
		subtract/3 as set_subtract/3,
		union/3 as set_union/3,
		member/2 as set_member/2,
		memberchk/2 as set_memberchk/2
	]).

	% BFS min_path uses avltree for predecessor map

	:- uses(avltree, [
		new/1 as bfs_new/1,
		insert/4 as bfs_insert/4,
		lookup/3 as bfs_lookup/3
	]).

	% === Graph creation (new/1 only; new/2, new/3 from graph_common) ===

	new(Graph) :-
		dict_new(Graph).

	% === Basic queries ===

	empty(Graph) :-
		dict_empty(Graph).

	vertices(Graph, Vertices) :-
		dict_keys(Graph, Vertices).

	edges(Graph, Edges) :-
		dict_as_list(Graph, Pairs),
		^^pairs_to_edges(Pairs, Edges).

	% === Vertex operations (add_vertices/3, delete_vertices/3 from graph_common) ===

	add_vertex(Graph, Vertex, NewGraph) :-
		(	dict_lookup(Vertex, _, Graph) ->
			NewGraph = Graph
		;	dict_insert(Graph, Vertex, [], NewGraph)
		).

	delete_vertex(Graph, Vertex, NewGraph) :-
		(	dict_delete(Graph, Vertex, _, NewGraph0) ->
			dict_as_list(NewGraph0, Pairs),
			remove_vertex_from_all(Pairs, Vertex, NewPairs),
			dict_as_dictionary(NewPairs, NewGraph)
		;	NewGraph = Graph
		).

	% === Edge operations ===

	edge(Vertex1, Vertex2, Graph) :-
		dict_lookup(Vertex1, Neighbors, Graph),
		set_memberchk(Vertex2, Neighbors).

	add_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		(	dict_lookup(Vertex1, Neighbors, Graph) ->
			set_insert(Neighbors, Vertex2, NewNeighbors)
		;	NewNeighbors = [Vertex2]
		),
		dict_insert(Graph, Vertex1, NewNeighbors, NewGraph0),
		(	dict_lookup(Vertex2, _, NewGraph0) ->
			NewGraph = NewGraph0
		;	dict_insert(NewGraph0, Vertex2, [], NewGraph)
		).

	add_edges(NewGraph, [], NewGraph).
	add_edges(Graph, [Vertex1-Vertex2|Edges], NewGraph) :-
		add_edge(Graph, Vertex1, Vertex2, NewGraph0),
		add_edges(NewGraph0, Edges, NewGraph).

	delete_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		(	dict_lookup(Vertex1, Ns, Graph) ->
			set_subtract(Ns, [Vertex2], NewNs),
			dict_insert(Graph, Vertex1, NewNs, NewGraph)
		;	NewGraph = Graph
		).

	delete_edges(NewGraph, [], NewGraph).
	delete_edges(Graph, [Vertex1-Vertex2| Edges], NewGraph) :-
		delete_edge(Graph, Vertex1, Vertex2, NewGraph0),
		delete_edges(NewGraph0, Edges, NewGraph).

	% === Neighbor queries ===

	neighbors(Vertex, Graph, Neighbors) :-
		dict_lookup(Vertex, Neighbors, Graph).

	% === Complement ===

	complement(Graph, NewGraph) :-
		dict_keys(Graph, AllVertices),
		dict_as_list(Graph, Pairs),
		complement_pairs(Pairs, AllVertices, NewPairs),
		dict_as_dictionary(NewPairs, NewGraph).

	% === Transpose ===

	transpose(Graph, NewGraph) :-
		dict_as_list(Graph, Pairs),
		dict_new(NewGraph0),
		add_empty_vertices_from_pairs(Pairs, NewGraph0, NewGraph1),
		add_transposed_edges(Pairs, NewGraph1, NewGraph).

	% === Transitive Closure (Warshall) ===

	transitive_closure(Graph, Closure) :-
		dict_keys(Graph, Vertices),
		warshall(Vertices, Graph, Closure).

	% === Symmetric Closure ===

	symmetric_closure(Graph, Closure) :-
		edges(Graph, Edges),
		invert_edges(Edges, InvertedEdges),
		add_edges(Graph, InvertedEdges, Closure).

	% === Compose ===

	compose(Graph1, Graph2, Composition) :-
		dict_keys(Graph1, Vertex1),
		dict_keys(Graph2, Vertex2),
		set_union(Vertex1, Vertex2, AllVertices),
		compose_vertices(AllVertices, Graph1, Graph2, CompPairs),
		dict_as_dictionary(CompPairs, Composition).

	% === Union ===

	union(Graph1, Graph2, UnionGraph) :-
		dict_as_list(Graph1, Pairs1),
		dict_as_list(Graph2, Pairs2),
		merge_graph_pairs(Pairs1, Pairs2, MergedPairs),
		dict_as_dictionary(MergedPairs, UnionGraph).

	% === Topological Sort (Kahn's algorithm) ===

	topological_sort(Graph, Sorted) :-
		topological_sort(Graph, [], Sorted).

	topological_sort(Graph, Sorted0, Sorted) :-
		dict_as_list(Graph, Pairs),
		keys(Pairs, Vertices),
		zeros(Vertices, Counts0),
		count_edges(Pairs, Vertices, Counts0, Counts1),
		select_zeros(Counts1, Vertices, Zeros),
		top_sort_loop(Zeros, Sorted, Sorted0, Graph, Vertices, Counts1).

	% === Leaves ===

	leaves(Graph, Leaves) :-
		dict_as_list(Graph, Pairs),
		find_leaves(Pairs, Leaves).

	% === Transitive Reduction ===

	transitive_reduction(Graph, Reduction) :-
		transitive_closure(Graph, TC),
		compose(Graph, TC, GTC),
		edges(Graph, GEdges),
		edges(GTC, GTCEdges),
		set_subtract(GEdges, GTCEdges, RedEdges),
		vertices(Graph, Vertices),
		^^new(Vertices, RedEdges, Reduction).

	% === Min Path (BFS for unweighted) ===

	min_path(Vertex1, Vertex2, Graph, Path, Length) :-
		(	Vertex1 == Vertex2 ->
			neighbors(Vertex1, Graph, _),
			Path = [Vertex1], Length = 0
		;	bfs_new(Visited0),
			bfs_insert(Visited0, Vertex1, true, Visited1),
			bfs_new(Pred0),
			bfs_shortest([Vertex1], [], Vertex2, Graph, Visited1, Pred0, Pred),
			bfs_trace(Vertex2, Vertex1, Pred, [Vertex2], Path),
			length(Path, N),
			Length is N - 1
		).

	% === Max Path (DFS for unweighted) ===

	max_path(Vertex1, Vertex2, Graph, Path, Length) :-
		(	Vertex1 == Vertex2 ->
			neighbors(Vertex1, Graph, _),
			Path = [Vertex1], Length = 0
		;	max_path_dfs(Vertex1, Vertex2, Graph, [Vertex1], none, -1, RevPath, Length),
			Length >= 0,
			reverse(RevPath, Path)
		).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- Remove vertex from all neighbor lists ---

	remove_vertex_from_all([], _, []).
	remove_vertex_from_all([Vertex0-Neighbors| Pairs], Vertex, [Vertex0-NewNeighbors| NewPairs]) :-
		set_subtract(Neighbors, [Vertex], NewNeighbors),
		remove_vertex_from_all(Pairs, Vertex, NewPairs).

	% --- Complement ---

	complement_pairs([], _, []).
	complement_pairs([Vertex-Neighbors| Pairs], AllVertices, [Vertex-ComplementNeighbors| NewPairs]) :-
		set_insert(Neighbors, Vertex, NeighborsWithSelf),
		set_subtract(AllVertices, NeighborsWithSelf, ComplementNeighbors),
		complement_pairs(Pairs, AllVertices, NewPairs).

	% --- Transpose ---

	add_empty_vertices_from_pairs([], NewGraph, NewGraph).
	add_empty_vertices_from_pairs([Vertex-_| Pairs], Graph, NewGraph) :-
		(	dict_lookup(Vertex, _, Graph) ->
			add_empty_vertices_from_pairs(Pairs, Graph, NewGraph)
		;	dict_insert(Graph, Vertex, [], NewGraph0),
			add_empty_vertices_from_pairs(Pairs, NewGraph0, NewGraph)
		).

	add_transposed_edges([], NewGraph, NewGraph).
	add_transposed_edges([Vertex-Neighbors| Pairs], Graph, NewGraph) :-
		add_reversed_neighbors(Neighbors, Vertex, Graph, NewGraph0),
		add_transposed_edges(Pairs, NewGraph0, NewGraph).

	add_reversed_neighbors([], _, NewGraph, NewGraph).
	add_reversed_neighbors([Neighbor| Neighbors], Vertex, Graph, NewGraph) :-
		(	dict_lookup(Neighbor, NeighborNeighbors, Graph) ->
			set_insert(NeighborNeighbors, Vertex, NewNeighborNeighbors),
			dict_insert(Graph, Neighbor, NewNeighborNeighbors, NewGraph0)
		;	dict_insert(Graph, Neighbor, [Vertex], NewGraph0)
		),
		add_reversed_neighbors(Neighbors, Vertex, NewGraph0, NewGraph).

	% --- Warshall ---

	warshall([], Graph, Graph).
	warshall([Vertex| Vertices], Graph, Closure) :-
		dict_lookup(Vertex, VNeighbors, Graph),
		dict_as_list(Graph, Pairs),
		warshall_update_pairs(Pairs, Vertex, VNeighbors, NewPairs),
		dict_as_dictionary(NewPairs, NewGraph),
		warshall(Vertices, NewGraph, Closure).

	warshall_update_pairs([], _, _, []).
	warshall_update_pairs([X-XNs| Pairs], Vertex, VNs, [X-NewXNs| NewPairs]) :-
		(	set_member(Vertex, XNs) ->
			set_union(XNs, VNs, NewXNs)
		;	NewXNs = XNs
		),
		warshall_update_pairs(Pairs, Vertex, VNs, NewPairs).

	% --- Compose ---

	compose_vertices([], _, _, []).
	compose_vertices([Vertex| Vertices], Graph1, Graph2, [Vertex-CompNeighbors| Rest]) :-
		(	dict_lookup(Vertex, Neighbors1, Graph1) ->
			compose_through_neighbors(Neighbors1, Graph2, [], CompNeighbors)
		;	CompNeighbors = []
		),
		compose_vertices(Vertices, Graph1, Graph2, Rest).

	compose_through_neighbors([], _, Acc, Acc).
	compose_through_neighbors([Neighbor1| Neighbors1], Graph2, Acc, Result) :-
		(	dict_lookup(Neighbor1, Neighbors2, Graph2) ->
			set_union(Acc, Neighbors2, NewAcc)
		;	NewAcc = Acc
		),
		compose_through_neighbors(Neighbors1, Graph2, NewAcc, Result).

	% --- Merge graph pairs (for union) ---

	merge_graph_pairs([], Pairs2, Pairs2).
	merge_graph_pairs(Pairs1, [], Pairs1).
	merge_graph_pairs([Vertex1-Neighbors1| Pairs1], [Vertex2-Neighbors2| Pairs2], Merged) :-
		compare(Order, Vertex1, Vertex2),
		(	Order == (=) ->
			set_union(Neighbors1, Neighbors2, MergedNeighbors),
			Merged = [Vertex1-MergedNeighbors| Rest],
			merge_graph_pairs(Pairs1, Pairs2, Rest)
		;	Order == (<) ->
			Merged = [Vertex1-Neighbors1| Rest],
			merge_graph_pairs(Pairs1, [Vertex2-Neighbors2| Pairs2], Rest)
		;	Merged = [Vertex2-Neighbors2| Rest],
			merge_graph_pairs([Vertex1-Neighbors1| Pairs1], Pairs2, Rest)
		).

	% --- Topological sort predicates ---

	zeros([], []).
	zeros([_| Vertices], [0| Zeros]) :-
		zeros(Vertices, Zeros).

	count_edges([], _, Counts, Counts).
	count_edges([_-Neighbors| Pairs], Vertices, Counts0, Counts2) :-
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
	top_sort_loop([Zero| Zeros], [Zero| Sorted], Sorted0, Graph, Vertices, Counts1) :-
		dict_lookup(Zero, Neighbors, Graph),
		decr_list(Neighbors, Vertices, Counts1, Counts2, Zeros, NewZeros),
		top_sort_loop(NewZeros, Sorted, Sorted0, Graph, Vertices, Counts2).

	decr_list([], _, Counts, Counts, Zeros, Zeros).
	decr_list([Vertex1| Neighbors], [Vertex2| Vertices], [1| Counts1], [0| Counts2], Zi, Zo) :-
		Vertex1 == Vertex2,
		!,
		decr_list(Neighbors, Vertices, Counts1, Counts2, [Vertex2|Zi], Zo).
	decr_list([Vertex1| Neighbors], [Vertex2| Vertices], [N| Counts1], [M| Counts2], Zi, Zo) :-
		Vertex1 == Vertex2,
		!,
		M is N - 1,
		decr_list(Neighbors, Vertices, Counts1, Counts2, Zi, Zo).
	decr_list(Neighbors, [_| Vertices], [N| Counts1], [N| Counts2], Zi, Zo) :-
		decr_list(Neighbors, Vertices, Counts1, Counts2, Zi, Zo).

	% --- Leaves ---

	find_leaves([], []).
	find_leaves([Vertex-[]| Pairs], [Vertex| Leaves]) :-
		!,
		find_leaves(Pairs, Leaves).
	find_leaves([_-[_|_]| Pairs], Leaves) :-
		find_leaves(Pairs, Leaves).

	% --- BFS min_path helpers ---

	bfs_shortest([], Next, Target, Graph, Visited, Pred0, Pred) :-
		Next \== [],
		bfs_shortest(Next, [], Target, Graph, Visited, Pred0, Pred).
	bfs_shortest([Vertex| Vertices], Next0, Target, Graph, Visited0, Pred0, Pred) :-
		dict_lookup(Vertex, Ns, Graph),
		bfs_expand(Ns, Vertex, Target, Visited0, Pred0, Next0, Visited1, Pred1, Next1, Found),
		(	Found == true ->
			Pred = Pred1
		;	bfs_shortest(Vertices, Next1, Target, Graph, Visited1, Pred1, Pred)
		).

	bfs_expand([], _, _, Visited, Pred, Next, Visited, Pred, Next, false).
	bfs_expand([Neighbor| Neighbors], Vertex, Target, Visited0, Pred0, Next0, Visited, Pred, Next, Found) :-
		(	bfs_lookup(Neighbor, _, Visited0) ->
			bfs_expand(Neighbors, Vertex, Target, Visited0, Pred0, Next0, Visited, Pred, Next, Found)
		;	bfs_insert(Visited0, Neighbor, true, Visited1),
			bfs_insert(Pred0, Neighbor, Vertex, Pred1),
			(	Neighbor == Target ->
				Found = true, Visited = Visited1, Pred = Pred1, Next = Next0
			;	bfs_expand(Neighbors, Vertex, Target, Visited1, Pred1, [Neighbor| Next0], Visited, Pred, Next, Found)
			)
		).

	bfs_trace(Vertex, Vertex, _, Path, Path) :-
		!.
	bfs_trace(Vertex, Start, Pred, Acc, Path) :-
		bfs_lookup(Vertex, Previous, Pred),
		bfs_trace(Previous, Start, Pred, [Previous| Acc], Path).

	% --- DFS max_path helpers ---

	max_path_dfs(Vertex, Target, _Graph, Visited, BestPath0, BestLength0, BestPath, BestLength) :-
		Vertex == Target,
		!,
		length(Visited, N),
		Length is N - 1,
		(	Length > BestLength0 ->
			BestLength = Length, BestPath = Visited
		;	BestLength = BestLength0, BestPath = BestPath0
		).
	max_path_dfs(Vertex, Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength) :-
		dict_lookup(Vertex, Neighbors, Graph),
		max_path_try(Neighbors, Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength).

	max_path_try([], _, _, _, BestPath, BestLen, BestPath, BestLen).
	max_path_try([Neighbor| Neighbors], Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength) :-
		(	list_member(Neighbor, Visited) ->
			max_path_try(Neighbors, Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength)
		;	max_path_dfs(Neighbor, Target, Graph, [Neighbor| Visited], BestPath0, BestLength0, BestPath1, BestLength1),
			max_path_try(Neighbors, Target, Graph, Visited, BestPath1, BestLength1, BestPath, BestLength)
		).

:- end_object.


:- object(unweighted_directed_graph,
	extends(unweighted_directed_graph(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Unweighted directed graph predicates using the AVL tree dictionary representation.'
	]).

:- end_object.
