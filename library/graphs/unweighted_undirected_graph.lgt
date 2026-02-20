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


:- object(unweighted_undirected_graph(_Dictionary_),
	implements(unweighted_graph_protocol),
	imports((graph_common, undirected_graph_common))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Unweighted undirected graph predicates using a dictionary representation. Undirected edges are stored as two directed edges. The parametric object parameter is the dictionary to use for the graph representation.',
		parnames is ['Dictionary']
	]).

	% Additional public predicates specific to undirected graphs

	:- public(degree/3).
	:- mode(degree(+vertex, +graph, -integer), zero_or_one).
	:- info(degree/3, [
		comment is 'Unifies ``Degree`` with the number of edges incident to ``Vertex``. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Degree']
	]).

	:- public(is_connected/1).
	:- mode(is_connected(+graph), zero_or_one).
	:- info(is_connected/1, [
		comment is 'True iff the graph is connected (every vertex is reachable from every other vertex).',
		argnames is ['Graph']
	]).

	:- public(connected_components/2).
	:- mode(connected_components(+graph, -list(list(vertex))), one).
	:- info(connected_components/2, [
		comment is 'Unifies ``Components`` with a list of connected components. Each component is a sorted list of vertices.',
		argnames is ['Graph', 'Components']
	]).

	% Dictionary operations

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

	:- uses(set, [
		insert/3 as set_insert/3,
		subtract/3 as set_subtract/3,
		memberchk/2 as set_memberchk/2
	]).

	:- uses(list, [
		length/2, reverse/2, subtract/3,
		memberchk/2 as list_memberchk/2
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

	% For undirected graphs, return each edge only once (Vertex1 @< Vertex2 or Vertex1 == Vertex2)
	edges(Graph, Edges) :-
		dict_as_list(Graph, Pairs),
		^^pairs_to_edges(Pairs, AllEdges),
		remove_duplicate_edges(AllEdges, Edges).

	% === Vertex operations (add_vertices/3, delete_vertices/3 from graph_common) ===

	add_vertex(Graph, Vertex, NewGraph) :-
		(	dict_lookup(Vertex, _, Graph) ->
			NewGraph = Graph
		;	dict_insert(Graph, Vertex, [], NewGraph)
		).

	delete_vertex(Graph, Vertex, NewGraph) :-
		(	dict_delete(Graph, Vertex, _, G1) ->
			dict_as_list(G1, Pairs),
			remove_vertex_from_all(Pairs, Vertex, NewPairs),
			dict_as_dictionary(NewPairs, NewGraph)
		;	NewGraph = Graph
		).

	% === Edge operations (add both directions) ===

	edge(Vertex1, Vertex2, Graph) :-
		dict_lookup(Vertex1, Neighbors, Graph),
		set_memberchk(Vertex2, Neighbors).

	add_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		add_directed_edge(Graph, Vertex1, Vertex2, G1),
		add_directed_edge(G1, Vertex2, Vertex1, NewGraph).

	add_edges(Graph, [], Graph).
	add_edges(Graph, [Vertex1-Vertex2| Edges], NewGraph) :-
		add_edge(Graph, Vertex1, Vertex2, G1),
		add_edges(G1, Edges, NewGraph).

	delete_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		delete_directed_edge(Graph, Vertex1, Vertex2, G1),
		delete_directed_edge(G1, Vertex2, Vertex1, NewGraph).

	delete_edges(Graph, [], Graph).
	delete_edges(Graph, [Vertex1-Vertex2| Edges], NewGraph) :-
		delete_edge(Graph, Vertex1, Vertex2, G1),
		delete_edges(G1, Edges, NewGraph).

	% === Neighbor queries (exclude self-loops) ===

	neighbors(Vertex, Graph, Neighbors) :-
		dict_lookup(Vertex, AllNeighbors, Graph),
		set_subtract(AllNeighbors, [Vertex], Neighbors).

	% === Complement ===

	complement(Graph, NewGraph) :-
		dict_keys(Graph, AllVertices),
		dict_as_list(Graph, Pairs),
		complement_pairs(Pairs, AllVertices, NewPairs),
		dict_as_dictionary(NewPairs, NewGraph).

	% === Degree ===

	degree(Vertex, Graph, Degree) :-
		neighbors(Vertex, Graph, Ns),
		length(Ns, Degree).

	% === Connectivity ===

	is_connected(Graph) :-
		dict_keys(Graph, Vertices),
		(	Vertices == [] ->
			true
		;	Vertices = [Vertex| _],
			^^reachable(Vertex, Graph, Reachable),
			length(Vertices, N),
			length(Reachable, N)
		).

	connected_components(Graph, Components) :-
		dict_keys(Graph, Vertices),
		find_components(Vertices, Graph, Components).

	% === Min Path (BFS for unweighted) ===

	min_path(Vertex1, Vertex2, Graph, Path, Length) :-
		(	Vertex1 == Vertex2 ->
			neighbors(Vertex1, Graph, _),
			Path = [Vertex1],
			Length = 0
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

	% --- Directed edge helpers (internal) ---

	add_directed_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		(	dict_lookup(Vertex1, Ns, Graph) ->
			set_insert(Ns, Vertex2, NewNs)
		;	NewNs = [Vertex2]
		),
		dict_insert(Graph, Vertex1, NewNs, G1),
		(	dict_lookup(Vertex2, _, G1) ->
			NewGraph = G1
		;	dict_insert(G1, Vertex2, [], NewGraph)
		).

	delete_directed_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		(	dict_lookup(Vertex1, Ns, Graph) ->
			set_subtract(Ns, [Vertex2], NewNs),
			dict_insert(Graph, Vertex1, NewNs, NewGraph)
		;	NewGraph = Graph
		).

	% Only keep edges where Vertex1 @< Vertex2 or Vertex1 == Vertex2 (self-loops)
	remove_duplicate_edges([], []).
	remove_duplicate_edges([Vertex1-Vertex2|Edges], Result) :-
		(	Vertex1 @> Vertex2 ->
			Result = Rest
		;	Result = [Vertex1-Vertex2|Rest]
		),
		remove_duplicate_edges(Edges, Rest).

	% --- Remove vertex from all neighbor lists ---

	remove_vertex_from_all([], _, []).
	remove_vertex_from_all([V-Ns|Pairs], Vertex, [V-NewNs|NewPairs]) :-
		set_subtract(Ns, [Vertex], NewNs),
		remove_vertex_from_all(Pairs, Vertex, NewPairs).

	% --- Complement ---

	complement_pairs([], _, []).
	complement_pairs([V-Ns|Pairs], AllVertices, [V-CompNs|NewPairs]) :-
		set_insert(Ns, V, NsWithSelf),
		set_subtract(AllVertices, NsWithSelf, CompNs),
		complement_pairs(Pairs, AllVertices, NewPairs).

	% --- Connected components ---

	find_components([], _, []).
	find_components([Vertex| Vertices], Graph, [Component| Components]) :-
		^^reachable(Vertex, Graph, Component),
		subtract(Vertices, Component, Remaining),
		find_components(Remaining, Graph, Components).

	% --- BFS min_path helpers ---

	bfs_shortest([], Next, Target, Graph, Visited, Pred0, Pred) :-
		Next \== [],
		bfs_shortest(Next, [], Target, Graph, Visited, Pred0, Pred).
	bfs_shortest([V|Vertices], Next0, Target, Graph, Visited0, Pred0, Pred) :-
		dict_lookup(V, AllNs, Graph),
		set_subtract(AllNs, [V], Ns),
		bfs_expand(Ns, V, Target, Visited0, Pred0, Next0, Visited1, Pred1, Next1, Found),
		(	Found == true ->
			Pred = Pred1
		;	bfs_shortest(Vertices, Next1, Target, Graph, Visited1, Pred1, Pred)
		).

	bfs_expand([], _, _, Visited, Pred, Next, Visited, Pred, Next, false).
	bfs_expand([N|Ns], V, Target, Visited0, Pred0, Next0, Visited, Pred, Next, Found) :-
		(	bfs_lookup(N, _, Visited0) ->
			bfs_expand(Ns, V, Target, Visited0, Pred0, Next0, Visited, Pred, Next, Found)
		;	bfs_insert(Visited0, N, true, Visited1),
			bfs_insert(Pred0, N, V, Pred1),
			(	N == Target ->
				Found = true, Visited = Visited1, Pred = Pred1, Next = Next0
			;	bfs_expand(Ns, V, Target, Visited1, Pred1, [N|Next0], Visited, Pred, Next, Found)
			)
		).

	bfs_trace(V, V, _, Path, Path) :- !.
	bfs_trace(V, Start, Pred, Acc, Path) :-
		bfs_lookup(V, Prev, Pred),
		bfs_trace(Prev, Start, Pred, [Prev|Acc], Path).

	% --- DFS max_path helpers ---

	max_path_dfs(V, Target, _Graph, Visited, BestPath0, BestLen0, BestPath, BestLen) :-
		V == Target,
		!,
		length(Visited, N),
		Len is N - 1,
		(	Len > BestLen0 ->
			BestLen = Len, BestPath = Visited
		;	BestLen = BestLen0, BestPath = BestPath0
		).
	max_path_dfs(V, Target, Graph, Visited, BestPath0, BestLen0, BestPath, BestLen) :-
		neighbors(V, Graph, Ns),
		max_path_try(Ns, Target, Graph, Visited, BestPath0, BestLen0, BestPath, BestLen).

	max_path_try([], _, _, _, BestPath, BestLen, BestPath, BestLen).
	max_path_try([N|Ns], Target, Graph, Visited, BestPath0, BestLen0, BestPath, BestLen) :-
		(	list_memberchk(N, Visited) ->
			max_path_try(Ns, Target, Graph, Visited, BestPath0, BestLen0, BestPath, BestLen)
		;	max_path_dfs(N, Target, Graph, [N|Visited], BestPath0, BestLen0, BestPath1, BestLen1),
			max_path_try(Ns, Target, Graph, Visited, BestPath1, BestLen1, BestPath, BestLen)
		).

:- end_object.


:- object(unweighted_undirected_graph,
	extends(unweighted_undirected_graph(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Unweighted undirected graph predicates using the AVL tree dictionary representation.'
	]).

:- end_object.
