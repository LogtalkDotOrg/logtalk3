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


:- category(unweighted_graph_common(_Dictionary_),
	implements(unweighted_graph_protocol),
	extends(graph_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Common unweighted graph predicates shared by both unweighted directed and unweighted undirected graph objects. Uses self-dispatch to call object-specific predicates such as ``add_edge/4``, ``delete_edge/4``, and ``neighbors/3``.',
		parnames is ['Dictionary']
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
		length/2, member/2, reverse/2
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

	% === Edge queries ===

	edge(Vertex1, Vertex2, Graph) :-
		dict_lookup(Vertex1, Neighbors, Graph),
		set_memberchk(Vertex2, Neighbors).

	% === Edge list operations (add_edge/4, delete_edge/4 are object-specific) ===

	add_edges(Graph, [], Graph).
	add_edges(Graph, [Vertex1-Vertex2| Edges], NewGraph) :-
		::add_edge(Graph, Vertex1, Vertex2, NewGraph0),
		add_edges(NewGraph0, Edges, NewGraph).

	delete_edges(Graph, [], Graph).
	delete_edges(Graph, [Vertex1-Vertex2| Edges], NewGraph) :-
		::delete_edge(Graph, Vertex1, Vertex2, NewGraph0),
		delete_edges(NewGraph0, Edges, NewGraph).

	% === Complement ===

	complement(Graph, NewGraph) :-
		dict_keys(Graph, AllVertices),
		dict_as_list(Graph, Pairs),
		complement_pairs(Pairs, AllVertices, NewPairs),
		dict_as_dictionary(NewPairs, NewGraph).

	% === Min Path (BFS for unweighted) ===

	min_path(Vertex1, Vertex2, Graph, Path, Length) :-
		(	Vertex1 == Vertex2 ->
			::neighbors(Vertex1, Graph, _),
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
			::neighbors(Vertex1, Graph, _),
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

	% --- BFS min_path helpers ---

	bfs_shortest([], Next, Target, Graph, Visited, Pred0, Pred) :-
		Next \== [],
		bfs_shortest(Next, [], Target, Graph, Visited, Pred0, Pred).
	bfs_shortest([Vertex| Vertices], Next0, Target, Graph, Visited0, Pred0, Pred) :-
		::neighbors(Vertex, Graph, Ns),
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
		::neighbors(Vertex, Graph, Neighbors),
		max_path_try(Neighbors, Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength).

	max_path_try([], _, _, _, BestPath, BestLength, BestPath, BestLength).
	max_path_try([Neighbor| Neighbors], Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength) :-
		(	member(Neighbor, Visited) ->
			max_path_try(Neighbors, Target, Graph, Visited, BestPath0, BestLength0, BestPath, BestLength)
		;	max_path_dfs(Neighbor, Target, Graph, [Neighbor| Visited], BestPath0, BestLength0, BestPath1, BestLength1),
			max_path_try(Neighbors, Target, Graph, Visited, BestPath1, BestLength1, BestPath, BestLength)
		).

:- end_category.
