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


:- category(graph_common,
	implements(graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Common graph predicates shared by all graph objects. Uses self-dispatch to call object-specific predicates such as ``neighbors/3``, ``vertices/2``, and ``edges/2``.'
	]).

	:- uses(set, [
		union/4 as set_union/4,
		memberchk/2 as set_memberchk/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2
	]).

	:- protected(pairs_to_edges/2).
	:- mode(pairs_to_edges(+list(pair), -list), one).
	:- info(pairs_to_edges/2, [
		comment is 'Converts a list of ``Vertex-Neighbors`` pairs from a dictionary into a flat list of ``Vertex1-Vertex2`` edges.',
		argnames is ['Pairs', 'Edges']
	]).

	:- protected(vertex_neighbors_to_edges/4).
	:- mode(vertex_neighbors_to_edges(+list, +vertex, -list, -list), one).
	:- info(vertex_neighbors_to_edges/4, [
		comment is 'Converts a neighbor list for a vertex into edges using a difference list.',
		argnames is ['Neighbors', 'Vertex', 'Edges', 'RestEdges']
	]).

	:- protected(wpairs_to_edges/2).
	:- mode(wpairs_to_edges(+list(pair), -list), one).
	:- info(wpairs_to_edges/2, [
		comment is 'Converts a list of ``Vertex-WNeighbors`` pairs from a dictionary into a flat list of ``(Vertex1-Vertex2)-Weight`` weighted edges.',
		argnames is ['Pairs', 'Edges']
	]).

	:- protected(wvertex_neighbors_to_edges/4).
	:- mode(wvertex_neighbors_to_edges(+list, +vertex, -list, -list), one).
	:- info(wvertex_neighbors_to_edges/4, [
		comment is 'Converts a weighted neighbor list for a vertex into weighted edges using a difference list.',
		argnames is ['WNeighbors', 'Vertex', 'Edges', 'RestEdges']
	]).

	% === Graph creation ===

	new(Edges, Graph) :-
		new([], Edges, Graph).

	new(Vertices, Edges, Graph) :-
		::new(Empty),
		add_vertices(Empty, Vertices, G1),
		::add_edges(G1, Edges, Graph).

	% === Vertex list operations ===

	add_vertices(Graph, [], Graph).
	add_vertices(Graph, [Vertex| Vertices], NewGraph) :-
		::add_vertex(Graph, Vertex, NewGraph0),
		add_vertices(NewGraph0, Vertices, NewGraph).

	delete_vertices(Graph, [], Graph).
	delete_vertices(Graph, [Vertex| Vertices], NewGraph) :-
		::delete_vertex(Graph, Vertex, NewGraph0),
		delete_vertices(NewGraph0, Vertices, NewGraph).

	% === Graph metrics ===

	number_of_vertices(Graph, N) :-
		::vertices(Graph, Vs),
		length(Vs, N).

	number_of_edges(Graph, N) :-
		::edges(Graph, Es),
		length(Es, N).

	% === Reachability ===

	reachable(Vertex, Graph, Vertices) :-
		::neighbors(Vertex, Graph, _),
		reachable_aux([Vertex], Graph, [Vertex], Vertices).

	reachable_aux([], _, Visited, Visited).
	reachable_aux([Vertex| Vertices], Graph, Visited, Reachable) :-
		(	::neighbors(Vertex, Graph, Neighbors) ->
			set_union(Visited, Neighbors, NewVisited, NewVertices),
			append(Vertices, NewVertices, NextToVisit)
		;	NewVisited = Visited,
			NextToVisit = Vertices
		),
		reachable_aux(NextToVisit, Graph, NewVisited, Reachable).

	% === Path existence ===

	has_path(Vertex1, Vertex2, Graph) :-
		reachable(Vertex1, Graph, Reachable),
		set_memberchk(Vertex2, Reachable).

	% === Nondeterministic maximal paths ===

	path(Vertex, Graph, Path) :-
		::neighbors(Vertex, Graph, _),
		path_extend(Vertex, Graph, [Vertex], RevPath),
		reverse(RevPath, Path).

	path_extend(Current, Graph, Visited, Path) :-
		::neighbors(Current, Graph, Neighbors),
		unvisited(Neighbors, Visited, Unvisited),
		(	Unvisited == [] ->
			Path = Visited
		;	member(Next, Unvisited),
			path_extend(Next, Graph, [Next| Visited], Path)
		).

	unvisited([], _, []).
	unvisited([Neighbor| Neighbors], Visited, Result) :-
		(	member(Neighbor, Visited) ->
			unvisited(Neighbors, Visited, Result)
		;	Result = [Neighbor|Rest],
			unvisited(Neighbors, Visited, Rest)
		).

	% === Completeness ===

	is_complete(Graph) :-
		::vertices(Graph, Vertices),
		length(Vertices, N),
		N > 0,
		Expected is N - 1,
		is_complete_check(Vertices, Graph, Expected).

	is_complete_check([], _, _).
	is_complete_check([Vertex| Vertices], Graph, Expected) :-
		::neighbors(Vertex, Graph, Neighbors),
		length(Neighbors, Expected),
		is_complete_check(Vertices, Graph, Expected).

	% === Bipartiteness (BFS 2-coloring) ===

	is_bipartite(Graph) :-
		::vertices(Graph, Vertices),
		is_bipartite_components(Vertices, Graph, []).

	% Process each connected component
	is_bipartite_components([], _, _).
	is_bipartite_components([Vertex| Vertices], Graph, Colors) :-
		(	bipartite_color(Vertex, Colors, _) ->
			% Already colored by a previous component
			is_bipartite_components(Vertices, Graph, Colors)
		;	% Start BFS from Vertex with color 0
			bipartite_bfs([Vertex], Graph, [Vertex-0|Colors], NewColors),
			is_bipartite_components(Vertices, Graph, NewColors)
		).

	bipartite_bfs([], _, Colors, Colors).
	bipartite_bfs([Vertex| Queue], Graph, Colors, FinalColors) :-
		bipartite_color(Vertex, Colors, VColor),
		OtherColor is 1 - VColor,
		::neighbors(Vertex, Graph, Neighbors),
		bipartite_check_neighbors(Neighbors, OtherColor, Queue, Colors, NewQueue, NewColors),
		bipartite_bfs(NewQueue, Graph, NewColors, FinalColors).

	bipartite_check_neighbors([], _, Queue, Colors, Queue, Colors).
	bipartite_check_neighbors([Neighbor| Neighbors], ExpectedColor, Queue, Colors, FinalQueue, FinalColors) :-
		(	bipartite_color(Neighbor, Colors, NeighborColor) ->
			% Already colored: check for conflict
			NeighborColor == ExpectedColor,
			bipartite_check_neighbors(Neighbors, ExpectedColor, Queue, Colors, FinalQueue, FinalColors)
		;	% Not yet colored: assign and enqueue
			bipartite_check_neighbors(Neighbors, ExpectedColor, [Neighbor| Queue], [Neighbor-ExpectedColor| Colors], FinalQueue, FinalColors)
		).

	bipartite_color(Neighbor, [Vertex-Color| Colors], NeighborColor) :-
		(	Neighbor == Vertex ->
			NeighborColor = Color
		;	bipartite_color(Neighbor, Colors, NeighborColor)
		).

	% === Density (cutoff: |E| = |V| * log2(|V|)) ===

	is_sparse(Graph) :-
		::vertices(Graph, Vertices),
		::edges(Graph, Edges),
		length(Vertices, V),
		length(Edges, E),
		(	V =< 1 ->
			true
		;	E =< V * log(V) / log(2)
		).

	% === Edge conversion from dictionary pairs ===

	pairs_to_edges([], []).
	pairs_to_edges([Vertex-Neighbors| Pairs], Edges) :-
		vertex_neighbors_to_edges(Neighbors, Vertex, Edges, RestEdges),
		pairs_to_edges(Pairs, RestEdges).

	vertex_neighbors_to_edges([], _, Edges, Edges).
	vertex_neighbors_to_edges([Neighbor| Neighbors], Vertex, [Vertex-Neighbor| Edges], RestEdges) :-
		vertex_neighbors_to_edges(Neighbors, Vertex, Edges, RestEdges).

	% === Weighted edge conversion from dictionary pairs ===

	wpairs_to_edges([], []).
	wpairs_to_edges([Vertex-WNeighbors| Pairs], Edges) :-
		wvertex_neighbors_to_edges(WNeighbors, Vertex, Edges, RestEdges),
		wpairs_to_edges(Pairs, RestEdges).

	wvertex_neighbors_to_edges([], _, Edges, Edges).
	wvertex_neighbors_to_edges([Neighbor-Weight| WNeighbors], Vertex, [(Vertex-Neighbor)-Weight| Edges], RestEdges) :-
		wvertex_neighbors_to_edges(WNeighbors, Vertex, Edges, RestEdges).

:- end_category.
