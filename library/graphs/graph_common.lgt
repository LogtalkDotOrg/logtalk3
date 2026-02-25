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
		date is 2026-02-25,
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
		add_vertices(Empty, Vertices, Graph0),
		::add_edges(Graph0, Edges, Graph).

	% === Vertex list operations ===

	add_vertices(NewGraph, [], NewGraph).
	add_vertices(Graph, [Vertex| Vertices], NewGraph) :-
		::add_vertex(Graph, Vertex, NewGraph0),
		add_vertices(NewGraph0, Vertices, NewGraph).

	delete_vertices(NewGraph, [], NewGraph).
	delete_vertices(Graph, [Vertex| Vertices], NewGraph) :-
		::delete_vertex(Graph, Vertex, NewGraph0),
		delete_vertices(NewGraph0, Vertices, NewGraph).

	% === Graph metrics ===

	number_of_vertices(Graph, N) :-
		::vertices(Graph, Vertices),
		length(Vertices, N).

	number_of_edges(Graph, N) :-
		::edges(Graph, Edges),
		length(Edges, N).

	% === Reachability ===

	reachable(Vertex, Graph, Vertices) :-
		::neighbors(Vertex, Graph, _),
		reachable_aux([Vertex], Graph, [Vertex], Vertices).

	breadth_first_order(Vertex, Graph, Vertices) :-
		::neighbors(Vertex, Graph, _),
		breadth_first_order_aux([Vertex], Graph, [Vertex], [], ReverseVertices),
		reverse(ReverseVertices, Vertices).

	depth_first_order(Vertex, Graph, Vertices) :-
		::neighbors(Vertex, Graph, _),
		depth_first_order_aux([Vertex], Graph, [Vertex], [], ReverseVertices),
		reverse(ReverseVertices, Vertices).

	reachable_aux([], _, Visited, Visited).
	reachable_aux([Vertex| Vertices], Graph, Visited, Reachable) :-
		(	::neighbors(Vertex, Graph, Neighbors) ->
			set_union(Visited, Neighbors, NewVisited, NewVertices),
			append(Vertices, NewVertices, NextToVisit)
		;	NewVisited = Visited,
			NextToVisit = Vertices
		),
		reachable_aux(NextToVisit, Graph, NewVisited, Reachable).

	breadth_first_order_aux([], _, _, ReverseVertices, ReverseVertices).
	breadth_first_order_aux([Vertex| Queue], Graph, Visited, ReverseVertices0, ReverseVertices) :-
		::neighbors(Vertex, Graph, Neighbors),
		bfs_collect_unvisited(Neighbors, Visited, [], NewVertices, NewVisited),
		append(Queue, NewVertices, NextQueue),
		breadth_first_order_aux(NextQueue, Graph, NewVisited, [Vertex| ReverseVertices0], ReverseVertices).

	bfs_collect_unvisited([], Visited, NewVertices, NewVertices, Visited).
	bfs_collect_unvisited([Neighbor| Neighbors], Visited0, NewVertices0, NewVertices, Visited) :-
		(	member(Neighbor, Visited0) ->
			bfs_collect_unvisited(Neighbors, Visited0, NewVertices0, NewVertices, Visited)
		;	append(NewVertices0, [Neighbor], NewVertices1),
			bfs_collect_unvisited(Neighbors, [Neighbor| Visited0], NewVertices1, NewVertices, Visited)
		).

	depth_first_order_aux([], _, _, ReverseVertices, ReverseVertices).
	depth_first_order_aux([Vertex| Stack], Graph, Visited, ReverseVertices0, ReverseVertices) :-
		::neighbors(Vertex, Graph, Neighbors),
		reverse(Neighbors, ReversedNeighbors),
		dfs_push_unvisited(ReversedNeighbors, Visited, Stack, NextStack, NewVisited),
		depth_first_order_aux(NextStack, Graph, NewVisited, [Vertex| ReverseVertices0], ReverseVertices).

	dfs_push_unvisited([], Visited, Stack, Stack, Visited).
	dfs_push_unvisited([Neighbor| Neighbors], Visited0, Stack0, Stack, Visited) :-
		(	member(Neighbor, Visited0) ->
			dfs_push_unvisited(Neighbors, Visited0, Stack0, Stack, Visited)
		;	dfs_push_unvisited(Neighbors, [Neighbor| Visited0], [Neighbor| Stack0], Stack, Visited)
		).

	% === Path existence ===

	has_path(Vertex1, Vertex2, Graph) :-
		reachable(Vertex1, Graph, Reachable),
		set_memberchk(Vertex2, Reachable).

	min_distances(Vertex, Graph, Distances) :-
		::neighbors(Vertex, Graph, _),
		::vertices(Graph, Vertices),
		min_distances_from_vertices(Vertices, Vertex, Graph, Distances).

	min_predecessors(Vertex, Graph, Predecessors) :-
		::neighbors(Vertex, Graph, _),
		::vertices(Graph, Vertices),
		min_predecessors_from_vertices(Vertices, Vertex, Graph, Predecessors).

	all_pairs_min_paths(Graph, Pairs) :-
		::vertices(Graph, Vertices),
		all_pairs_min_paths_sources(Vertices, Graph, Pairs).

	all_pairs_min_predecessors(Graph, Pairs) :-
		::vertices(Graph, Vertices),
		all_pairs_min_predecessors_sources(Vertices, Graph, Pairs).

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

	min_distances_from_vertices([], _, _, []).
	min_distances_from_vertices([Target| Targets], Source, Graph, [Target-Cost| Distances]) :-
		::min_path(Source, Target, Graph, _, Cost),
		!,
		min_distances_from_vertices(Targets, Source, Graph, Distances).
	min_distances_from_vertices([_| Targets], Source, Graph, Distances) :-
		min_distances_from_vertices(Targets, Source, Graph, Distances).

	min_predecessors_from_vertices([], _, _, []).
	min_predecessors_from_vertices([Target| Targets], Source, Graph, [Target-Predecessor| Predecessors]) :-
		::min_path(Source, Target, Graph, Path, _),
		!,
		path_predecessor(Path, Source, Predecessor),
		min_predecessors_from_vertices(Targets, Source, Graph, Predecessors).
	min_predecessors_from_vertices([_| Targets], Source, Graph, Predecessors) :-
		min_predecessors_from_vertices(Targets, Source, Graph, Predecessors).

	path_predecessor([Source], Source, none) :-
		!.
	path_predecessor([First,Second], _, First) :-
		Second \== none,
		!.
	path_predecessor([_| Tail], Source, Predecessor) :-
		path_predecessor(Tail, Source, Predecessor).

	all_pairs_min_paths_sources([], _, []).
	all_pairs_min_paths_sources([Source| Sources], Graph, Pairs) :-
		::min_distances(Source, Graph, Distances),
		tag_distances(Source, Distances, SourcePairs),
		all_pairs_min_paths_sources(Sources, Graph, RestPairs),
		append(SourcePairs, RestPairs, Pairs).

	tag_distances(_, [], []).
	tag_distances(Source, [Target-Cost| Distances], [((Source-Target)-Cost)| Pairs]) :-
		tag_distances(Source, Distances, Pairs).

	all_pairs_min_predecessors_sources([], _, []).
	all_pairs_min_predecessors_sources([Source| Sources], Graph, Pairs) :-
		::min_predecessors(Source, Graph, Predecessors),
		tag_predecessors(Source, Predecessors, SourcePairs),
		all_pairs_min_predecessors_sources(Sources, Graph, RestPairs),
		append(SourcePairs, RestPairs, Pairs).

	tag_predecessors(_, [], []).
	tag_predecessors(Source, [Target-Predecessor| Predecessors], [((Source-Target)-Predecessor)| Pairs]) :-
		tag_predecessors(Source, Predecessors, Pairs).

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
