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


:- object(tests_unweighted_undirected_graph(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-23,
		comment is 'Unit tests for the "unweighted_undirected_graph" library predicates.',
		parnames is ['DictionaryObject']
	]).

	:- uses(unweighted_undirected_graph(_DictionaryObject_), [
		new/1, new/2, new/3,
		empty/1,
		edges/2, vertices/2,
		add_vertex/3, add_vertices/3,
		delete_vertex/3, delete_vertices/3,
		edge/3, add_edge/4,
		add_edges/3, delete_edge/4, delete_edges/3,
		neighbors/3, reachable/3,
		complement/2,
		number_of_vertices/2, number_of_edges/2,
		path/3, has_path/3,
		min_path/5, max_path/5,
		degree/3,
		is_connected/1,
		connected_components/2,
		is_complete/1,
		is_bipartite/1,
		is_sparse/1,
		is_tree/1,
		graph_coloring/3,
		maximal_cliques/2,
		maximum_cliques/2
	]).

	cover(unweighted_undirected_graph(_DictionaryObject_)).
	cover(unweighted_graph_common(_DictionaryObject_)).
	cover(undirected_graph_common).

	% new/1 tests

	test(uug_new_1_01, true) :-
		new(Graph),
		empty(Graph).

	% new/2 tests

	test(uug_new_2_01, true(nonvar(Graph))) :-
		new([1-2,1-3], Graph).

	% new/3 tests

	test(uug_new_3_01, true(nonvar(Graph))) :-
		new([1,2,3], [1-2], Graph).

	% empty/1 tests

	test(uug_empty_1_01, true) :-
		new(Graph),
		empty(Graph).

	test(uug_empty_1_02, false) :-
		new([1,2,3], [1-2], Graph),
		empty(Graph).

	% vertices/2 tests

	test(uug_vertices_2_01, true(Vertices == [])) :-
		new(Graph),
		vertices(Graph, Vertices).

	test(uug_vertices_2_02, true(Vertices == [1,2,3])) :-
		new([1,2,3], [1-2], Graph),
		vertices(Graph, Vertices).

	% edges/2 tests (each undirected edge returned once)

	test(uug_edges_2_01, true(Edges == [1-2,1-3])) :-
		new([1,2,3], [1-2,1-3], Graph),
		edges(Graph, Edges).

	test(uug_edges_2_02, true(Edges == [])) :-
		new([1,2,3], [], Graph),
		edges(Graph, Edges).

	% edge/3 tests (symmetric)

	test(uug_edge_3_01, true) :-
		new([1-2], Graph),
		edge(1, 2, Graph).

	test(uug_edge_3_02, true) :-
		new([1-2], Graph),
		edge(2, 1, Graph).

	test(uug_edge_3_03, false) :-
		new([1-2], Graph),
		edge(1, 3, Graph).

	% add_edge/4 tests (both directions)

	test(uug_add_edge_4_01, true(Edges == [1-2,1-3])) :-
		new([1-2], Graph),
		add_edge(Graph, 1, 3, NewGraph),
		edges(NewGraph, Edges).

	test(uug_add_edge_4_02, true) :-
		new([1-2], Graph),
		add_edge(Graph, 1, 3, NewGraph),
		edge(3, 1, NewGraph).
	% add_vertex/3 tests

	test(uug_add_vertex_3_01, true(Vertices == [1,2,3])) :-
		new([1-2], Graph),
		add_vertex(Graph, 3, NewGraph),
		vertices(NewGraph, Vertices).

	test(uug_add_vertex_3_02, true(Vertices == [1,2])) :-
		new([1-2], Graph),
		add_vertex(Graph, 1, NewGraph),
		vertices(NewGraph, Vertices).
	% add_vertices/3 tests

	test(uug_add_vertices_3_01, true(Vertices == [1,2,3,4])) :-
		new([1,2], [], Graph),
		add_vertices(Graph, [3,4], NewGraph),
		vertices(NewGraph, Vertices).

	% delete_vertex/3 tests

	test(uug_delete_vertex_3_01, true((Vertices == [1,3], Edges == []))) :-
		new([1,2,3], [1-2,2-3], Graph),
		delete_vertex(Graph, 2, NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% delete_vertices/3 tests

	test(uug_delete_vertices_3_01, true(Vertices == [3,4])) :-
		new([1,2,3,4], [1-2,2-3,3-4], Graph),
		delete_vertices(Graph, [1,2], NewGraph),
		vertices(NewGraph, Vertices).

	% delete_edge/4 tests

	test(uug_delete_edge_4_01, true(Edges == [1-3])) :-
		new([1-2,1-3], Graph),
		delete_edge(Graph, 1, 2, NewGraph),
		edges(NewGraph, Edges).

	test(uug_delete_edge_4_02, false) :-
		new([1-2,1-3], Graph),
		delete_edge(Graph, 1, 2, NewGraph),
		edge(2, 1, NewGraph).

	% delete_edges/3 tests

	test(uug_delete_edges_3_01, true(Edges == [1-3])) :-
		new([1-2,1-3,2-3], Graph),
		delete_edges(Graph, [1-2,2-3], NewGraph),
		edges(NewGraph, Edges).

	% add_edges/3 tests

	test(uug_add_edges_3_01, true(Edges == [1-2,1-3,2-3])) :-
		new([1-2], Graph),
		add_edges(Graph, [1-3,2-3], NewGraph),
		edges(NewGraph, Edges).

	test(uug_add_edges_3_02, true(Edges == [1-2,1-3])) :-
		new([1-2], Graph),
		add_edges(Graph, [1-3,1-2], NewGraph),
		edges(NewGraph, Edges).

	% neighbors/3 tests (exclude self)

	test(uug_neighbors_3_01, true(Neighbors == [1,3])) :-
		new([1,2,3], [1-2,2-3], Graph),
		neighbors(2, Graph, Neighbors).

	test(uug_neighbors_3_02, false) :-
		new([1,2,3], [1-2], Graph),
		neighbors(42, Graph, _).

	% reachable/3 tests

	test(uug_reachable_3_01, true(Vertices == [1,2,3])) :-
		new([1,2,3,4], [1-2,2-3], Graph),
		reachable(1, Graph, Vertices).

	test(uug_reachable_3_02, false) :-
		new([1,2,3], [1-2], Graph),
		reachable(42, Graph, _).

	% complement/2 tests

	test(uug_complement_2_01, true(Edges == [1-3,2-3])) :-
		new([1,2,3], [1-2], Graph),
		complement(Graph, NewGraph),
		edges(NewGraph, Edges).

	% number_of_vertices/2 tests

	test(uug_number_of_vertices_2_01, true(N == 0)) :-
		new(Graph),
		number_of_vertices(Graph, N).

	test(uug_number_of_vertices_2_02, true(N == 3)) :-
		new([1,2,3], [1-2], Graph),
		number_of_vertices(Graph, N).

	% number_of_edges/2 tests

	test(uug_number_of_edges_2_01, true(N == 0)) :-
		new(Graph),
		number_of_edges(Graph, N).

	test(uug_number_of_edges_2_02, true(N == 2)) :-
		new([1,2,3], [1-2,1-3], Graph),
		number_of_edges(Graph, N).

	% path/3 tests

	test(uug_path_3_01, true(Path == [1,2,3])) :-
		new([1-2,2-3], Graph),
		path(1, Graph, Path).

	test(uug_path_3_02, false) :-
		new([1-2,3-4], Graph),
		path(42, Graph, _).

	% has_path/3 tests

	test(uug_has_path_3_01, true) :-
		new([1-2,2-3], Graph),
		has_path(1, 3, Graph).

	test(uug_has_path_3_02, true) :-
		new([1-2,2-3], Graph),
		has_path(3, 1, Graph).

	test(uug_has_path_3_03, false) :-
		new([1-2,3-4], Graph),
		has_path(1, 4, Graph).

	% min_path/5 tests

	test(uug_min_path_5_01, true((Path == [1,3], Cost == 1))) :-
		new([1-2,2-3,1-3], Graph),
		min_path(1, 3, Graph, Path, Cost).

	test(uug_min_path_5_02, true((Path == [1], Cost == 0))) :-
		new([1-2,2-3], Graph),
		min_path(1, 1, Graph, Path, Cost).

	test(uug_min_path_5_03, false) :-
		new([1-2,3-4], Graph),
		min_path(1, 4, Graph, _, _).

	% max_path/5 tests

	test(uug_max_path_5_01, true((Path == [1,2,3], Cost == 2))) :-
		new([1-2,2-3,1-3], Graph),
		max_path(1, 3, Graph, Path, Cost).

	test(uug_max_path_5_02, true((Path == [1], Cost == 0))) :-
		new([1-2], Graph),
		max_path(1, 1, Graph, Path, Cost).

	test(uug_max_path_5_03, false) :-
		new([1-2,3-4], Graph),
		max_path(1, 4, Graph, _, _).

	% degree/3 tests

	test(uug_degree_3_01, true(D == 2)) :-
		new([1-2,1-3], Graph),
		degree(1, Graph, D).

	test(uug_degree_3_02, true(D == 1)) :-
		new([1-2,1-3], Graph),
		degree(2, Graph, D).

	test(uug_degree_3_03, true(D == 0)) :-
		new([1,2,3], [], Graph),
		degree(1, Graph, D).

	% is_connected/1 tests

	test(uug_is_connected_1_01, true) :-
		new([1-2,2-3], Graph),
		is_connected(Graph).

	test(uug_is_connected_1_02, false) :-
		new([1-2,3-4], Graph),
		is_connected(Graph).

	% connected_components/2 tests

	test(uug_connected_components_2_01, subsumes([_], Components)) :-
		new([1-2,2-3], Graph),
		connected_components(Graph, Components).

	test(uug_connected_components_2_02, subsumes([_, _], Components)) :-
		new([1-2,3-4], Graph),
		connected_components(Graph, Components).

	% is_complete/1 tests

	test(uug_is_complete_1_01, true) :-
		new([1-2, 1-3, 2-3], Graph),
		is_complete(Graph).

	test(uug_is_complete_1_02, false) :-
		new([1-2, 2-3], Graph),
		is_complete(Graph).

	test(uug_is_complete_1_03, true) :-
		new([1-2], Graph),
		is_complete(Graph).

	% is_bipartite/1 tests

	test(uug_is_bipartite_1_01, true) :-
		new([1-2, 1-4, 3-2, 3-4], Graph),
		is_bipartite(Graph).

	test(uug_is_bipartite_1_02, false) :-
		new([1-2, 2-3, 1-3], Graph),
		is_bipartite(Graph).

	test(uug_is_bipartite_1_03, true) :-
		new(Graph),
		is_bipartite(Graph).

	test(uug_is_bipartite_1_04, true) :-
		new([1-2, 3-4], Graph),
		is_bipartite(Graph).

	% is_sparse/1 tests

	test(uug_is_sparse_1_01, true) :-
		% 4 vertices, 3 edges =< 4*log2(4) = 8
		new([1-2, 2-3, 3-4], Graph),
		is_sparse(Graph).

	test(uug_is_sparse_1_02, false) :-
		% K7: 7 vertices, 21 edges > 7*log2(7) ~= 19.65
		new([1-2,1-3,1-4,1-5,1-6,1-7,2-3,2-4,2-5,2-6,2-7,3-4,3-5,3-6,3-7,4-5,4-6,4-7,5-6,5-7,6-7], Graph),
		is_sparse(Graph).

	test(uug_is_sparse_1_03, true) :-
		new(Graph),
		is_sparse(Graph).

	% is_tree/1 tests

	test(uug_is_tree_1_01, true) :-
		% path graph: 3 vertices, 2 edges, connected
		new([1-2, 2-3], Graph),
		is_tree(Graph).

	test(uug_is_tree_1_02, false) :-
		% cycle: 3 vertices, 3 edges, connected but too many edges
		new([1-2, 2-3, 3-1], Graph),
		is_tree(Graph).

	test(uug_is_tree_1_03, false) :-
		% forest: 4 vertices, 2 edges, not connected
		new([1-2, 3-4], Graph),
		is_tree(Graph).

	test(uug_is_tree_1_04, true) :-
		% single vertex
		new([1], [], Graph),
		is_tree(Graph).

	test(uug_is_tree_1_05, false) :-
		% empty graph
		new(Graph),
		is_tree(Graph).

	% graph_coloring/3 tests

	test(uug_graph_coloring_3_01, true(NumberOfColors == 3)) :-
		% triangle requires 3 colors
		new([1-2, 2-3, 3-1], Graph),
		graph_coloring(Graph, _Coloring, NumberOfColors).

	test(uug_graph_coloring_3_02, true(NumberOfColors == 2)) :-
		% 4-cycle is bipartite, needs 2 colors
		new([1-2, 2-3, 3-4, 4-1], Graph),
		graph_coloring(Graph, _Coloring, NumberOfColors).

	test(uug_graph_coloring_3_03, true(NumberOfColors == 1)) :-
		% isolated vertices need 1 color
		new([1, 2, 3], [], Graph),
		graph_coloring(Graph, _Coloring, NumberOfColors).

	test(uug_graph_coloring_3_04, true(NumberOfColors == 0)) :-
		% empty graph needs 0 colors
		new(Graph),
		graph_coloring(Graph, _Coloring, NumberOfColors).

	test(uug_graph_coloring_3_05, true(NumberOfColors == 2)) :-
		% path graph is bipartite
		new([1-2, 2-3, 3-4], Graph),
		graph_coloring(Graph, _Coloring, NumberOfColors).

	% maximal_cliques/2 tests

	test(uug_maximal_cliques_2_01, true(Cliques == [])) :-
		% empty graph has no cliques
		new(Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_02, true(Cliques == [[1],[2],[3]])) :-
		% isolated vertices: each is a maximal clique of size 1
		new([1,2,3], [], Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_03, true(Cliques == [[1,2]])) :-
		% single edge: one maximal clique
		new([1-2], Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_04, true(Cliques == [[1,2,3]])) :-
		% triangle: one maximal clique of size 3
		new([1-2, 2-3, 1-3], Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_05, true(Cliques == [[1,2],[2,3]])) :-
		% path 1-2-3: two maximal cliques {1,2} and {2,3}
		new([1-2, 2-3], Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_06, true(Cliques == [[1,2,3],[2,3,4]])) :-
		% diamond without 1-4: cliques are {1,2,3} and {2,3,4}
		new([1-2, 1-3, 2-3, 2-4, 3-4], Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_07, true(Cliques == [[1,2,3,4]])) :-
		% complete graph K4
		new([1-2, 1-3, 1-4, 2-3, 2-4, 3-4], Graph),
		maximal_cliques(Graph, Cliques).

	test(uug_maximal_cliques_2_08, true(Cliques == [[1,2,3],[4]])) :-
		% triangle plus isolated vertex
		new([1,2,3,4], [1-2, 2-3, 1-3], Graph),
		maximal_cliques(Graph, Cliques).

	% maximum_cliques/2 tests

	test(uug_maximum_cliques_2_01, true(Cliques == [])) :-
		% empty graph
		new(Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_02, true(Cliques == [[1],[2],[3]])) :-
		% isolated vertices: all are maximum (size 1)
		new([1,2,3], [], Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_03, true(Cliques == [[1,2,3]])) :-
		% triangle: one maximum clique
		new([1-2, 2-3, 1-3], Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_04, true(Cliques == [[1,2],[2,3]])) :-
		% path 1-2-3: both maximal cliques are maximum (size 2)
		new([1-2, 2-3], Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_05, true(Cliques == [[1,2,3],[2,3,4]])) :-
		% diamond without 1-4: two maximum cliques of size 3
		new([1-2, 1-3, 2-3, 2-4, 3-4], Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_06, true(Cliques == [[1,2,3,4]])) :-
		% complete graph K4: the whole graph is the only maximum clique
		new([1-2, 1-3, 1-4, 2-3, 2-4, 3-4], Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_07, true(Cliques == [[1,2,3]])) :-
		% triangle plus isolated vertex: maximum clique is triangle
		new([1,2,3,4], [1-2, 2-3, 1-3], Graph),
		maximum_cliques(Graph, Cliques).

	test(uug_maximum_cliques_2_08, true(Cliques == [[1,2,3],[4,5,6]])) :-
		% two disjoint triangles: two maximum cliques of size 3
		new([1-2, 2-3, 1-3, 4-5, 5-6, 4-6], Graph),
		maximum_cliques(Graph, Cliques).

:- end_object.
