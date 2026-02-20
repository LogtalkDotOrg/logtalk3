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


:- object(tests_weighted_directed_graph(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Unit tests for the "weighted_directed_graph" library predicates.',
		parnames is ['DictionaryObject']
	]).

	:- uses(weighted_directed_graph(_DictionaryObject_), [
		new/1, new/2, new/3,
		empty/1,
		edges/2, vertices/2,
		add_vertex/3, add_vertices/3,
		delete_vertex/3, delete_vertices/3,
		edge/4, add_edge/5,
		add_edges/3, delete_edge/5, delete_edges/3,
		neighbors/3, wneighbors/3,
		reachable/3,
		transpose/2,
		transitive_closure/2,
		symmetric_closure/2,
		topological_sort/2,
		min_path/5, max_path/5,
		min_paths/3,
		number_of_vertices/2, number_of_edges/2,
		path/3, has_path/3,
		in_degree/3, out_degree/3,
		is_acyclic/1,
		strongly_connected_components/2,
		is_complete/1,
		is_bipartite/1,
		is_sparse/1
	]).

	cover(weighted_directed_graph(_DictionaryObject_)).
	cover(weighted_graph_common(_DictionaryObject_)).

	% new/1 tests

	test(wdg_new_1_01, true) :-
		new(Graph),
		empty(Graph).

	% new/2 tests

	test(wdg_new_2_01, true(nonvar(Graph))) :-
		new([(1-2)-5, (1-3)-10], Graph).

	% new/3 tests

	test(wdg_new_3_01, true(nonvar(Graph))) :-
		new([4], [(1-2)-5, (1-3)-10], Graph).

	% vertices/2 tests

	test(wdg_vertices_2_01, true(Vertices == [1,2,3])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		vertices(Graph, Vertices).

	test(wdg_vertices_2_02, true(Vertices == [1,2,3,4])) :-
		new([4], [(1-2)-5, (1-3)-10], Graph),
		vertices(Graph, Vertices).

	% edges/2 tests

	test(wdg_edges_2_01, true(Edges == [(1-2)-5,(1-3)-10])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		edges(Graph, Edges).

	test(wdg_edges_2_02, true(Edges == [])) :-
		new([1,2], [], Graph),
		edges(Graph, Edges).

	% edge/4 tests

	test(wdg_edge_4_01, true(W == 5)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		edge(1, 2, W, Graph).

	test(wdg_edge_4_02, false) :-
		new([(1-2)-5], Graph),
		edge(2, 1, _, Graph).

	% add_edge/5 tests

	test(wdg_add_edge_5_01, true(W == 7)) :-
		new([(1-2)-5], Graph),
		add_edge(Graph, 2, 3, 7, NewGraph),
		edge(2, 3, W, NewGraph).

	% add_edges/3 tests

	test(wdg_add_edges_3_01, true(Edges == [(1-2)-5,(1-3)-10,(2-3)-7])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		add_edges(Graph, [(2-3)-7], NewGraph),
		edges(NewGraph, Edges).

	% delete_edge/5 tests

	test(wdg_delete_edge_5_01, true((Edges == [(1-3)-10], W == 5))) :-
		new([(1-2)-5, (1-3)-10], Graph),
		delete_edge(Graph, 1, 2, W, NewGraph),
		edges(NewGraph, Edges).

	% delete_edges/3 tests

	test(wdg_delete_edges_3_01, true(Edges == [(1-3)-10])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		delete_edges(Graph, [(1-2)-5], NewGraph),
		edges(NewGraph, Edges).

	% add_vertex/3 tests

	test(wdg_add_vertex_3_01, true(Vertices == [1,2,3,4])) :-
		new([(1-2)-5], Graph),
		add_vertex(Graph, 3, G1),
		add_vertex(G1, 4, NewGraph),
		vertices(NewGraph, Vertices).

	% add_vertices/3 tests

	test(wdg_add_vertices_3_01, true(Vertices == [1,2,3,4])) :-
		new([(1-2)-5], Graph),
		add_vertices(Graph, [3,4], NewGraph),
		vertices(NewGraph, Vertices).

	test(wdg_add_vertices_3_02, true(Vertices == [1,2])) :-
		new([(1-2)-5], Graph),
		add_vertices(Graph, [1,2], NewGraph),
		vertices(NewGraph, Vertices).

	% delete_vertex/3 tests

	test(wdg_delete_vertex_3_01, true((Vertices == [1,3], Edges == [(1-3)-10]))) :-
		new([(1-2)-5, (1-3)-10, (2-3)-7], Graph),
		delete_vertex(Graph, 2, NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% delete_vertices/3 tests

	test(wdg_delete_vertices_3_01, true(Vertices == [3])) :-
		new([(1-2)-5, (1-3)-10, (2-3)-7], Graph),
		delete_vertices(Graph, [1,2], NewGraph),
		vertices(NewGraph, Vertices).

	% neighbors/3 tests (without weights)

	test(wdg_neighbors_3_01, true(Neighbors == [2,3])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		neighbors(1, Graph, Neighbors).

	test(wdg_neighbors_3_02, false) :-
		new([(1-2)-5], Graph),
		neighbors(42, Graph, _).

	% wneighbors/3 tests (with weights)

	test(wdg_wneighbors_3_01, true(WNeighbors == [2-5, 3-10])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		wneighbors(1, Graph, WNeighbors).

	% reachable/3 tests

	test(wdg_reachable_3_01, true(Vertices == [1,2,3])) :-
		new([(1-2)-5, (2-3)-10], Graph),
		reachable(1, Graph, Vertices).

	test(wdg_reachable_3_02, false) :-
		new([(1-2)-5], Graph),
		reachable(42, Graph, _).

	% transpose/2 tests

	test(wdg_transpose_2_01, true(Edges == [(2-1)-5, (3-1)-10])) :-
		new([(1-2)-5, (1-3)-10], Graph),
		transpose(Graph, NewGraph),
		edges(NewGraph, Edges).

	% topological_sort/2 tests

	test(wdg_topological_sort_2_01, true(Sorted == [1,2,3])) :-
		new([(1-2)-5, (2-3)-10], Graph),
		topological_sort(Graph, Sorted).

	% min_path/5 tests

	test(wdg_min_path_5_01, true((Path == [1,2,3], Cost == 15))) :-
		new([(1-2)-5, (1-3)-20, (2-3)-10], Graph),
		min_path(1, 3, Graph, Path, Cost).

	test(wdg_min_path_5_02, true((Path == [1,3], Cost == 3))) :-
		new([(1-2)-5, (1-3)-3, (2-3)-10], Graph),
		min_path(1, 3, Graph, Path, Cost).

	test(wdg_min_path_5_03, false) :-
		new([(1-2)-5, (3-4)-10], Graph),
		min_path(1, 4, Graph, _, _).

	% edge/4 additional test (wfind lookup past first neighbor)

	test(wdg_edge_4_03, true(W == 10)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		edge(1, 3, W, Graph).

	% max_path/5 tests

	test(wdg_max_path_5_01, true((Path == [1,3], Cost == 20))) :-
		new([(1-2)-5, (1-3)-20, (2-3)-10], Graph),
		max_path(1, 3, Graph, Path, Cost).

	% transitive_closure/2 tests

	test(wdg_transitive_closure_2_01, true(Edges == [(1-2)-5,(1-3)-15,(2-3)-10])) :-
		new([(1-2)-5, (2-3)-10], Graph),
		transitive_closure(Graph, Closure),
		edges(Closure, Edges).

	test(wdg_transitive_closure_2_02, true(Edges == [(1-2)-5,(1-3)-10,(2-3)-10])) :-
		new([(1-2)-5, (1-3)-10, (2-3)-10], Graph),
		transitive_closure(Graph, Closure),
		edges(Closure, Edges).

	% symmetric_closure/2 tests

	test(wdg_symmetric_closure_2_01, true(Edges == [(1-2)-5, (2-1)-5, (2-3)-10, (3-2)-10])) :-
		new([(1-2)-5, (2-3)-10], Graph),
		symmetric_closure(Graph, Closure),
		edges(Closure, Edges).

	% min_paths/3 tests

	test(wdg_min_paths_3_01, true(Edges == [(1-2)-1, (1-3)-1])) :-
		new([(1-2)-5, (1-3)-3, (2-3)-10], Graph),
		min_paths(1, Graph, PathTree),
		edges(PathTree, Edges).

	test(wdg_min_paths_3_02, true(Edges == [(1-2)-1, (2-3)-1])) :-
		new([(1-2)-5, (1-3)-20, (2-3)-10], Graph),
		min_paths(1, Graph, PathTree),
		edges(PathTree, Edges).

	% number_of_vertices/2 tests

	test(wdg_number_of_vertices_2_01, true(N == 0)) :-
		new(Graph),
		number_of_vertices(Graph, N).

	test(wdg_number_of_vertices_2_02, true(N == 3)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		number_of_vertices(Graph, N).

	% number_of_edges/2 tests

	test(wdg_number_of_edges_2_01, true(N == 0)) :-
		new(Graph),
		number_of_edges(Graph, N).

	test(wdg_number_of_edges_2_02, true(N == 2)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		number_of_edges(Graph, N).

	% path/3 tests

	test(wdg_path_3_01, true(Path == [1,2,3])) :-
		new([(1-2)-5, (2-3)-10], Graph),
		path(1, Graph, Path).

	test(wdg_path_3_02, false) :-
		new([(1-2)-5, (2-3)-10], Graph),
		path(42, Graph, _).

	% has_path/3 tests

	test(wdg_has_path_3_01, true) :-
		new([(1-2)-5, (2-3)-10], Graph),
		has_path(1, 3, Graph).

	test(wdg_has_path_3_02, false) :-
		new([(1-2)-5, (2-3)-10], Graph),
		has_path(3, 1, Graph).

	test(wdg_has_path_3_03, true) :-
		new([(1-2)-5, (2-3)-10], Graph),
		has_path(1, 1, Graph).

	% in_degree/3 tests

	test(wdg_in_degree_3_01, true(D == 2)) :-
		new([(1-3)-5, (2-3)-10, (3-4)-7], Graph),
		in_degree(3, Graph, D).

	test(wdg_in_degree_3_02, true(D == 0)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		in_degree(1, Graph, D).

	% out_degree/3 tests

	test(wdg_out_degree_3_01, true(D == 2)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		out_degree(1, Graph, D).

	test(wdg_out_degree_3_02, true(D == 0)) :-
		new([(1-2)-5, (1-3)-10], Graph),
		out_degree(3, Graph, D).

	% is_acyclic/1 tests

	test(wdg_is_acyclic_1_01, true) :-
		new([(1-2)-5, (2-3)-10], Graph),
		is_acyclic(Graph).

	test(wdg_is_acyclic_1_02, false) :-
		new([(1-2)-5, (2-3)-10, (3-1)-7], Graph),
		is_acyclic(Graph).

	% strongly_connected_components/2 tests

	test(wdg_strongly_connected_components_2_01, true(Components == [[1],[2],[3]])) :-
		new([(1-2)-5, (2-3)-10], Graph),
		strongly_connected_components(Graph, Components).

	test(wdg_strongly_connected_components_2_02, true(Components == [[1,2,3]])) :-
		new([(1-2)-5, (2-3)-10, (3-1)-7], Graph),
		strongly_connected_components(Graph, Components).

	% is_complete/1 tests

	test(wdg_is_complete_1_01, true) :-
		new([(1-2)-5, (2-1)-3, (1-3)-10, (3-1)-7, (2-3)-4, (3-2)-6], Graph),
		is_complete(Graph).

	test(wdg_is_complete_1_02, false) :-
		new([(1-2)-5, (2-3)-10], Graph),
		is_complete(Graph).

	test(wdg_is_complete_1_03, true) :-
		new([(1-2)-5, (2-1)-3], Graph),
		is_complete(Graph).

	% is_bipartite/1 tests

	test(wdg_is_bipartite_1_01, true) :-
		new([(1-2)-5, (2-1)-3, (1-4)-10, (4-1)-7, (3-2)-4, (2-3)-6, (3-4)-8, (4-3)-2], Graph),
		is_bipartite(Graph).

	test(wdg_is_bipartite_1_02, false) :-
		new([(1-2)-5, (2-3)-10, (3-1)-7], Graph),
		is_bipartite(Graph).

	test(wdg_is_bipartite_1_03, true) :-
		new(Graph),
		is_bipartite(Graph).

	% is_sparse/1 tests

	test(wdg_is_sparse_1_01, true) :-
		new([(1-2)-5, (2-3)-10, (3-4)-7], Graph),
		is_sparse(Graph).

	test(wdg_is_sparse_1_02, false) :-
		new([(1-2)-1,(1-3)-1,(1-4)-1,(2-1)-1,(2-3)-1,(2-4)-1,(3-1)-1,(3-2)-1,(3-4)-1,(4-1)-1,(4-2)-1,(4-3)-1], Graph),
		is_sparse(Graph).

	test(wdg_is_sparse_1_03, true) :-
		new(Graph),
		is_sparse(Graph).

:- end_object.
