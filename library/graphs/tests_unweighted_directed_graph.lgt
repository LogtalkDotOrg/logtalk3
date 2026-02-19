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


:- object(tests_unweighted_directed_graph(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Unit tests for the "unweighted_directed_graph" library predicates.',
		parnames is ['DictionaryObject']
	]).

	:- uses(unweighted_directed_graph(_DictionaryObject_), [
		new/1, new/2, new/3,
		empty/1,
		edges/2, vertices/2,
		add_vertex/3, add_vertices/3,
		delete_vertex/3, delete_vertices/3,
		edge/3, add_edge/4,
		add_edges/3, delete_edge/4, delete_edges/3,
		neighbors/3, reachable/3,
		complement/2, compose/3, union/3, transpose/2,
		transitive_closure/2, symmetric_closure/2,
		topological_sort/2, topological_sort/3,
		leaves/2,
		number_of_vertices/2, number_of_edges/2,
		path/3, has_path/3,
		min_path/5, max_path/5,
		in_degree/3, out_degree/3,
		is_dag/1,
		strongly_connected_components/2,
		transitive_reduction/2
	]).

	cover(unweighted_directed_graph(_DictionaryObject_)).

	% new/1 tests

	test(udg_new_1_01, true) :-
		new(Graph),
		empty(Graph).

	% new/2 tests

	test(udg_new_2_01, true(nonvar(Graph))) :-
		new([1-2,1-3], Graph).

	% new/3 tests

	test(udg_new_3_01, true(nonvar(Graph))) :-
		new([1,2,4], [1-2,1-3], Graph).

	test(udg_new_3_02, true(nonvar(Graph))) :-
		new([], [1-2,1-3], Graph).

	test(udg_new_3_03, true(nonvar(Graph))) :-
		new([1,2,3], [], Graph).

	% empty/1 tests

	test(udg_empty_1_01, true) :-
		new(Graph),
		empty(Graph).

	test(udg_empty_1_02, false) :-
		new([1,2,3], [1-2,1-3], Graph),
		empty(Graph).

	% vertices/2 tests

	test(udg_vertices_2_01, true(Vertices == [])) :-
		new(Graph),
		vertices(Graph, Vertices).

	test(udg_vertices_2_02, true(Vertices == [1,2,3])) :-
		new([1,2,3], [1-2,1-3], Graph),
		vertices(Graph, Vertices).

	test(udg_vertices_2_03, true(Vertices == [1,2,3])) :-
		new([1-2,1-3], Graph),
		vertices(Graph, Vertices).

	test(udg_vertices_2_04, true(Vertices == [1,2,3])) :-
		new([1,2,3], [], Graph),
		vertices(Graph, Vertices).

	% edges/2 tests

	test(udg_edges_2_01, true(Edges == [])) :-
		new(Graph),
		edges(Graph, Edges).

	test(udg_edges_2_02, true(Edges == [1-2,1-3])) :-
		new([1,2,3], [1-2,1-3], Graph),
		edges(Graph, Edges).

	test(udg_edges_2_03, true(Edges == [1-2,1-3])) :-
		new([1-2,1-3], Graph),
		edges(Graph, Edges).

	test(udg_edges_2_04, true(Edges == [])) :-
		new([1,2,3], [], Graph),
		edges(Graph, Edges).

	% edge/3 tests

	test(udg_edge_3_01, true) :-
		new([1-2,1-3], Graph),
		edge(1, 2, Graph).

	test(udg_edge_3_02, false) :-
		new([1-2,1-3], Graph),
		edge(2, 1, Graph).

	% add_vertex/3 tests

	test(udg_add_vertex_3_01, true(Vertices == [1,2,3,4])) :-
		new([1,2,3], [], Graph),
		add_vertex(Graph, 4, NewGraph),
		vertices(NewGraph, Vertices).

	test(udg_add_vertex_3_02, true(Vertices == [1,2,3])) :-
		new([1,2,3], [], Graph),
		add_vertex(Graph, 2, NewGraph),
		vertices(NewGraph, Vertices).

	% add_vertices/3 tests

	test(udg_add_vertices_3_01, true(Graph == NewGraph)) :-
		new([1,2,3,5], [1-3,1-5], Graph),
		add_vertices(Graph, [], NewGraph).

	test(udg_add_vertices_3_02, true(Vertices == [0,1,2,3,5,9])) :-
		new([1,2,3,5], [1-3,1-5], Graph),
		add_vertices(Graph, [0,1,2,9], NewGraph),
		vertices(NewGraph, Vertices).

	% delete_vertex/3 tests

	test(udg_delete_vertex_3_01, true(Vertices == [1,3])) :-
		new([1,2,3], [1-2,2-3], Graph),
		delete_vertex(Graph, 2, NewGraph),
		vertices(NewGraph, Vertices).

	% delete_vertices/3 tests

	test(udg_delete_vertices_3_01, true(Graph == NewGraph)) :-
		new([1,2,3,4,5,6,7,8], [1-3,1-5,2-4,4-5,7-2,7-6], Graph),
		delete_vertices(Graph, [], NewGraph).

	test(udg_delete_vertices_3_02, true((Vertices == [3,4,5,6,7,8], Edges == [4-5,7-6]))) :-
		new([1,2,3,4,5,6,7,8], [1-3,1-5,2-4,4-5,7-2,7-6], Graph),
		delete_vertices(Graph, [2,1], NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% add_edge/4 tests

	test(udg_add_edge_4_01, true(Edges == [1-2,1-3,2-3])) :-
		new([1-2,1-3], Graph),
		add_edge(Graph, 2, 3, NewGraph),
		edges(NewGraph, Edges).

	% add_edges/3 tests

	test(udg_add_edges_3_01, true(Graph == NewGraph)) :-
		new([1,2,3,5], [1-3,1-5], Graph),
		add_edges(Graph, [], NewGraph).

	test(udg_add_edges_3_02, true(Edges == [1-3,1-5,2-3,5-9])) :-
		new([1,2,3,5], [1-3,1-5], Graph),
		add_edges(Graph, [2-3,5-9], NewGraph),
		edges(NewGraph, Edges).

	% delete_edge/4 tests

	test(udg_delete_edge_4_01, true(Edges == [1-3])) :-
		new([1-2,1-3], Graph),
		delete_edge(Graph, 1, 2, NewGraph),
		edges(NewGraph, Edges).

	% delete_edges/3 tests

	test(udg_delete_edges_3_01, true(Graph == NewGraph)) :-
		new([1,2,3,4,5,6,7,8], [1-3,1-5,2-4,4-5], Graph),
		delete_edges(Graph, [], NewGraph).

	test(udg_delete_edges_3_02, true((Vertices == [1,2,3,4,5,6,7,8], Edges == [1-5,2-4]))) :-
		new([1,2,3,4,5,6,7,8], [1-3,1-5,2-4,4-5], Graph),
		delete_edges(Graph, [1-6,2-3,3-2,5-7,3-2,4-5,1-3], NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% neighbors/3 tests

	test(udg_neighbors_3_01, true(Neighbors == [1,2,5,7])) :-
		new([1,2,3,4,5], [1-3,1-5,2-4,4-1,4-2,4-7,4-5], Graph),
		neighbors(4, Graph, Neighbors).

	test(udg_neighbors_3_02, false) :-
		new([1,2,3,4,5], [1-3,1-5,2-4,4-1,4-2,4-7,4-5], Graph),
		neighbors(42, Graph, _).

	% reachable/3 tests

	test(udg_reachable_3_01, true(Vertices == [1,3,5])) :-
		new([1,2,3,4,5], [1-3,1-5,2-4,4-5], Graph),
		reachable(1, Graph, Vertices).

	test(udg_reachable_3_02, true(Vertices == [2,4,5])) :-
		new([1,2,3,4,5], [1-3,1-5,2-4,4-5], Graph),
		reachable(2, Graph, Vertices).

	test(udg_reachable_3_03, true(Vertices == [1,3,5,6])) :-
		new([1,2,3,4,5], [1-3,1-5,2-4,4-5,5-6], Graph),
		reachable(1, Graph, Vertices).

	test(udg_reachable_3_04, false) :-
		new([1,2,3,4,5], [1-3,1-5,2-4,4-5], Graph),
		reachable(6, Graph, _).

	% complement/2 tests

	test(udg_complement_2_01, true((Vertices == [1,2,3,4], Edges == [1-3,2-1,2-3,2-4,3-1,3-2,3-4,4-1,4-2,4-3]))) :-
		new([3], [1-2,1-4], Graph),
		complement(Graph, NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% compose/3 tests

	test(udg_compose_3_01, true((Vertices == [1,2,3,4], Edges == [1-4,2-1,2-2,2-4]))) :-
		new([1-2,2-3], Graph1),
		new([2-4,3-1,3-2,3-4], Graph2),
		compose(Graph1, Graph2, NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% union/3 tests

	test(udg_union_3_01, true((Vertices == [1,2,3,4,5], Edges == [1-2,1-3,1-4,4-5]))) :-
		new([1,2,3], [1-2,1-3], Graph1),
		new([1,4,5], [1-4,4-5], Graph2),
		union(Graph1, Graph2, NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	test(udg_union_3_02, true((Vertices == [1,2,3], Edges == [1-2,1-3]))) :-
		new([1,2,3], [1-2,1-3], Graph1),
		new(Graph2),
		union(Graph1, Graph2, NewGraph),
		vertices(NewGraph, Vertices),
		edges(NewGraph, Edges).

	% transpose/2 tests

	test(udg_transpose_2_01, true(Edges == [2-1,3-1])) :-
		new([1,2,3], [1-2,1-3], Graph),
		transpose(Graph, NewGraph),
		edges(NewGraph, Edges).

	% transitive_closure/2 tests

	test(udg_transitive_closure_2_01, true((Vertices == [1,2,3,4,5,6], Edges == [1-2,1-3,1-4,1-5,1-6,2-4,2-5,2-6,4-6]))) :-
		new([1-2,1-3,2-4,2-5,4-6], Graph),
		transitive_closure(Graph, Closure),
		vertices(Closure, Vertices),
		edges(Closure, Edges).

	% symmetric_closure/2 tests

	test(udg_symmetric_closure_2_01, true(Edges == [1-2,1-3,2-1,3-1])) :-
		new([1,2,3], [1-2,1-3], Graph),
		symmetric_closure(Graph, Closure),
		edges(Closure, Edges).

	% topological_sort/2 tests

	test(udg_topological_sort_2_01, true(Vertices == [1,2,3])) :-
		new([1,2,3], [1-2,2-3], Graph),
		topological_sort(Graph, Vertices).

	% topological_sort/3 tests

	test(udg_topological_sort_3_01, true(Vertices == [1,2,3,4,5])) :-
		new([1,2,3], [1-2,2-3], Graph),
		topological_sort(Graph, [4,5], Vertices).

	% leaves/2 tests

	test(udg_leaves_2_01, true(Leaves == [3,5])) :-
		new([1,2,3,4,5], [1-2,2-3,4-5], Graph),
		leaves(Graph, Leaves).

	% number_of_vertices/2 tests

	test(udg_number_of_vertices_2_01, true(N == 0)) :-
		new(Graph),
		number_of_vertices(Graph, N).

	test(udg_number_of_vertices_2_02, true(N == 3)) :-
		new([1,2,3], [1-2], Graph),
		number_of_vertices(Graph, N).

	% number_of_edges/2 tests

	test(udg_number_of_edges_2_01, true(N == 0)) :-
		new(Graph),
		number_of_edges(Graph, N).

	test(udg_number_of_edges_2_02, true(N == 2)) :-
		new([1,2,3], [1-2,1-3], Graph),
		number_of_edges(Graph, N).

	% path/3 tests

	test(udg_path_3_01, true(Path == [1,2,3])) :-
		new([1-2,2-3], Graph),
		path(1, Graph, Path).

	test(udg_path_3_02, false) :-
		new([1-2,2-3], Graph),
		path(42, Graph, _).

	test(udg_path_3_03, true) :-
		new([1-2,2-3,1-3], Graph),
		findall(P, path(1, Graph, P), Paths),
		list::member([1,2,3], Paths).

	% has_path/3 tests

	test(udg_has_path_3_01, true) :-
		new([1-2,2-3], Graph),
		has_path(1, 3, Graph).

	test(udg_has_path_3_02, false) :-
		new([1-2,2-3], Graph),
		has_path(3, 1, Graph).

	test(udg_has_path_3_03, true) :-
		new([1-2,2-3], Graph),
		has_path(1, 1, Graph).

	% min_path/5 tests

	test(udg_min_path_5_01, true((Path == [1,2,3], Cost == 2))) :-
		new([1-2,2-3,1-4,4-5,5-3], Graph),
		min_path(1, 3, Graph, Path, Cost).

	test(udg_min_path_5_02, true((Path == [1], Cost == 0))) :-
		new([1-2,2-3], Graph),
		min_path(1, 1, Graph, Path, Cost).

	test(udg_min_path_5_03, false) :-
		new([1-2,3-4], Graph),
		min_path(1, 4, Graph, _, _).

	% max_path/5 tests

	test(udg_max_path_5_01, true((Path == [1,4,5,3], Cost == 3))) :-
		new([1-2,2-3,1-4,4-5,5-3], Graph),
		max_path(1, 3, Graph, Path, Cost).

	test(udg_max_path_5_02, true((Path == [1], Cost == 0))) :-
		new([1-2,2-3], Graph),
		max_path(1, 1, Graph, Path, Cost).

	test(udg_max_path_5_03, false) :-
		new([1-2,3-4], Graph),
		max_path(1, 4, Graph, _, _).

	% in_degree/3 tests

	test(udg_in_degree_3_01, true(D == 2)) :-
		new([1-3,2-3,3-4], Graph),
		in_degree(3, Graph, D).

	test(udg_in_degree_3_02, true(D == 0)) :-
		new([1-2,1-3], Graph),
		in_degree(1, Graph, D).

	% out_degree/3 tests

	test(udg_out_degree_3_01, true(D == 2)) :-
		new([1-2,1-3], Graph),
		out_degree(1, Graph, D).

	test(udg_out_degree_3_02, true(D == 0)) :-
		new([1-2,1-3], Graph),
		out_degree(3, Graph, D).

	% is_dag/1 tests

	test(udg_is_dag_1_01, true) :-
		new([1-2,2-3], Graph),
		is_dag(Graph).

	test(udg_is_dag_1_02, false) :-
		new([1-2,2-3,3-1], Graph),
		is_dag(Graph).

	% strongly_connected_components/2 tests

	test(udg_strongly_connected_components_2_01, true(Components == [[1],[2],[3]])) :-
		new([1-2,2-3], Graph),
		strongly_connected_components(Graph, Components).

	test(udg_strongly_connected_components_2_02, true(Components == [[1,2,3]])) :-
		new([1-2,2-3,3-1], Graph),
		strongly_connected_components(Graph, Components).

	test(udg_strongly_connected_components_2_03, true) :-
		new([1-2,2-3,3-1,1-4], Graph),
		strongly_connected_components(Graph, Components),
		list::msort(Components, Sorted),
		Sorted == [[1,2,3],[4]].

	% transitive_reduction/2 tests

	test(udg_transitive_reduction_2_01, true(Edges == [1-2,2-3])) :-
		new([1-2,2-3,1-3], Graph),
		transitive_reduction(Graph, Reduced),
		edges(Reduced, Edges).

	test(udg_transitive_reduction_2_02, true(Edges == [1-2,2-3])) :-
		new([1-2,2-3], Graph),
		transitive_reduction(Graph, Reduced),
		edges(Reduced, Edges).

:- end_object.
