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


:- object(tests(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Unit tests for the "flow_networks" library.',
		parnames is ['DictionaryObject']
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	:- uses(fast_random(as183), [
		between/3 as random_between/3, get_seed/1, set_seed/1, randomize/1
	]).

	:- uses(numberlist, [
		min/2 as min_list/2
	]).

	:- uses(flow_network(_DictionaryObject_), [
		new/2, from_graph/2, vertices/2, arcs/2, decompose_flow/3,
		flow_cost/3, flow_balances/3, cut_capacity/3, valid_cut/5,
		valid_flow/3 as network_valid_flow/3, valid_st_flow/5 as network_valid_st_flow/5,
		valid_min_cost_flow/4 as network_valid_min_cost_flow/4,
		valid_min_cost_flow/5 as network_valid_min_cost_flow/5
	]).

	:- uses(edmonds_karp(_DictionaryObject_), [
		max_flow/5 as ek_max_flow/5, min_cut/5 as ek_min_cut/5
	]).

	:- uses(dinic(_DictionaryObject_), [
		max_flow/5 as dinic_max_flow/5, multi_source_max_flow/5,
		node_capacity_max_flow/6, min_cut/5 as dinic_min_cut/5,
		sink_closest_min_cut/5, unique_min_cut/4, min_cuts/5
	]).

	:- uses(successive_shortest_paths(_DictionaryObject_), [
		min_cost_flow/4, min_cost_flow/5 as min_cost_flow_with_potentials/5,
		min_cost_max_flow/6, min_cost_max_flow/7 as min_cost_max_flow_with_potentials/7
	]).

	cover(flow_network(_DictionaryObject_)).
	cover(edmonds_karp(_DictionaryObject_)).
	cover(dinic(_DictionaryObject_)).
	cover(successive_shortest_paths(_DictionaryObject_)).

	% construction and inspection

	test(flow_network_new_2_01, deterministic(Arcs == [arc(a,b,0,3,0), arc(b,c,0,2,-1), arc(a,c,0,5,4)])) :-
		new([arc(a,b,3), arc(b,c,2,-1), arc(a,c,5,4)], Network),
		arcs(Network, Arcs).

	test(flow_network_new_2_02, deterministic(Arcs == [])) :-
		new([], Network),
		arcs(Network, Arcs).

	test(flow_network_new_2_03, deterministic(Arcs == [arc(a,a,0,7,0), arc(a,b,0,1,0)])) :-
		% self-loops are retained
		new([arc(a,a,7), arc(a,b,1)], Network),
		arcs(Network, Arcs).

	test(flow_network_new_2_04, deterministic) :-
		% arbitrary ground terms as vertices
		new([arc(f(1),s,2), arc(s,[x],3,1)], Network),
		vertices(Network, Vertices),
		length(Vertices, 3).

	test(flow_network_new_2_05, error(instantiation_error)) :-
		new(_, _).

	test(flow_network_new_2_06, error(instantiation_error)) :-
		new([arc(a,_,1)], _).

	test(flow_network_new_2_07, error(type_error(list, foo))) :-
		new(foo, _).

	test(flow_network_new_2_08, error(domain_error(flow_arc, edge(a,b)))) :-
		new([edge(a,b)], _).

	test(flow_network_new_2_09, error(domain_error(non_negative_integer, -1))) :-
		new([arc(a,b,-1)], _).

	test(flow_network_new_2_10, error(type_error(integer, 1.5))) :-
		new([arc(a,b,1,1.5)], _).

	test(flow_network_new_2_11, error(type_error(integer, 1.5))) :-
		new([arc(a,b,1.5)], _).

	test(flow_network_new_2_12, deterministic(Arcs == [arc(a,b,2,5,3)])) :-
		new([arc(a,b,2,5,3)], Network),
		arcs(Network, Arcs).

	test(flow_network_new_2_13, error(consistency_error(arc_lower_bound_and_capacity, 4, 3))) :-
		new([arc(a,b,4,3,1)], _).

	test(flow_network_vertices_2_01, deterministic(Vertices == [a,b,c])) :-
		new([arc(c,a,1), arc(a,b,1)], Network),
		vertices(Network, Vertices).

	test(flow_network_vertices_2_02, deterministic(Vertices == [a,b])) :-
		% vertices that only occur in self-loops are kept
		new([arc(a,b,1), arc(b,b,1)], Network),
		vertices(Network, Vertices).

	test(flow_network_decompose_flow_3_01, deterministic(Decomposition == [path([s,a,t],[1,3],1), path([s,a,t],[2,3],1), cycle([a,b,a],[4,5],1)])) :-
		new([arc(s,a,2), arc(s,a,2), arc(a,t,4), arc(a,b,1), arc(b,a,1)], Network),
		Flow = [flow(1,s,a,1), flow(2,s,a,1), flow(3,a,t,2), flow(4,a,b,1), flow(5,b,a,1)],
		decompose_flow(Network, Flow, Decomposition).

	test(flow_network_decompose_flow_3_02, deterministic(Decomposition == [cycle([a,a],[1],3)])) :-
		new([arc(a,a,3,-2)], Network),
		decompose_flow(Network, [flow(1,a,a,3)], Decomposition).

	test(flow_network_valid_flow_3_01, deterministic) :-
		new([arc(s,t,1), arc(s,t,3)], Network),
		network_valid_flow(Network, [s-2, t-(-2)], [flow(1,s,t,1), flow(2,s,t,1)]).

	test(flow_network_valid_flow_3_02, false) :-
		new([arc(s,t,1), arc(s,t,3)], Network),
		network_valid_flow(Network, [s-2, t-(-2)], [flow(1,s,t,2)]).

	test(flow_network_valid_flow_lower_bound_3_03, deterministic) :-
		new([arc(s,t,2,5,1)], Network),
		network_valid_flow(Network, [s-2, t-(-2)], [flow(1,s,t,2)]).

	test(flow_network_valid_flow_missing_lower_bound_3_04, false) :-
		new([arc(s,t,2,5,1)], Network),
		network_valid_flow(Network, [], []).

	test(flow_network_valid_st_flow_5_01, deterministic) :-
		textbook(Network),
		dinic_max_flow(Network, s, t, Value, Flow),
		network_valid_st_flow(Network, s, t, Value, Flow).

	test(flow_network_valid_st_flow_variable_source, false) :-
		new([arc(s,t,1)], Network),
		network_valid_st_flow(Network, _, t, 0, []).

	test(flow_network_valid_st_flow_variable_sink, false) :-
		new([arc(s,t,1)], Network),
		network_valid_st_flow(Network, s, _, 0, []).

	test(flow_network_valid_st_flow_partial_terminal, false) :-
		new([arc(f(1),t,1)], Network),
		network_valid_st_flow(Network, f(_), t, 0, []).

	% flow inspection

	test(flow_inspection_parallel_loops, deterministic(Cost-Balances == 17-[a-3,b-(-3),z-0])) :-
		new([arc(a,a,1,3,-2), arc(a,b,4,3), arc(a,b,2,5), arc(b,a,1,7), arc(z,z,0)], Network),
		Flow = [flow(3,a,b,1), flow(1,a,a,2), flow(4,b,a,1), flow(2,a,b,3)],
		flow_cost(Network, Flow, Cost),
		flow_balances(Network, Flow, Balances).

	test(flow_inspection_empty, deterministic(Cost-Balances == 0-[])) :-
		new([], Network),
		flow_cost(Network, [], Cost),
		flow_balances(Network, [], Balances).

	test(flow_inspection_transshipment_imbalance, deterministic(Balances == [a-1,b-(-1),c-0])) :-
		new([arc(a,b,2), arc(b,c,2)], Network),
		flow_balances(Network, [flow(1,a,b,1)], Balances).

	test(flow_inspection_variable_list, error(instantiation_error)) :-
		new([], Network),
		flow_cost(Network, _, _).

	test(flow_inspection_partial_list, error(instantiation_error)) :-
		new([arc(a,b,2)], Network),
		flow_balances(Network, [flow(1,a,b,1)| _], _).

	test(flow_inspection_non_list, error(type_error(list, foo))) :-
		new([], Network),
		flow_cost(Network, foo, _).

	test(flow_inspection_variable_entry, error(instantiation_error)) :-
		new([], Network),
		flow_cost(Network, [_], _).

	test(flow_inspection_malformed_entry, error(domain_error(flow, edge(a,b,1)))) :-
		new([arc(a,b,2)], Network),
		flow_cost(Network, [edge(a,b,1)], _).

	test(flow_inspection_non_integer_id, error(type_error(integer, 1.5))) :-
		new([arc(a,b,2)], Network),
		flow_cost(Network, [flow(1.5,a,b,1)], _).

	test(flow_inspection_non_positive_id, error(domain_error(positive_integer, 0))) :-
		new([arc(a,b,2)], Network),
		flow_cost(Network, [flow(0,a,b,1)], _).

	test(flow_inspection_non_integer_amount, error(type_error(integer, 1.5))) :-
		new([arc(a,b,2)], Network),
		flow_balances(Network, [flow(1,a,b,1.5)], _).

	test(flow_inspection_non_positive_amount, error(domain_error(positive_integer, 0))) :-
		new([arc(a,b,2)], Network),
		flow_balances(Network, [flow(1,a,b,0)], _).

	test(flow_inspection_variable_endpoint, error(instantiation_error)) :-
		new([arc(a,b,2)], Network),
		flow_cost(Network, [flow(1,_,b,1)], _).

	test(flow_inspection_missing_arc, error(existence_error(arc, 2))) :-
		new([arc(a,b,2)], Network),
		flow_cost(Network, [flow(2,a,b,1)], _).

	test(flow_inspection_wrong_endpoints, error(domain_error(flow_arc_endpoints, flow(1,b,a,1)))) :-
		new([arc(a,b,2)], Network),
		flow_balances(Network, [flow(1,b,a,1)], _).

	test(flow_inspection_duplicate_arc, error(domain_error(capacity_valid_flow, _))) :-
		new([arc(a,b,2)], Network),
		flow_cost(Network, [flow(1,a,b,1), flow(1,a,b,1)], _).

	test(flow_inspection_upper_bound, error(domain_error(capacity_valid_flow, _))) :-
		new([arc(a,b,2)], Network),
		flow_balances(Network, [flow(1,a,b,3)], _).

	test(flow_inspection_lower_bound, error(domain_error(capacity_valid_flow, _))) :-
		new([arc(a,b,2,3,0)], Network),
		flow_cost(Network, [flow(1,a,b,1)], _).

	test(flow_inspection_missing_lower_bound, error(domain_error(capacity_valid_flow, _))) :-
		new([arc(a,a,1,3,0)], Network),
		flow_balances(Network, [], _).

	% cut inspection and verification

	test(cut_capacity_parallel_loops, deterministic(Capacity == 5)) :-
		new([arc(s,t,2), arc(s,t,3), arc(t,s,4), arc(s,s,6), arc(s,z,0)], Network),
		cut_capacity(Network, [z,s], Capacity),
		valid_cut(Network, s, t, Capacity, cut([s,z], [t], [arc(s,t,2),arc(s,t,3)])).

	test(cut_capacity_empty_full_sets, deterministic(Empty-Full == 0-0)) :-
		new([arc(s,t,3)], Network),
		cut_capacity(Network, [], Empty),
		cut_capacity(Network, [t,s], Full).

	test(cut_capacity_empty_network, deterministic(Capacity == 0)) :-
		new([], Network),
		cut_capacity(Network, [], Capacity).

	test(cut_capacity_variable_list, error(instantiation_error)) :-
		new([arc(s,t,3)], Network),
		cut_capacity(Network, _, _).

	test(cut_capacity_non_list, error(type_error(list, foo))) :-
		new([arc(s,t,3)], Network),
		cut_capacity(Network, foo, _).

	test(cut_capacity_variable_vertex, error(instantiation_error)) :-
		new([arc(s,t,3)], Network),
		cut_capacity(Network, [_], _).

	test(cut_capacity_unknown_vertex, error(existence_error(vertex, x))) :-
		new([arc(s,t,3)], Network),
		cut_capacity(Network, [x], _).

	test(cut_capacity_duplicate_vertex, error(domain_error(unique_cut_vertices, s))) :-
		new([arc(s,t,3)], Network),
		cut_capacity(Network, [s,s], _).

	test(cut_capacity_lower_bound, error(domain_error(zero_lower_bounds, _))) :-
		new([arc(s,t,1,3,0)], Network),
		cut_capacity(Network, [s], _).

	test(valid_cut_nonminimum, deterministic) :-
		new([arc(s,a,3), arc(a,t,1)], Network),
		valid_cut(Network, s, t, 3, cut([s], [a,t], [arc(s,a,3)])).

	test(valid_cut_invalid_partitions, deterministic) :-
		new([arc(s,a,3), arc(a,t,1)], Network),
		forall(
			member(Cut, [
				cut([s,s], [a,t], [arc(s,a,3)]),
				cut([s], [s,t], [arc(s,a,3)]),
				cut([s], [a,s,t], [arc(s,a,3)]),
				cut([s], [t], [arc(s,a,3)]),
				cut([s,x], [a,t], [arc(s,a,3)]),
				cut([s,a], [t], [arc(a,t,1)]),
				cut(foo, [a,t], []),
				cut([s], foo, []),
				cut([s], [a,t], []),
				cut([s], [a,t], [arc(s,a,2)]),
				cut([s], [a,t], [arc(s,a,3),arc(s,a,3)]),
				foo
			]),
			\+ valid_cut(Network, s, t, 3, Cut)
		).

	test(valid_cut_unsorted_sides, deterministic) :-
		new([arc(s,a,3), arc(a,t,1)], Network),
		\+ valid_cut(Network, s, t, 1, cut([s,a], [t], [arc(a,t,1)])),
		\+ valid_cut(Network, s, t, 3, cut([s], [t,a], [arc(s,a,3)])).

	test(valid_cut_invalid_terminals_values, deterministic) :-
		new([arc(s,t,3)], Network),
		Cut = cut([s], [t], [arc(s,t,3)]),
		forall(
			member(Source-Sink-Value, [s-s-3, x-t-3, s-x-3, t-s-3, s-t-2, s-t-(-1), s-t-3.0]),
			\+ valid_cut(Network, Source, Sink, Value, Cut)
		),
		\+ valid_cut(Network, _, t, 3, Cut),
		\+ valid_cut(Network, s, t, 3, cut([s| _], [t], [arc(s,t,3)])).

	test(valid_cut_parallel_multiplicity_order, deterministic) :-
		new([arc(s,t,2), arc(s,t,3)], Network),
		\+ valid_cut(Network, s, t, 5, cut([s], [t], [arc(s,t,5)])),
		\+ valid_cut(Network, s, t, 5, cut([s], [t], [arc(s,t,3),arc(s,t,2)])).

	test(valid_cut_lower_bound, false) :-
		new([arc(s,t,1,3,0)], Network),
		valid_cut(Network, s, t, 3, cut([s], [t], [arc(s,t,3)])).

	% maximum flow: known instances

	test(max_flow_5_01, deterministic(Value == 23)) :-
		clrs(Network),
		ek_max_flow(Network, s, t, Value, _).

	test(max_flow_5_02, deterministic(Value == 23)) :-
		clrs(Network),
		dinic_max_flow(Network, s, t, Value, _).

	test(max_flow_5_03, deterministic) :-
		clrs(Network),
		dinic_max_flow(Network, s, t, Value, Flow),
		network_valid_st_flow(Network, s, t, Value, Flow).

	test(max_flow_5_04, deterministic) :-
		clrs(Network),
		ek_max_flow(Network, s, t, Value, Flow),
		network_valid_st_flow(Network, s, t, Value, Flow).

	test(max_flow_5_05, deterministic(Value-Flow == 0-[])) :-
		% sink not reachable
		new([arc(s,a,5), arc(b,t,5)], Network),
		dinic_max_flow(Network, s, t, Value, Flow).

	test(max_flow_5_06, deterministic(Value == 7)) :-
		% parallel arcs
		new([arc(s,t,3), arc(s,t,4)], Network),
		dinic_max_flow(Network, s, t, Value, _).

	test(max_flow_5_07, deterministic(Value == 5)) :-
		% antiparallel arcs
		new([arc(s,a,5), arc(a,s,2), arc(a,t,9), arc(t,a,3)], Network),
		ek_max_flow(Network, s, t, Value, _),
		dinic_max_flow(Network, s, t, Value, _).

	test(max_flow_5_08, deterministic(Value == 0)) :-
		new([arc(s,t,0)], Network),
		dinic_max_flow(Network, s, t, Value, _).

	test(max_flow_5_09, deterministic(Value == 4)) :-
		% flow must be redirected through a reverse arc
		new([arc(s,a,2), arc(s,b,2), arc(a,b,2), arc(a,t,2), arc(b,t,2)], Network),
		ek_max_flow(Network, s, t, Value, _),
		dinic_max_flow(Network, s, t, Value, _).

	test(max_flow_5_10, deterministic(Value-Flow == 3-[flow(1,s,t,3)])) :-
		% a vertex only reachable through a self-loop
		new([arc(s,t,3), arc(z,z,5)], Network),
		dinic_max_flow(Network, s, t, Value, Flow).

	test(max_flow_5_11, error(consistency_error(distinct_vertices, s, s))) :-
		new([arc(s,t,1)], Network),
		dinic_max_flow(Network, s, s, _, _).

	test(max_flow_5_12, error(existence_error(vertex, x))) :-
		new([arc(s,t,1)], Network),
		ek_max_flow(Network, s, x, _, _).

	test(max_flow_5_13, error(instantiation_error)) :-
		new([arc(s,t,1)], Network),
		dinic_max_flow(Network, _, t, _, _).

	test(max_flow_partial_source, error(instantiation_error)) :-
		new([arc(f(1),t,1)], Network),
		dinic_max_flow(Network, f(_), t, _, _).

	test(max_flow_partial_sink, error(instantiation_error)) :-
		new([arc(s,f(1),1)], Network),
		ek_max_flow(Network, s, f(_), _, _).

	test(max_flow_5_14, error(domain_error(zero_lower_bounds, _))) :-
		new([arc(s,t,1,3,0)], Network),
		dinic_max_flow(Network, s, t, _, _).

	test(multi_source_max_flow_5_01, deterministic(Value-Flow == 5-[flow(1,s1,a,2), flow(2,s2,a,3), flow(3,a,t,5)])) :-
		new([arc(s1,a,2), arc(s2,a,3), arc(a,t,5)], Network),
		multi_source_max_flow(Network, [s1,s2], [t], Value, Flow).

	test(multi_source_max_flow_5_02, deterministic(Value == 5)) :-
		new([arc(s,t1,2), arc(s,t2,3)], Network),
		multi_source_max_flow(Network, [s], [t1,t2], Value, _).

	test(multi_source_max_flow_5_03, error(consistency_error(disjoint_terminals, _, _))) :-
		new([arc(s,t,1)], Network),
		multi_source_max_flow(Network, [s], [s,t], _, _).

	test(multi_source_max_flow_partial_source, error(instantiation_error)) :-
		new([arc(f(1),t,1)], Network),
		multi_source_max_flow(Network, [f(_)], [t], _, _).

	test(multi_source_max_flow_partial_sink, error(instantiation_error)) :-
		new([arc(s,f(1),1)], Network),
		multi_source_max_flow(Network, [s], [f(_)], _, _).

	test(multi_source_max_flow_ground_compound, deterministic(Value == 3)) :-
		new([arc(f(1),g(1),1), arc(f(2),g(1),2)], Network),
		multi_source_max_flow(Network, [f(1),f(2)], [g(1)], Value, _).

	test(node_capacity_max_flow_5_01, deterministic(Value == 3)) :-
		new([arc(s,a,5), arc(s,b,5), arc(a,t,5), arc(b,t,5)], Network),
		node_capacity_max_flow(Network, s, t, [a-1, b-2], Value, Flow),
		network_valid_st_flow(Network, s, t, Value, Flow).

	test(node_capacity_max_flow_5_02, error(domain_error(intermediate_vertex, s))) :-
		new([arc(s,t,1)], Network),
		node_capacity_max_flow(Network, s, t, [s-1], _, _).

	test(node_capacity_max_flow_partial_vertex, error(instantiation_error)) :-
		new([arc(s,f(1),2), arc(f(1),t,2)], Network),
		node_capacity_max_flow(Network, s, t, [f(_)-1], _, _).

	test(node_capacity_max_flow_ground_compound, deterministic(Value == 1)) :-
		new([arc(s,f(1),2), arc(f(1),t,2)], Network),
		node_capacity_max_flow(Network, s, t, [f(1)-1], Value, _).

	% minimum cut

	test(min_cut_5_01, deterministic) :-
		clrs(Network),
		dinic_min_cut(Network, s, t, Value, cut(SourceSide, SinkSide, CutArcs)),
		Value == 23,
		valid_cut(Network, s, t, Value, cut(SourceSide, SinkSide, CutArcs)).

	test(min_cut_5_02, deterministic) :-
		clrs(Network),
		ek_min_cut(Network, s, t, Value, cut(SourceSide, SinkSide, CutArcs)),
		Value == 23,
		valid_cut(Network, s, t, Value, cut(SourceSide, SinkSide, CutArcs)).

	test(min_cut_5_03, deterministic(Cut == cut([a,b,s], [t], [arc(a,t,8), arc(b,t,10)]))) :-
		new([arc(s,a,10), arc(s,b,10), arc(a,b,2), arc(a,t,8), arc(b,t,10)], Network),
		dinic_min_cut(Network, s, t, 18, Cut).

	test(min_cut_5_04, deterministic(Cut == cut([s], [a,t], []))) :-
		new([arc(s,a,0), arc(a,t,5)], Network),
		dinic_min_cut(Network, s, t, 0, Cut).

	test(min_cut_sink_closest_5_01, deterministic(Cut == cut([a,b,s], [t], [arc(a,t,1), arc(b,t,1)]))) :-
		new([arc(s,a,1), arc(a,t,1), arc(s,b,1), arc(b,t,1)], Network),
		sink_closest_min_cut(Network, s, t, 2, Cut).

	test(min_cut_unique_5_01, deterministic(Unique == true)) :-
		new([arc(s,t,3)], Network),
		unique_min_cut(Network, s, t, Unique).

	test(min_cut_unique_5_02, deterministic(Unique == false)) :-
		new([arc(s,a,1), arc(a,t,1), arc(s,b,1), arc(b,t,1)], Network),
		unique_min_cut(Network, s, t, Unique).

	test(min_cuts_5_01, deterministic(Sides == [[a,b,s],[a,s],[b,s],[s]])) :-
		new([arc(s,a,1), arc(a,t,1), arc(s,b,1), arc(b,t,1)], Network),
		findall(cut(SourceSide, SinkSide, CutArcs), min_cuts(Network, s, t, 2, cut(SourceSide, SinkSide, CutArcs)), Cuts),
		findall(SourceSide, member(cut(SourceSide, _, _), Cuts), SourceSides),
		sort(SourceSides, Sides),
		forall(member(Cut, Cuts), valid_cut(Network, s, t, 2, Cut)).

	% minimum cost flow: known instances

	test(min_cost_flow_4_01, deterministic(Cost == 10)) :-
		textbook(Network),
		min_cost_flow(Network, [s-3, t-(-3)], Cost, _).

	test(min_cost_flow_4_02, deterministic(Flow == [flow(1,s,a,1), flow(2,s,b,2), flow(3,a,b,1), flow(5,b,t,3)])) :-
		textbook(Network),
		min_cost_flow(Network, [s-3, t-(-3)], _, Flow).

	test(min_cost_flow_4_03, false) :-
		% more supply than the network can route
		textbook(Network),
		min_cost_flow(Network, [s-7, t-(-7)], _, _).

	test(min_cost_flow_4_04, deterministic(Cost-Flow == 0-[])) :-
		textbook(Network),
		min_cost_flow(Network, [], Cost, Flow).

	test(min_cost_flow_lower_bound_4_01, deterministic(Cost-Flow == 3-[flow(1,s,t,3)])) :-
		new([arc(s,t,2,5,1)], Network),
		min_cost_flow(Network, [s-3, t-(-3)], Cost, Flow),
		network_valid_min_cost_flow(Network, [s-3, t-(-3)], Flow, Cost).

	test(min_cost_flow_lower_circulation_4_02, deterministic(Cost-Flow == 4-[flow(1,a,b,2), flow(2,b,a,2)])) :-
		new([arc(a,b,2,3,1), arc(b,a,2,3,1)], Network),
		min_cost_flow(Network, [], Cost, Flow).

	test(min_cost_flow_lower_infeasible_4_03, false) :-
		new([arc(a,b,1,1,0)], Network),
		min_cost_flow(Network, [], _, _).

	test(min_cost_flow_lower_negative_loop_4_04, deterministic(Cost-Flow == -6-[flow(1,a,a,3)])) :-
		new([arc(a,a,1,3,-2)], Network),
		min_cost_flow(Network, [], Cost, Flow).

	test(min_cost_flow_4_05, deterministic(Cost == -6)) :-
		% negative cost cycle is saturated
		new([arc(a,b,3,-2), arc(b,c,3,1), arc(c,a,3,-1)], Network),
		min_cost_flow(Network, [], Cost, _).

	test(min_cost_flow_4_16, deterministic(Cost-Flow == -6-[flow(1,a,a,3)])) :-
		% negative cost self-loop is saturated and reported
		new([arc(a,a,3,-2)], Network),
		min_cost_flow(Network, [], Cost, Flow).

	test(min_cost_flow_5_01, deterministic) :-
		textbook(Network),
		Supplies = [s-3, t-(-3)],
		min_cost_flow_with_potentials(Network, Supplies, 10, Flow, Potentials),
		Potentials = [a-A, b-B, s-S, t-T],
		integer(A),
		integer(B),
		integer(S),
		integer(T),
		network_valid_min_cost_flow(Network, Supplies, Flow, 10),
		network_valid_min_cost_flow(Network, Supplies, Flow, 10, Potentials).

	test(min_cost_flow_optimality_4_01, false) :-
		textbook(Network),
		Flow = [flow(1,s,a,3), flow(4,a,t,3)],
		network_valid_min_cost_flow(Network, [s-3, t-(-3)], Flow, 15).

	test(min_cost_flow_4_06, deterministic(Cost == -2)) :-
		new([arc(a,b,3,-2), arc(b,c,3,1), arc(c,a,3,-1), arc(a,d,2,5), arc(c,d,2,1)], Network),
		min_cost_flow(Network, [a-2, d-(-2)], Cost, _).

	test(min_cost_flow_4_07, deterministic(Cost == 0)) :-
		% a negative cost arc that is not part of a cycle is not used
		new([arc(a,b,5,-3)], Network),
		min_cost_flow(Network, [], Cost, _).

	test(min_cost_flow_4_08, deterministic(Cost == 6)) :-
		% duplicate supply entries are added
		new([arc(a,b,5,1), arc(b,c,5,1), arc(a,c,1,5)], Network),
		min_cost_flow(Network, [a-2, a-1, c-(-3)], Cost, _).

	test(min_cost_flow_4_09, deterministic(Cost == 7)) :-
		% multiple supplies and demands
		new([arc(s1,m,5,1), arc(s2,m,5,2), arc(m,d1,5,1), arc(m,d2,5,1)], Network),
		min_cost_flow(Network, [s1-2, s2-1, d1-(-1), d2-(-2)], Cost, _).

	test(min_cost_flow_4_10, error(domain_error(balanced_supplies, _))) :-
		textbook(Network),
		min_cost_flow(Network, [s-3, t-(-2)], _, _).

	test(min_cost_flow_4_11, error(existence_error(vertex, x))) :-
		textbook(Network),
		min_cost_flow(Network, [s-1, x-(-1)], _, _).

	test(min_cost_flow_4_12, error(type_error(integer, 1.5))) :-
		textbook(Network),
		min_cost_flow(Network, [s-1.5, t-(-1.5)], _, _).

	test(min_cost_flow_4_13, error(instantiation_error)) :-
		textbook(Network),
		min_cost_flow(Network, [_-1, t-(-1)], _, _).

	test(min_cost_flow_4_14, error(type_error(list, foo))) :-
		textbook(Network),
		min_cost_flow(Network, foo, _, _).

	test(min_cost_max_flow_6_01, deterministic(Value-Cost == 6-24)) :-
		textbook(Network),
		min_cost_max_flow(Network, s, t, Value, Cost, _).

	test(min_cost_max_flow_6_02, deterministic(Value-Cost-Flow == 0-0-[])) :-
		new([arc(s,a,5,1), arc(b,t,5,1)], Network),
		min_cost_max_flow(Network, s, t, Value, Cost, Flow).

	test(min_cost_max_flow_6_05, deterministic(Value-Cost-Flow == 1-(-3)-[flow(1,s,t,1), flow(2,a,a,2)])) :-
		new([arc(s,t,1,3), arc(a,a,2,-3)], Network),
		min_cost_max_flow(Network, s, t, Value, Cost, Flow).

	test(min_cost_max_flow_7_01, deterministic) :-
		textbook(Network),
		min_cost_max_flow_with_potentials(Network, s, t, 6, 24, _, Potentials),
		Potentials = [a-A, b-B, s-S, t-T],
		integer(A),
		integer(B),
		integer(S),
		integer(T).

	test(min_cost_max_flow_6_03, error(consistency_error(distinct_vertices, s, s))) :-
		textbook(Network),
		min_cost_max_flow(Network, s, s, _, _, _).

	test(min_cost_max_flow_6_04, deterministic) :-
		textbook(Network),
		min_cost_max_flow(Network, s, t, Value, Cost, Flow),
		network_valid_st_flow(Network, s, t, Value, Flow),
		flow_cost(Network, Flow, Cost).

	% assignment problem as a min cost flow: workers w1,w2,w3 to jobs j1,j2,j3

	test(min_cost_flow_4_15, deterministic(Cost == 5)) :-
		% costs: w1: 4 1 3; w2: 2 0 5; w3: 3 2 2
		Costs = [w1-[j1-4, j2-1, j3-3], w2-[j1-2, j2-0, j3-5], w3-[j1-3, j2-2, j3-2]],
		assignment_arcs(Costs, Arcs0),
		append(Arcs0, [arc(src,w1,1), arc(src,w2,1), arc(src,w3,1), arc(j1,snk,1), arc(j2,snk,1), arc(j3,snk,1)], Arcs),
		new(Arcs, Network),
		min_cost_flow(Network, [src-3, snk-(-3)], Cost, _).

	% conversion from graphs

	test(flow_network_from_graph_2_01, deterministic(Value == 18)) :-
		weighted_directed_graph(_DictionaryObject_)::new([(s-a)-10, (s-b)-10, (a-b)-2, (a-t)-8, (b-t)-10], Graph),
		from_graph(Graph, Network),
		dinic_max_flow(Network, s, t, Value, _).

	% alternative algorithms must agree on random networks and match a brute-force oracle

	test(random_networks_01, all(random_check_max_flow(Arcs)), [
		setup(reset_random_seed(123456789, SavedSeed)), cleanup(set_seed(SavedSeed))
	]) :-
		between(1, 60, _),
		random_between(1, 6, ArcCount),
		random_arcs(ArcCount, Arcs).

	test(random_networks_02, all(random_check_min_cost(Arcs)), [
		setup(reset_random_seed(987654321, SavedSeed)), cleanup(set_seed(SavedSeed))
	]) :-
		between(1, 60, _),
		random_between(1, 6, ArcCount),
		random_arcs(ArcCount, Arcs).

	test(random_networks_03, all(random_check_supplies(Arcs)), [
		setup(reset_random_seed(555, SavedSeed)), cleanup(set_seed(SavedSeed))
	]) :-
		between(1, 60, _),
		random_between(1, 6, ArcCount),
		random_arcs(ArcCount, Arcs).

	% auxiliary predicates

	clrs(Network) :-
		new([
			arc(s,v1,16), arc(s,v2,13), arc(v1,v3,12), arc(v2,v1,4), arc(v2,v4,14),
			arc(v3,v2,9), arc(v3,t,20), arc(v4,v3,7), arc(v4,t,4)
		], Network).

	textbook(Network) :-
		new([arc(s,a,4,2), arc(s,b,2,2), arc(a,b,2,1), arc(a,t,3,3), arc(b,t,5,1)], Network).

	assignment_arcs([], []).
	assignment_arcs([Worker-Jobs| Rest], Arcs) :-
		job_arcs(Jobs, Worker, Arcs, Arcs1),
		assignment_arcs(Rest, Arcs1).

	job_arcs([], _, Arcs, Arcs).
	job_arcs([Job-Cost| Jobs], Worker, [arc(Worker,Job,1,Cost)| Arcs], Arcs0) :-
		job_arcs(Jobs, Worker, Arcs, Arcs0).

	% deterministic pseudo-random network generation

	random_arcs(Count, Arcs) :-
		(	Count =:= 0 ->
			Arcs = []
		;	random_between(1, 5, From),
			random_between(1, 5, To),
			random_between(0, 3, Capacity),
			random_between(-2, 4, Cost),
			Arcs = [arc(From, To, Capacity, Cost)| Arcs1],
			Count1 is Count - 1,
			random_arcs(Count1, Arcs1)
		).

	reset_random_seed(Seed, SavedSeed) :-
		get_seed(SavedSeed),
		randomize(Seed).

	% brute force oracle: enumerate all flows (every arc between zero and its capacity)

	brute_balances([], _, _, []).
	brute_balances([Vertex| Vertices], Arcs, Amounts, [Vertex-Balance| Balances]) :-
		net_out(Arcs, Amounts, Vertex, 0, Balance),
		brute_balances(Vertices, Arcs, Amounts, Balances).

	net_out([], [], _, Balance, Balance).
	net_out([arc(From, To, _, _)| Arcs], [Amount| Amounts], Vertex, Balance0, Balance) :-
		(	From == Vertex ->
			Balance1 is Balance0 + Amount
		;	To == Vertex ->
			Balance1 is Balance0 - Amount
		;	Balance1 = Balance0
		),
		net_out(Arcs, Amounts, Vertex, Balance1, Balance).

	sum_cost([], [], Cost, Cost).
	sum_cost([arc(_, _, _, ArcCost)| Arcs], [Amount| Amounts], Cost0, Cost) :-
		Cost1 is Cost0 + Amount * ArcCost,
		sum_cost(Arcs, Amounts, Cost1, Cost).

	brute_flows([], []).
	brute_flows([arc(_, _, Capacity, _)| Arcs], [Amount| Amounts]) :-
		up_to(0, Capacity, Amount),
		brute_flows(Arcs, Amounts).

	up_to(Low, High, Low) :-
		Low =< High.
	up_to(Low, High, Value) :-
		Low < High,
		Next is Low + 1,
		up_to(Next, High, Value).

	arc_vertices(Arcs, Vertices) :-
		arc_endpoints(Arcs, Endpoints),
		sort(Endpoints, Vertices).

	arc_endpoints([], []).
	arc_endpoints([arc(From, To, _, _)| Arcs], [From, To| Endpoints]) :-
		arc_endpoints(Arcs, Endpoints).

	% random brute-force comparisons omit self-loops (covered separately)

	without_loops([], []).
	without_loops([arc(From, To, Capacity, Cost)| Arcs], Kept) :-
		(	From == To ->
			Kept = Kept1
		;	Kept = [arc(From, To, Capacity, Cost)| Kept1]
		),
		without_loops(Arcs, Kept1).

	% brute-force best (maximum value, then minimum cost) s-t flow from vertex 1 to vertex 5
	% among flows satisfying all vertex balances

	brute_st(Arcs, Source, Sink, BestValue, BestCost) :-
		arc_vertices(Arcs, Vertices),
		findall(
			Value-Cost,
			(	brute_flows(Arcs, Amounts),
				brute_balances(Vertices, Arcs, Amounts, Balances),
				st_balances(Balances, Source, Sink, Value),
				sum_cost(Arcs, Amounts, 0, Cost)
			),
			Pairs0
		),
		best_pair(Pairs0, BestValue, BestCost).

	st_balances(Balances, Source, Sink, Value) :-
		member_balance(Balances, Source, Value),
		member_balance(Balances, Sink, SinkBalance),
		SinkBalance =:= -Value,
		others_zero(Balances, Source, Sink).

	member_balance([Vertex-Balance| Balances], Target, Value) :-
		(	Vertex == Target ->
			Value = Balance
		;	member_balance(Balances, Target, Value)
		).

	others_zero([], _, _).
	others_zero([Vertex-Balance| Balances], Source, Sink) :-
		(	(	Vertex == Source
			;	Vertex == Sink
			) ->
			true
		;	Balance =:= 0
		),
		others_zero(Balances, Source, Sink).

	best_pair(Pairs, BestValue, BestCost) :-
		max_value(Pairs, -1000, BestValue),
		min_cost_for(Pairs, BestValue, 100000, BestCost).

	max_value([], Value, Value).
	max_value([Value-_| Pairs], Value0, Max) :-
		Value1 is max(Value, Value0),
		max_value(Pairs, Value1, Max).

	min_cost_for([], _, Cost, Cost).
	min_cost_for([Value-Cost| Pairs], Best, Cost0, Min) :-
		(	Value =:= Best ->
			Cost1 is min(Cost, Cost0)
		;	Cost1 = Cost0
		),
		min_cost_for(Pairs, Best, Cost1, Min).

	random_check_max_flow(Arcs0) :-
		without_loops(Arcs0, Arcs),
		arcs_with_terminals(Arcs, Arcs1),
		new(Arcs1, Network),
		brute_st(Arcs1, 1, 5, BestValue, _),
		ek_max_flow(Network, 1, 5, EKValue, EKFlow),
		dinic_max_flow(Network, 1, 5, DinicValue, DinicFlow),
		dinic_min_cut(Network, 1, 5, CutValue, cut(SourceSide, SinkSide, CutArcs)),
		EKValue =:= BestValue,
		DinicValue =:= BestValue,
		CutValue =:= BestValue,
		network_valid_st_flow(Network, 1, 5, EKValue, EKFlow),
		network_valid_st_flow(Network, 1, 5, DinicValue, DinicFlow),
		valid_cut(Network, 1, 5, CutValue, cut(SourceSide, SinkSide, CutArcs)),
		cut_capacity(Network, SourceSide, CutValue).

	% make sure that vertices 1 to 5 exist in every generated network (using zero capacity arcs)

	arcs_with_terminals(Arcs, [arc(1,2,0,0), arc(2,3,0,0), arc(3,4,0,0), arc(4,5,0,0)| Arcs]).

	random_check_min_cost(Arcs0) :-
		without_loops(Arcs0, Arcs),
		arcs_with_terminals(Arcs, Arcs1),
		new(Arcs1, Network),
		brute_st(Arcs1, 1, 5, BestValue, BestCost),
		min_cost_max_flow(Network, 1, 5, Value, Cost, Flow),
		Value =:= BestValue,
		Cost =:= BestCost,
		network_valid_st_flow(Network, 1, 5, Value, Flow),
		NegatedValue is -Value,
		network_valid_min_cost_flow(Network, [1-Value, 5-NegatedValue], Flow, Cost).

	% minimum cost flow with random supplies compared with the brute-force minimum
	% (failing exactly when no feasible flow exists)

	random_check_supplies(Arcs0) :-
		without_loops(Arcs0, Arcs),
		arcs_with_terminals(Arcs, Arcs1),
		new(Arcs1, Network),
		Supplies = [2-2, 3-(-1), 4-(-1)],
		arc_vertices(Arcs1, Vertices),
		findall(
			Cost,
			(	brute_flows(Arcs1, Amounts),
				brute_balances(Vertices, Arcs1, Amounts, Balances),
				supplies_match(Balances, Supplies),
				sum_cost(Arcs1, Amounts, 0, Cost)
			),
			Costs
		),
		(	Costs == [] ->
			\+ min_cost_flow(Network, Supplies, _, _)
		;	min_list(Costs, Best),
			min_cost_flow(Network, Supplies, Cost, Flow),
			Cost =:= Best,
			network_valid_flow(Network, Supplies, Flow)
		).

	supplies_match([], _).
	supplies_match([Vertex-Balance| Balances], Supplies) :-
		(	member(Vertex-Supply, Supplies) ->
			Balance =:= Supply
		;	Balance =:= 0
		),
		supplies_match(Balances, Supplies).

:- end_object.
