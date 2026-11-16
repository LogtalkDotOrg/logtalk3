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


:- object(tests_piecewise_costs(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Unit tests for convex piecewise-linear flow costs.',
		parnames is ['DictionaryObject']
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(numberlist, [
		min/2 as min_list/2
	]).

	:- uses(flow_network(_DictionaryObject_), [
		new/2 as network_new/2,
		vertices/2 as network_vertices/2,
		arcs/2 as network_arcs/2,
		flow_cost/3 as network_flow_cost/3
	]).

	:- uses(piecewise_costs(_DictionaryObject_), [
		build/4, collapse_flow/4
	]).

	:- uses(successive_shortest_paths(_DictionaryObject_), [
		min_cost_flow/4
	]).

	cover(piecewise_costs(_DictionaryObject_)).

	test(piecewise_costs_lower_bound_inside_segment, deterministic(Arcs == [
		arc(s,t,2,2,-2), arc(s,t,1,3,1)
	])) :-
		build([s,t], [arc(s,t,3,[segment(2,-2),segment(3,1)])], Network, _),
		network_arcs(Network, Arcs).

	test(piecewise_costs_lower_bound_at_boundary, deterministic(Arcs == [
		arc(s,t,2,2,-2), arc(s,t,0,3,1)
	])) :-
		build([s,t], [arc(s,t,2,[segment(2,-2),segment(3,1)])], Network, _),
		network_arcs(Network, Arcs).

	test(piecewise_costs_parallel_arcs_and_self_loop, deterministic(Flow == [
		flow(1,a,b,1), flow(2,a,b,2), flow(3,a,a,1)
	])) :-
		CostArcs = [
			arc(a,b,0,[segment(1,1)]),
			arc(a,b,0,[segment(2,2)]),
			arc(a,a,0,[segment(1,-1)])
		],
		build([a,b], CostArcs, Network, Mapping),
		ExpandedFlow = [flow(1,a,b,1), flow(2,a,b,2), flow(3,a,a,1)],
		collapse_flow(Network, Mapping, ExpandedFlow, Flow).

	test(piecewise_costs_equal_slope_segments, deterministic(Flow == [flow(1,a,b,1)])) :-
		build([a,b], [arc(a,b,0,[segment(2,-1),segment(2,-1)])], Network, Mapping),
		collapse_flow(Network, Mapping, [flow(2,a,b,1)], Flow).

	test(piecewise_costs_omits_zero_amounts, deterministic(Flow == [])) :-
		build([], [arc(a,b,0,[segment(1,1)])], Network, Mapping),
		collapse_flow(Network, Mapping, [], Flow).

	test(piecewise_costs_preserves_isolated_vertices, deterministic(Vertices == [isolated])) :-
		build([isolated], [], Network, _),
		network_vertices(Network, Vertices).

	test(piecewise_costs_rejects_noncanonical_projection, error(domain_error(canonical_piecewise_flow, [flow(2,a,b,1)]))) :-
		build([a,b], [arc(a,b,0,[segment(2,-1),segment(2,2)])], Network, Mapping),
		collapse_flow(Network, Mapping, [flow(2,a,b,1)], _).

	test(piecewise_costs_rejects_wrong_network_mapping, error(consistency_error(piecewise_mapping_network, _, _))) :-
		build([a,b], [arc(a,b,0,[segment(1,1)])], _, Mapping),
		network_new([arc(a,b,1)], OtherNetwork),
		collapse_flow(OtherNetwork, Mapping, [], _).

	test(piecewise_costs_mapping_public_snapshot, deterministic(Flow == [flow(1,a,b,1)])) :-
		build([a,b], [arc(a,b,0,[segment(1,1)])], Network, Mapping),
		ground(Mapping),
		collapse_flow(Network, Mapping, [flow(1,a,b,1)], Flow).

	test(piecewise_costs_malformed_mapping, error(domain_error(piecewise_cost_mapping, foo))) :-
		build([a,b], [arc(a,b,0,[segment(1,1)])], Network, _),
		collapse_flow(Network, foo, [], _).

	test(piecewise_costs_variable_mapping, error(instantiation_error)) :-
		build([a,b], [arc(a,b,0,[segment(1,1)])], Network, _),
		collapse_flow(Network, _, [], _).

	test(piecewise_costs_rejects_invalid_expanded_flow, error(domain_error(flow_arc_endpoints, flow(1,b,a,1)))) :-
		build([a,b], [arc(a,b,0,[segment(1,1)])], Network, Mapping),
		collapse_flow(Network, Mapping, [flow(1,b,a,1)], _).

	test(piecewise_costs_infeasible_supplies, false) :-
		build([s,t], [arc(s,t,0,[segment(1,1)])], Network, _),
		min_cost_flow(Network, [s-2,t-(-2)], _, _).

	test(piecewise_costs_invalid_cost_arc, error(domain_error(piecewise_cost_arc, foo))) :-
		build([], [foo], _, _).

	test(piecewise_costs_empty_segments, error(type_error(non_empty_list, []))) :-
		build([a,b], [arc(a,b,0,[])], _, _).

	test(piecewise_costs_invalid_segment_shape, error(domain_error(piecewise_cost_segment, width(2)))) :-
		build([a,b], [arc(a,b,0,[width(2)])], _, _).

	test(piecewise_costs_nonpositive_width, error(domain_error(positive_integer, 0))) :-
		build([a,b], [arc(a,b,0,[segment(0,1)])], _, _).

	test(piecewise_costs_noninteger_width, error(type_error(integer, 1.5))) :-
		build([a,b], [arc(a,b,0,[segment(1.5,1)])], _, _).

	test(piecewise_costs_noninteger_marginal_cost, error(type_error(integer, 1.5))) :-
		build([a,b], [arc(a,b,0,[segment(1,1.5)])], _, _).

	test(piecewise_costs_decreasing_marginal_cost, error(domain_error(non_decreasing_marginal_costs, segment(1,0)))) :-
		build([a,b], [arc(a,b,0,[segment(1,1),segment(1,0)])], _, _).

	test(piecewise_costs_lower_exceeds_capacity, error(consistency_error(piecewise_lower_bound_and_capacity, 3, 2))) :-
		build([a,b], [arc(a,b,3,[segment(2,1)])], _, _).

	test(piecewise_costs_negative_lower_bound, error(domain_error(non_negative_integer, -1))) :-
		build([a,b], [arc(a,b,-1,[segment(1,1)])], _, _).

	test(piecewise_costs_nonground_vertex, error(instantiation_error)) :-
		build([f(_)], [], _, _).

	test(piecewise_costs_non_list_segments, error(type_error(non_empty_list, foo))) :-
		build([a,b], [arc(a,b,0,foo)], _, _).

	test(piecewise_costs_independent_brute_force_oracle, deterministic(Cost == 5)) :-
		CostArcs = [
			arc(s,a,1,[segment(1,-2),segment(1,1)]),
			arc(a,t,0,[segment(2,3)]),
			arc(s,t,0,[segment(1,4)])
		],
		Supplies = [s-2,a-0,t-(-2)],
		oracle_minimum(CostArcs, Supplies, Expected),
		build([s,a,t], CostArcs, Network, Mapping),
		min_cost_flow(Network, Supplies, Cost, ExpandedFlow),
		collapse_flow(Network, Mapping, ExpandedFlow, Flow),
		network_flow_cost(Network, ExpandedFlow, ExpandedCost),
		projected_objective(CostArcs, Flow, ProjectedCost),
		Cost =:= Expected,
		ExpandedCost =:= Expected,
		ProjectedCost =:= Expected.

	% auxiliary predicates

	% Independent exhaustive oracle over original arc amounts and the
	% cumulative piecewise objective, including mandatory lower-bound cost.

	oracle_minimum(CostArcs, Supplies, Minimum) :-
		cost_arc_vertices(CostArcs, Vertices),
		findall(
			Cost,
			(	enumerate_original_amounts(CostArcs, Amounts),
				original_balances(Vertices, CostArcs, Amounts, Balances),
				supplies_match(Balances, Supplies),
				original_objective(CostArcs, Amounts, Cost)
			),
			Costs
		),
		min_list(Costs, Minimum).

	enumerate_original_amounts([], []).
	enumerate_original_amounts([arc(_, _, Lower, Segments)| Arcs], [Amount| Amounts]) :-
		segments_width(Segments, Capacity),
		between(Lower, Capacity, Amount),
		enumerate_original_amounts(Arcs, Amounts).

	original_objective([], [], 0).
	original_objective([arc(_, _, _, Segments)| Arcs], [Amount| Amounts], Cost) :-
		segment_objective(Amount, Segments, ArcCost),
		original_objective(Arcs, Amounts, RestCost),
		Cost is ArcCost + RestCost.

	segment_objective(0, _, 0) :-
		!.
	segment_objective(Amount, [segment(Width, MarginalCost)| Segments], Cost) :-
		Used is min(Amount, Width),
		Remaining is Amount - Used,
		segment_objective(Remaining, Segments, RestCost),
		Cost is Used * MarginalCost + RestCost.

	segments_width([], 0).
	segments_width([segment(Width, _)| Segments], Total) :-
		segments_width(Segments, Rest),
		Total is Width + Rest.

	cost_arc_vertices(CostArcs, Vertices) :-
		cost_arc_endpoints(CostArcs, Endpoints),
		sort(Endpoints, Vertices).

	cost_arc_endpoints([], []).
	cost_arc_endpoints([arc(From, To, _, _)| Arcs], [From,To| Endpoints]) :-
		cost_arc_endpoints(Arcs, Endpoints).

	original_balances([], _, _, []).
	original_balances([Vertex| Vertices], CostArcs, Amounts, [Vertex-Balance| Balances]) :-
		vertex_balance(CostArcs, Amounts, Vertex, 0, Balance),
		original_balances(Vertices, CostArcs, Amounts, Balances).

	vertex_balance([], [], _, Balance, Balance).
	vertex_balance([arc(From, To, _, _)| Arcs], [Amount| Amounts], Vertex, Balance0, Balance) :-
		(	From == To ->
			Balance1 = Balance0
		;	From == Vertex ->
			Balance1 is Balance0 + Amount
		;	To == Vertex ->
			Balance1 is Balance0 - Amount
		;	Balance1 = Balance0
		),
		vertex_balance(Arcs, Amounts, Vertex, Balance1, Balance).

	supplies_match([], _).
	supplies_match([Vertex-Balance| Balances], Supplies) :-
		supply_for_vertex(Supplies, Vertex, Supply),
		Balance =:= Supply,
		supplies_match(Balances, Supplies).

	supply_for_vertex([], _, 0).
	supply_for_vertex([Vertex-Supply| Supplies], Target, Amount) :-
		(	Vertex == Target ->
			Amount = Supply
		;	supply_for_vertex(Supplies, Target, Amount)
		).

	projected_objective(CostArcs, Flow, Cost) :-
		projected_objective(CostArcs, Flow, 1, Cost).

	projected_objective([], _, _, 0).
	projected_objective([arc(_, _, _, Segments)| Arcs], Flow, ArcId, Cost) :-
		projected_amount(ArcId, Flow, Amount),
		segment_objective(Amount, Segments, ArcCost),
		ArcId1 is ArcId + 1,
		projected_objective(Arcs, Flow, ArcId1, RestCost),
		Cost is ArcCost + RestCost.

	projected_amount(ArcId, Flow, Amount) :-
		(	member(flow(ArcId, _, _, Value), Flow) ->
			Amount = Value
		;	Amount = 0
		).

:- end_object.
