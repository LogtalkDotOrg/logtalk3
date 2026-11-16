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


:- object(piecewise_costs(_Dictionary_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Builds ordinary flow networks that model convex piecewise-linear integer arc costs by expanding each cost segment into a linear-cost arc. The returned mapping is opaque and records the public vertices and arcs of the expanded network.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the expanded flow network (e.g., ``avltree``).'
		],
		see_also is [flow_network(_), successive_shortest_paths(_)]
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(list, [
		append/3, member/2
	]).

	:- uses(flow_network(_Dictionary_), [
		new/3 as network_new/3, vertices/2 as network_vertices/2, arcs/2 as network_arcs/2,
		flow_cost/3 as network_flow_cost/3
	]).

	:- public(build/4).
	:- mode(build(+list(ground), +list(compound), -compound, -compound), one_or_error).
	:- info(build/4, [
		comment is 'Expands convex cost arcs ``arc(From, To, Lower, Segments)`` where each segment is ``segment(Width, MarginalCost)``. Widths are positive integers and marginal costs are nondecreasing integers; the original cost at flow amount ``X`` is the sum of the marginal costs of its first ``X`` units, with cost zero at amount zero. Each expanded arc receives the lower-bound portion of the original flow in segment order. The opaque mapping records a ground public network snapshot (vertices and arcs), and preserves original arc and segment order.',
		argnames is ['Vertices', 'CostArcs', 'Network', 'Mapping'],
		exceptions is [
			'An input list is partial, or a vertex is not ground' - instantiation_error,
			'An input is not a list' - type_error(list, 'Input'),
			'A cost arc is not an ``arc/4`` term' - domain_error(piecewise_cost_arc, 'Arc'),
			'A segment is not a ``segment/2`` term' - domain_error(piecewise_cost_segment, 'Segment'),
			'A lower bound is not an integer' - type_error(integer, 'Lower'),
			'A lower bound is negative' - domain_error(non_negative_integer, 'Lower'),
			'A lower bound exceeds the total segment width' - consistency_error(piecewise_lower_bound_and_capacity, 'Lower', 'Capacity'),
			'A segment width is not an integer' - type_error(integer, 'Width'),
			'A segment width is not positive' - domain_error(positive_integer, 'Width'),
			'A marginal cost is not an integer' - type_error(integer, 'MarginalCost'),
			'Segment marginal costs decrease' - domain_error(non_decreasing_marginal_costs, 'Segments'),
			'A cost arc has no segments' - domain_error(non_empty_list, 'Segments')
		]
	]).

	:- public(collapse_flow/4).
	:- mode(collapse_flow(+compound, +compound, +list(compound), -list(compound)), one_or_error).
	:- info(collapse_flow/4, [
		comment is 'Projects a capacity-valid flow on the expanded network to original ``flow(ArcId, From, To, Amount)`` entries, omitting zero flows. Rejects a noncanonical segment allocation when a more expensive segment carries optional flow while a cheaper segment has unused capacity. Arbitrary distribution across equal-cost segments is accepted. The original convex arcs are not themselves a core flow network.',
		argnames is ['Network', 'Mapping', 'ExpandedFlow', 'Flow'],
		exceptions is [
			'The mapping is not ground' - instantiation_error,
			'The mapping is malformed' - domain_error(piecewise_cost_mapping, 'Mapping'),
			'The network vertices or arcs differ from the mapped snapshot' - consistency_error(piecewise_mapping_network, 'Snapshot', 'MappedSnapshot'),
			'The expanded flow is a variable or partial list, or an entry has a variable field' - instantiation_error,
			'The expanded flow is not a list' - type_error(list, 'ExpandedFlow'),
			'An expanded-flow entry is malformed' - domain_error(flow, 'Term'),
			'An expanded arc ID or flow amount is not an integer' - type_error(integer, 'Value'),
			'An expanded arc ID or flow amount is not positive' - domain_error(positive_integer, 'Value'),
			'An expanded arc ID is not in the network' - existence_error(arc, 'ArcId'),
			'An expanded-flow entry has incorrect endpoints' - domain_error(flow_arc_endpoints, 'Term'),
			'The expanded flow violates an arc bound or repeats an arc ID' - domain_error(capacity_valid_flow, 'ExpandedFlow'),
			'The segment allocation is not canonical' - domain_error(canonical_piecewise_flow, 'ExpandedFlow')
		]
	]).

	build(Vertices, CostArcs, Network, piecewise_costs_mapping(snapshot(NetworkVertices, ExpandedArcs), Entries)) :-
		context(Context),
		check(list(ground), Vertices, Context),
		check(list(ground), CostArcs, Context),
		validate_cost_arcs(CostArcs, Normalized, Context),
		expand_cost_arcs(Normalized, 1, 1, ExpandedArcs, Entries),
		network_new(Vertices, ExpandedArcs, Network),
		network_vertices(Network, NetworkVertices).

	collapse_flow(Network, Mapping, ExpandedFlow, Flow) :-
		context(Context),
		check(compound, Network, Context),
		(	ground(Mapping) ->
			true
		;	throw(error(instantiation_error, Context))
		),
		check(list, ExpandedFlow, Context),
		validate_mapping(Network, Mapping, Entries, Context),
		network_flow_cost(Network, ExpandedFlow, _),
		(	canonical_flow(Entries, ExpandedFlow) ->
			collapse_entries(Entries, ExpandedFlow, Flow)
		;	throw(error(domain_error(canonical_piecewise_flow, ExpandedFlow), Context))
		).

	validate_cost_arcs([], [], _).
	validate_cost_arcs([Arc| Arcs], [cost_arc(From, To, Lower, Segments)| Normalized], Context) :-
		(	Arc = arc(From, To, Lower, RawSegments) ->
			check(non_negative_integer, Lower, Context),
			check(non_empty_list, RawSegments, Context),
			validate_segments(RawSegments, Segments, 0, _, none, Context),
			total_width(Segments, 0, Capacity),
			(	Lower =< Capacity ->
				true
			;	throw(error(consistency_error(piecewise_lower_bound_and_capacity, Lower, Capacity), Context))
			),
			validate_cost_arcs(Arcs, Normalized, Context)
		;	throw(error(domain_error(piecewise_cost_arc, Arc), Context))
		).

	validate_segments([], [], Width, Width, _, _).
	validate_segments([Segment| Segments], [segment(Width, Cost)| Normalized], Width0, Total, PreviousCost, Context) :-
		(	Segment = segment(Width, Cost) ->
			check(positive_integer, Width, Context),
			check(integer, Cost, Context),
			(	PreviousCost == none ->
				true
			;	PreviousCost = previous(Previous),
				(	Cost >= Previous ->
					true
				;	throw(error(domain_error(non_decreasing_marginal_costs, Segment), Context))
				)
			),
			Width1 is Width0 + Width,
			validate_segments(Segments, Normalized, Width1, Total, previous(Cost), Context)
		;	throw(error(domain_error(piecewise_cost_segment, Segment), Context))
		).

	total_width([], Total, Total).
	total_width([segment(Width, _)| Segments], Total0, Total) :-
		Total1 is Total0 + Width,
		total_width(Segments, Total1, Total).

	expand_cost_arcs([], _, _, [], []).
	expand_cost_arcs([cost_arc(From, To, Lower, Segments)| Arcs], ArcId, SegmentId0, ExpandedArcs, [original(ArcId, From, To, Lower, MappedSegments)| Entries]) :-
		expand_segments(Segments, From, To, Lower, 0, SegmentId0, ExpandedArcs, ExpandedRest, MappedSegments, SegmentId),
		ArcId1 is ArcId + 1,
		expand_cost_arcs(Arcs, ArcId1, SegmentId, ExpandedRest, Entries).

	expand_segments([], _, _, _, _, SegmentId, ExpandedArcs, ExpandedArcs, [], SegmentId).
	expand_segments([segment(Width, Cost)| Segments], From, To, Lower, Prefix, SegmentId0, [arc(From, To, SegmentLower, Width, Cost)| ExpandedArcs], ExpandedRest, [segment(SegmentId0, Width, SegmentLower, Cost)| MappedSegments], SegmentId) :-
		Remaining is Lower - Prefix,
		(	Remaining =< 0 ->
			SegmentLower = 0
		;	Remaining >= Width ->
			SegmentLower = Width
		;	SegmentLower = Remaining
		),
		Prefix1 is Prefix + Width,
		SegmentId1 is SegmentId0 + 1,
		expand_segments(Segments, From, To, Lower, Prefix1, SegmentId1, ExpandedArcs, ExpandedRest, MappedSegments, SegmentId).

	validate_mapping(Network, piecewise_costs_mapping(MappedSnapshot, Entries), Entries, Context) :-
		!,
		network_vertices(Network, Vertices),
		network_arcs(Network, ActualArcs),
		Snapshot = snapshot(Vertices, ActualArcs),
		(	Snapshot == MappedSnapshot ->
			true
		;	throw(error(consistency_error(piecewise_mapping_network, Snapshot, MappedSnapshot), Context))
		),
		(	valid_mapping_entries(Entries, 1, 1, ExpectedArcs) ->
			true
		;	throw(error(domain_error(piecewise_cost_mapping, piecewise_costs_mapping(MappedSnapshot, Entries)), Context))
		),
		(	ActualArcs == ExpectedArcs ->
			true
		;	throw(error(domain_error(piecewise_cost_mapping, piecewise_costs_mapping(MappedSnapshot, Entries)), Context))
		).
	validate_mapping(_, Mapping, _, Context) :-
		throw(error(domain_error(piecewise_cost_mapping, Mapping), Context)).

	valid_mapping_entries([], _, _, []).
	valid_mapping_entries([original(ArcId, From, To, Lower, Segments)| Entries], ArcId, SegmentId0, Arcs) :-
		ground(From-To),
		integer(Lower),
		Lower >= 0,
		Segments = [_| _],
		valid_mapping_segments(Segments, SegmentId0, 0, Lower, none, From, To, MappedArcs, SegmentId),
		total_mapping_width(Segments, 0, Width),
		Lower =< Width,
		total_mapping_lower(Segments, 0, SegmentLower),
		SegmentLower =:= Lower,
		ArcId1 is ArcId + 1,
		append(MappedArcs, RestArcs, Arcs),
		valid_mapping_entries(Entries, ArcId1, SegmentId, RestArcs).

	valid_mapping_segments([], SegmentId, _, _, _, _, _, [], SegmentId).
	valid_mapping_segments([segment(SegmentId, Width, Lower, Cost)| Segments], SegmentId, Prefix, OriginalLower, PreviousCost, From, To, [arc(From, To, Lower, Width, Cost)| Arcs], NextSegmentId) :-
		integer(Width),
		Width > 0,
		integer(Lower),
		Lower >= 0,
		Lower =< Width,
		integer(Cost),
		Remaining is OriginalLower - Prefix,
		(	Remaining =< 0 ->
			ExpectedLower = 0
		;	Remaining >= Width ->
			ExpectedLower = Width
		;	ExpectedLower = Remaining
		),
		Lower =:= ExpectedLower,
		(	PreviousCost == none ->
			true
		;	PreviousCost = previous(Previous),
			Cost >= Previous
		),
		Prefix1 is Prefix + Width,
		SegmentId1 is SegmentId + 1,
		valid_mapping_segments(Segments, SegmentId1, Prefix1, OriginalLower, previous(Cost), From, To, Arcs, NextSegmentId).

	total_mapping_width([], Total, Total).
	total_mapping_width([segment(_, Width, _, _)| Segments], Total0, Total) :-
		Total1 is Total0 + Width,
		total_mapping_width(Segments, Total1, Total).

	total_mapping_lower([], Total, Total).
	total_mapping_lower([segment(_, _, Lower, _)| Segments], Total0, Total) :-
		Total1 is Total0 + Lower,
		total_mapping_lower(Segments, Total1, Total).

	canonical_flow([], _).
	canonical_flow([original(_, _, _, _, Segments)| Entries], ExpandedFlow) :-
		highest_optional_cost(Segments, ExpandedFlow, none, HighestCost),
		canonical_segments(Segments, HighestCost, ExpandedFlow),
		canonical_flow(Entries, ExpandedFlow).

	highest_optional_cost([], _, HighestCost, HighestCost).
	highest_optional_cost([segment(Id, _, Lower, Cost)| Segments], ExpandedFlow, HighestCost0, HighestCost) :-
		flow_amount(Id, ExpandedFlow, Amount),
		(	Amount > Lower ->
			HighestCost1 = cost(Cost)
		;	HighestCost1 = HighestCost0
		),
		highest_optional_cost(Segments, ExpandedFlow, HighestCost1, HighestCost).

	canonical_segments([], _, _).
	canonical_segments([segment(Id, Width, _, Cost)| Segments], HighestCost, ExpandedFlow) :-
		(	HighestCost = cost(Highest),
			Cost < Highest ->
			flow_amount(Id, ExpandedFlow, Amount),
			(	Amount =:= Width ->
				true
			;	fail
			)
		;	true
		),
		canonical_segments(Segments, HighestCost, ExpandedFlow).

	collapse_entries([], _, []).
	collapse_entries([original(ArcId, From, To, _, Segments)| Entries], ExpandedFlow, Flow) :-
		sum_segment_amounts(Segments, ExpandedFlow, 0, Amount),
		(	Amount > 0 ->
			Flow = [flow(ArcId, From, To, Amount)| Flow1]
		;	Flow = Flow1
		),
		collapse_entries(Entries, ExpandedFlow, Flow1).

	sum_segment_amounts([], _, Amount, Amount).
	sum_segment_amounts([segment(Id, _, _, _)| Segments], ExpandedFlow, Amount0, Amount) :-
		flow_amount(Id, ExpandedFlow, SegmentAmount),
		Amount1 is Amount0 + SegmentAmount,
		sum_segment_amounts(Segments, ExpandedFlow, Amount1, Amount).

	flow_amount(Id, Flow, Amount) :-
		(	member(flow(Id, _, _, Value), Flow) ->
			Amount = Value
		;	Amount = 0
		).

:- end_object.


:- object(piecewise_costs,
	extends(piecewise_costs(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Convex piecewise-linear integer cost network builder using the ``avltree`` dictionary.',
		see_also is [piecewise_costs(_), successive_shortest_paths]
	]).

:- end_object.
