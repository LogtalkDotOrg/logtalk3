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


:- object(flow_network(_Dictionary_),
	implements(flow_network_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Flow network construction and inspection. The network is an opaque term. Each input arc is stored as a residual pair of arcs: a forward arc with identifier ``2*I`` and a reverse arc with identifier ``2*I+1`` (``I`` being the zero-based position of the arc in the input list), where the reverse arc has zero initial residual capacity and the negated cost. The parametric object parameter is the dictionary object used for the representation and must be the same one used by the flow algorithm objects.',
		parameters is [
			'Dictionary' - 'Dictionary object (e.g., ``avltree``, ``rbtree``, ``bintree``, or ``splaytree``).'
		],
		see_also is [flow_network, max_flow_protocol, min_cost_flow_protocol]
	]).

	:- uses(_Dictionary_, [
		as_dictionary/2 as dict_as_dictionary/2, as_list/2 as dict_as_list/2,
		lookup/3 as dict_lookup/3
	]).

	:- uses(type, [
		check/3, valid/2 as valid_type/2
	]).

	:- uses(list, [
		keysort/2, member/2, memberchk/2
	]).

	% network(Vertices, Arcs, Adjacency, Residual):
	%   Vertices  - sorted list of vertices
	%   Arcs      - dictionary mapping arc identifiers to arc(From, To, Cost, Lower)
	%   Adjacency - dictionary mapping vertices to ascending lists of outgoing arc identifiers
	%   Residual  - dictionary mapping arc identifiers to initial residual capacities

	new(Arcs, network(Vertices, ArcDict, AdjacencyDict, ResidualDict)) :-
		context(Context),
		check(list, Arcs, Context),
		normalize(Arcs, Normalized, Context),
		vertices_of(Normalized, Vertices),
		residual_pairs(Normalized, 0, ArcPairs0, ResidualPairs0, AdjacencyPairs0),
		dict_as_dictionary(ArcPairs0, ArcDict),
		dict_as_dictionary(ResidualPairs0, ResidualDict),
		keysort(AdjacencyPairs0, Sorted),
		group_adjacency(Sorted, AdjacencyPairs),
		dict_as_dictionary(AdjacencyPairs, AdjacencyDict).

	from_graph(Graph, Network) :-
		weighted_directed_graph(_Dictionary_)::edges(Graph, Edges),
		edges_to_arcs(Edges, Arcs),
		new(Arcs, Network).

	vertices(network(Vertices, _, _, _), Vertices).

	arcs(network(_, ArcDict, _, ResidualDict), Arcs) :-
		dict_as_list(ArcDict, ArcPairs),
		forward_arcs(ArcPairs, ResidualDict, Arcs).

	flow_cost(Network, Flow, Cost) :-
		context(Context),
		arcs(Network, Arcs),
		inspect_flow_edges(Flow, Arcs, Edges, Context),
		flow_cost_value(Arcs, Edges, 1, 0, Cost).

	flow_balances(Network, Flow, Balances) :-
		context(Context),
		Network = network(Vertices, _, _, _),
		arcs(Network, Arcs),
		inspect_flow_edges(Flow, Arcs, Edges, Context),
		compute_flow_balances(Vertices, Edges, Balances).

	cut_capacity(Network, SourceSide, Capacity) :-
		context(Context),
		Network = network(Vertices, _, _, _),
		check(list, SourceSide, Context),
		check_cut_vertices(SourceSide, Vertices, [], Context),
		arcs(Network, Arcs),
		(	zero_lower_bounds(Arcs) ->
			true
		;	throw(error(domain_error(zero_lower_bounds, Network), Context))
		),
		measure_cut(Arcs, SourceSide, 0, Capacity, _).

	valid_cut(Network, Source, Sink, Value, Cut) :-
		ground(Source-Sink-Cut),
		integer(Value),
		Value >= 0,
		Source \== Sink,
		Network = network(Vertices, _, _, _),
		memberchk(Source, Vertices),
		memberchk(Sink, Vertices),
		Cut = cut(SourceSide, SinkSide, CutArcs),
		valid_type(list, SourceSide),
		valid_type(list, SinkSide),
		sort(SourceSide, SourceSide),
		sort(SinkSide, SinkSide),
		memberchk(Source, SourceSide),
		memberchk(Sink, SinkSide),
		join_lists(SourceSide, SinkSide, AllVertices),
		sort(AllVertices, Vertices),
		vertex_count(AllVertices, Count),
		vertex_count(Vertices, Count),
		arcs(Network, Arcs),
		zero_lower_bounds(Arcs),
		measure_cut(Arcs, SourceSide, 0, Capacity, ExpectedArcs),
		Value =:= Capacity,
		CutArcs == ExpectedArcs.

	decompose_flow(Network, Flow, Decomposition) :-
		Network = network(Vertices, _, _, _),
		arcs(Network, Arcs),
		flow_edges(Flow, Arcs, Edges0),
		keysort(Edges0, SortedEdges),
		unique_edge_keys(SortedEdges),
		strip_edge_keys(SortedEdges, Edges),
		compute_flow_balances(Vertices, Edges, Balances),
		decompose_paths(Edges, Balances, Paths, RemainingEdges, RemainingBalances),
		decompose_cycles(RemainingEdges, Cycles),
		join_lists(Paths, Cycles, Decomposition),
		balanced(RemainingBalances).

	valid_flow(Network, Supplies, Flow) :-
		Network = network(Vertices, _, _, _),
		valid_supplies(Supplies, Vertices),
		arcs(Network, Arcs),
		capacity_flow_edges(Flow, Arcs, Edges),
		compute_flow_balances(Vertices, Edges, Balances),
		supply_balances(Vertices, Supplies, Expected),
		balances_equal(Balances, Expected).

	valid_st_flow(Network, Source, Sink, Value, Flow) :-
		ground(Source),
		ground(Sink),
		integer(Value),
		Value >= 0,
		Source \== Sink,
		Network = network(Vertices, _, _, _),
		memberchk(Source, Vertices),
		memberchk(Sink, Vertices),
		NegatedValue is -Value,
		valid_flow(Network, [Source-Value, Sink-NegatedValue], Flow).

	valid_min_cost_flow(Network, Supplies, Flow, Cost) :-
		integer(Cost),
		valid_flow(Network, Supplies, Flow),
		Network = network(Vertices, _, _, _),
		arcs(Network, Arcs),
		flow_edges(Flow, Arcs, Edges0),
		keysort(Edges0, SortedEdges),
		strip_edge_keys(SortedEdges, Edges),
		flow_cost_value(Arcs, Edges, 1, 0, ActualCost),
		ActualCost =:= Cost,
		residual_arcs(Arcs, Edges, 1, ResidualArcs),
		vertex_count(Vertices, Count),
		no_negative_cycle(Count, ResidualArcs).

	valid_min_cost_flow(Network, Supplies, Flow, Cost, Potentials) :-
		integer(Cost),
		valid_flow(Network, Supplies, Flow),
		Network = network(Vertices, _, _, _),
		valid_potentials(Vertices, Potentials),
		arcs(Network, Arcs),
		flow_edges(Flow, Arcs, Edges0),
		keysort(Edges0, SortedEdges),
		strip_edge_keys(SortedEdges, Edges),
		flow_cost_value(Arcs, Edges, 1, 0, ActualCost),
		ActualCost =:= Cost,
		residual_arcs(Arcs, Edges, 1, ResidualArcs),
		non_negative_reduced_costs(ResidualArcs, Potentials).

	% auxiliary predicates

	inspect_flow_edges(Flow, Arcs, Edges, Context) :-
		check(list, Flow, Context),
		check_flow_terms(Flow, Arcs, Context),
		(	capacity_flow_edges(Flow, Arcs, Edges) ->
			true
		;	throw(error(domain_error(capacity_valid_flow, Flow), Context))
		).

	check_flow_terms([], _, _).
	check_flow_terms([Term| Flow], Arcs, Context) :-
		(	var(Term) ->
			throw(error(instantiation_error, Context))
		;	Term = flow(ArcId, From, To, Amount) ->
			check(positive_integer, ArcId, Context),
			check(positive_integer, Amount, Context),
			(	ground(From-To) ->
				true
			;	throw(error(instantiation_error, Context))
			),
			(	arc_by_id(Arcs, ArcId, arc(ArcFrom, ArcTo, _, _, _)) ->
				true
			;	throw(error(existence_error(arc, ArcId), Context))
			),
			(	From-To == ArcFrom-ArcTo ->
				true
			;	throw(error(domain_error(flow_arc_endpoints, Term), Context))
			),
			check_flow_terms(Flow, Arcs, Context)
		;	throw(error(domain_error(flow, Term), Context))
		).

	capacity_flow_edges(Flow, Arcs, Edges) :-
		flow_edges(Flow, Arcs, Edges0),
		keysort(Edges0, SortedEdges),
		unique_edge_keys(SortedEdges),
		strip_edge_keys(SortedEdges, Edges),
		flow_capacities(Arcs, Edges, 1).

	check_cut_vertices([], _, _, _).
	check_cut_vertices([Vertex| SourceSide], Vertices, Seen, Context) :-
		(	ground(Vertex) ->
			true
		;	throw(error(instantiation_error, Context))
		),
		(	memberchk(Vertex, Vertices) ->
			true
		;	throw(error(existence_error(vertex, Vertex), Context))
		),
		(	memberchk(Vertex, Seen) ->
			throw(error(domain_error(unique_cut_vertices, Vertex), Context))
		;	true
		),
		check_cut_vertices(SourceSide, Vertices, [Vertex| Seen], Context).

	zero_lower_bounds([]).
	zero_lower_bounds([arc(_, _, Lower, _, _)| Arcs]) :-
		Lower =:= 0,
		zero_lower_bounds(Arcs).

	measure_cut([], _, Capacity, Capacity, []).
	measure_cut([arc(From, To, _, Capacity, _)| Arcs], SourceSide, Capacity0, Total, CutArcs) :-
		(	Capacity > 0,
			memberchk(From, SourceSide),
			\+ member(To, SourceSide) ->
			Capacity1 is Capacity0 + Capacity,
			CutArcs = [arc(From, To, Capacity)| CutArcs1]
		;	Capacity1 = Capacity0,
			CutArcs = CutArcs1
		),
		measure_cut(Arcs, SourceSide, Capacity1, Total, CutArcs1).

	edges_to_arcs([], []).
	edges_to_arcs([(From-To)-Capacity| Edges], [arc(From, To, 0, Capacity, 0)| Arcs]) :-
		edges_to_arcs(Edges, Arcs).

	normalize([], [], _).
	normalize([Arc| Arcs], [arc(From, To, Lower, Capacity, Cost)| Normalized], Context) :-
		normalize_arc(Arc, From, To, Lower, Capacity, Cost, Context),
		normalize(Arcs, Normalized, Context).

	normalize_arc(Arc, _, _, _, _, _, Context) :-
		var(Arc),
		throw(error(instantiation_error, Context)).
	normalize_arc(arc(From, To, Capacity), From, To, 0, Capacity, 0, Context) :-
		!,
		validate(From, To, 0, Capacity, 0, Context).
	normalize_arc(arc(From, To, Capacity, Cost), From, To, 0, Capacity, Cost, Context) :-
		!,
		validate(From, To, 0, Capacity, Cost, Context).
	normalize_arc(arc(From, To, Lower, Capacity, Cost), From, To, Lower, Capacity, Cost, Context) :-
		!,
		validate(From, To, Lower, Capacity, Cost, Context).
	normalize_arc(Arc, _, _, _, _, _, Context) :-
		throw(error(domain_error(flow_arc, Arc), Context)).

	validate(From, To, Lower, Capacity, Cost, Context) :-
		(	ground(From), ground(To) ->
			true
		;	instantiation_error
		),
		check(non_negative_integer, Lower, Context),
		check(non_negative_integer, Capacity, Context),
		(	Lower =< Capacity ->
			true
		;	throw(error(consistency_error(arc_lower_bound_and_capacity, Lower, Capacity), Context))
		),
		check(integer, Cost, Context).

	vertices_of(Arcs, Vertices) :-
		endpoints(Arcs, Endpoints),
		sort(Endpoints, Vertices).

	endpoints([], []).
	endpoints([arc(From, To, _, _, _)| Arcs], [From, To| Endpoints]) :-
		endpoints(Arcs, Endpoints).

	residual_pairs([], _, [], [], []).
	residual_pairs([arc(From, To, Lower, Capacity, Cost)| Arcs], Index, ArcPairs, ResidualPairs, AdjacencyPairs) :-
		Forward is 2 * Index,
		Reverse is Forward + 1,
		NegatedCost is -Cost,
		ResidualCapacity is Capacity - Lower,
		ArcPairs = [Forward-arc(From, To, Cost, Lower), Reverse-arc(To, From, NegatedCost, 0)| ArcPairs1],
		ResidualPairs = [Forward-ResidualCapacity, Reverse-0| ResidualPairs1],
		AdjacencyPairs = [From-Forward, To-Reverse| AdjacencyPairs1],
		Next is Index + 1,
		residual_pairs(Arcs, Next, ArcPairs1, ResidualPairs1, AdjacencyPairs1).

	% the keysort/2 predicate is stable, thus identifiers are ascending within a vertex

	group_adjacency([], []).
	group_adjacency([Vertex-Id| Pairs], [Vertex-[Id| Ids]| Groups]) :-
		same_vertex(Pairs, Vertex, Ids, Rest),
		group_adjacency(Rest, Groups).

	same_vertex([Vertex1-Id| Pairs], Vertex, [Id| Ids], Rest) :-
		Vertex1 == Vertex,
		!,
		same_vertex(Pairs, Vertex, Ids, Rest).
	same_vertex(Pairs, _, [], Pairs).

	forward_arcs([], _, []).
	forward_arcs([Id-arc(From, To, Cost, Lower)| Pairs], ResidualDict, Arcs) :-
		(	0 =:= Id mod 2 ->
			dict_lookup(Id, ResidualCapacity, ResidualDict),
			Capacity is Lower + ResidualCapacity,
			Arcs = [arc(From, To, Lower, Capacity, Cost)| Arcs1]
		;	Arcs = Arcs1
		),
		forward_arcs(Pairs, ResidualDict, Arcs1).

	flow_edges([], _, []).
	flow_edges([flow(ArcId, From, To, Amount)| Flow], Arcs, [ArcId-edge(From, To, Amount)| Edges]) :-
		integer(ArcId),
		ArcId > 0,
		integer(Amount),
		Amount > 0,
		arc_by_id(Arcs, ArcId, arc(From, To, _, _, _)),
		flow_edges(Flow, Arcs, Edges).

	unique_edge_keys([]).
	unique_edge_keys([_-_]) :-
		!.
	unique_edge_keys([Id-_, Id1-_| Pairs]) :-
		Id =\= Id1,
		unique_edge_keys([Id1-_| Pairs]).

	arc_by_id([Arc| _], 1, Arc) :-
		!.
	arc_by_id([_| Arcs], ArcId, Arc) :-
		ArcId > 1,
		ArcId1 is ArcId - 1,
		arc_by_id(Arcs, ArcId1, Arc).

	strip_edge_keys([], []).
	strip_edge_keys([ArcId-edge(From, To, Amount)| Pairs], [edge(ArcId, From, To, Amount)| Edges]) :-
		strip_edge_keys(Pairs, Edges).

	compute_flow_balances([], _, []).
	compute_flow_balances([Vertex| Vertices], Edges, [Vertex-Balance| Balances]) :-
		vertex_balance(Edges, Vertex, 0, Balance),
		compute_flow_balances(Vertices, Edges, Balances).

	vertex_balance([], _, Balance, Balance).
	vertex_balance([edge(_, From, To, Amount)| Edges], Vertex, Balance0, Balance) :-
		(	From == Vertex, To == Vertex ->
			Balance1 = Balance0
		;	From == Vertex ->
			Balance1 is Balance0 + Amount
		;	To == Vertex ->
			Balance1 is Balance0 - Amount
		;	Balance1 = Balance0
		),
		vertex_balance(Edges, Vertex, Balance1, Balance).

	decompose_paths(Edges, Balances, Paths, RemainingEdges, RemainingBalances) :-
		(	positive_balance(Balances, Source, Supply) ->
			find_balance_path(Source, Balances, Edges, [Source], Vertices, ArcIds, Sink),
			balance_of(Balances, Sink, Demand),
			Limit is min(Supply, -Demand),
			edge_bottleneck(ArcIds, Edges, Limit, Amount),
			subtract_edges(Edges, ArcIds, Amount, Edges1),
			update_balance(Balances, Source, -Amount, Balances1),
			update_balance(Balances1, Sink, Amount, Balances2),
			Paths = [path(Vertices, ArcIds, Amount)| Paths1],
			decompose_paths(Edges1, Balances2, Paths1, RemainingEdges, RemainingBalances)
		;	Paths = [],
			RemainingEdges = Edges,
			RemainingBalances = Balances
		).

	positive_balance([Vertex-Amount| _], Vertex, Amount) :-
		Amount > 0,
		!.
	positive_balance([_| Balances], Vertex, Amount) :-
		positive_balance(Balances, Vertex, Amount).

	balance_of([Vertex-Amount| _], Vertex, Amount) :-
		!.
	balance_of([_| Balances], Vertex, Amount) :-
		balance_of(Balances, Vertex, Amount).

	find_balance_path(Vertex, Balances, _, _, [Vertex], [], Vertex) :-
		balance_of(Balances, Vertex, Balance),
		Balance < 0,
		!.
	find_balance_path(Vertex, Balances, Edges, Visited, [Vertex| Vertices], [ArcId| ArcIds], Sink) :-
		select_positive_edge(Vertex, Edges, ArcId, To),
		\+ member(To, Visited),
		find_balance_path(To, Balances, Edges, [To| Visited], Vertices, ArcIds, Sink),
		!.

	select_positive_edge(Vertex, [edge(ArcId, From, To, Amount)| _], ArcId, To) :-
		From == Vertex,
		Amount > 0.
	select_positive_edge(Vertex, [_| Edges], ArcId, To) :-
		select_positive_edge(Vertex, Edges, ArcId, To).

	edge_bottleneck([], _, Amount, Amount).
	edge_bottleneck([ArcId| ArcIds], Edges, Limit, Amount) :-
		edge_amount(ArcId, Edges, Capacity),
		Limit1 is min(Limit, Capacity),
		edge_bottleneck(ArcIds, Edges, Limit1, Amount).

	edge_amount(ArcId, [edge(ArcId, _, _, Amount)| _], Amount) :-
		!.
	edge_amount(ArcId, [_| Edges], Amount) :-
		edge_amount(ArcId, Edges, Amount).

	subtract_edges([], _, _, []).
	subtract_edges([edge(ArcId, From, To, Amount0)| Edges], ArcIds, SubtractAmount, [edge(ArcId, From, To, Amount)| Edges1]) :-
		(	member(ArcId, ArcIds) ->
			Amount is Amount0 - SubtractAmount
		;	Amount = Amount0
		),
		subtract_edges(Edges, ArcIds, SubtractAmount, Edges1).

	update_balance([], _, _, []).
	update_balance([Vertex-Balance0| Balances], Target, Change, [Vertex-Balance| Balances1]) :-
		(	Vertex == Target ->
			Balance is Balance0 + Change
		;	Balance = Balance0
		),
		update_balance(Balances, Target, Change, Balances1).

	decompose_cycles(Edges, Cycles) :-
		(	positive_edge(Edges, ArcId, From, To, Capacity) ->
			(	From == To ->
				Vertices = [From, From],
				ArcIds = [ArcId],
				Amount = Capacity
			;	find_target_path(To, From, Edges, [To], PathVertices, PathIds),
				Vertices = [From| PathVertices],
				ArcIds = [ArcId| PathIds],
				edge_bottleneck(PathIds, Edges, Capacity, Amount)
			),
			subtract_edges(Edges, ArcIds, Amount, Edges1),
			Cycles = [cycle(Vertices, ArcIds, Amount)| Cycles1],
			decompose_cycles(Edges1, Cycles1)
		;	Cycles = []
		).

	positive_edge([edge(ArcId, From, To, Amount)| _], ArcId, From, To, Amount) :-
		Amount > 0,
		!.
	positive_edge([_| Edges], ArcId, From, To, Amount) :-
		positive_edge(Edges, ArcId, From, To, Amount).

	find_target_path(Vertex, Target, _, _, [Vertex], []) :-
		Vertex == Target,
		!.
	find_target_path(Vertex, Target, Edges, Visited, [Vertex| Vertices], [ArcId| ArcIds]) :-
		select_positive_edge(Vertex, Edges, ArcId, To),
		\+ member(To, Visited),
		find_target_path(To, Target, Edges, [To| Visited], Vertices, ArcIds),
		!.

	join_lists([], List, List).
	join_lists([Head| Tail], List, [Head| Rest]) :-
		join_lists(Tail, List, Rest).

	balanced([]).
	balanced([_-Balance| Balances]) :-
		Balance =:= 0,
		balanced(Balances).

	valid_supplies([], _).
	valid_supplies([Vertex-Amount| Supplies], Vertices) :-
		ground(Vertex),
		integer(Amount),
		memberchk(Vertex, Vertices),
		valid_supplies(Supplies, Vertices).

	supply_balances([], _, []).
	supply_balances([Vertex| Vertices], Supplies, [Vertex-Balance| Balances]) :-
		supply_balance(Supplies, Vertex, 0, Balance),
		supply_balances(Vertices, Supplies, Balances).

	supply_balance([], _, Balance, Balance).
	supply_balance([Vertex-Amount| Supplies], Target, Balance0, Balance) :-
		(	Vertex == Target ->
			Balance1 is Balance0 + Amount
		;	Balance1 = Balance0
		),
		supply_balance(Supplies, Target, Balance1, Balance).

	balances_equal([], []).
	balances_equal([Vertex-Balance| Balances], [Vertex-Expected| ExpectedBalances]) :-
		Balance =:= Expected,
		balances_equal(Balances, ExpectedBalances).

	flow_capacities([], _, _).
	flow_capacities([arc(_, _, Lower, Capacity, _)| Arcs], Edges, ArcId) :-
		(	edge_amount(ArcId, Edges, Amount) ->
			Amount >= Lower,
			Amount =< Capacity
		;	Lower =:= 0
		),
		Next is ArcId + 1,
		flow_capacities(Arcs, Edges, Next).

	flow_cost_value([], _, _, Cost, Cost).
	flow_cost_value([arc(_, _, _, _, ArcCost)| Arcs], Edges, ArcId, Cost0, Cost) :-
		(	edge_amount(ArcId, Edges, Amount) ->
			Cost1 is Cost0 + Amount * ArcCost
		;	Cost1 = Cost0
		),
		Next is ArcId + 1,
		flow_cost_value(Arcs, Edges, Next, Cost1, Cost).

	residual_arcs([], _, _, []).
	residual_arcs([arc(From, To, Lower, Capacity, Cost)| Arcs], Edges, ArcId, ResidualArcs) :-
		(	edge_amount(ArcId, Edges, Amount) ->
			ForwardCapacity is Capacity - Amount,
			(	ForwardCapacity > 0 ->
				ResidualArcs = [residual_arc(From, To, Cost)| ResidualArcs1]
			;	ResidualArcs = ResidualArcs1
			),
			ReverseCapacity is Amount - Lower,
			(	ReverseCapacity > 0 ->
				ResidualArcs1 = [residual_arc(To, From, NegatedCost)| ResidualArcs2],
				NegatedCost is -Cost
			;	ResidualArcs1 = ResidualArcs2
			)
		;	Capacity > 0 ->
			ResidualArcs = [residual_arc(From, To, Cost)| ResidualArcs1],
			ResidualArcs1 = ResidualArcs2
		;	ResidualArcs = ResidualArcs2
		),
		Next is ArcId + 1,
		residual_arcs(Arcs, Edges, Next, ResidualArcs2).

	vertex_count([], 0).
	vertex_count([_| Vertices], Count) :-
		vertex_count(Vertices, Count0),
		Count is Count0 + 1.

	no_negative_cycle(0, _).
	no_negative_cycle(Count, Arcs) :-
		Count > 0,
		zero_distances(Arcs, Distances),
		bellman_ford(Count, Arcs, Distances).

	zero_distances([], []).
	zero_distances([residual_arc(From, To, _)| Arcs], Distances) :-
		zero_distances(Arcs, Distances0),
		add_zero_distance(From, Distances0, Distances1),
		add_zero_distance(To, Distances1, Distances).

	add_zero_distance(Vertex, Distances, Distances) :-
		distance(Vertex, Distances, _),
		!.
	add_zero_distance(Vertex, Distances, [Vertex-0| Distances]).

	bellman_ford(Count, Arcs, Distances0) :-
		relax_residual_arcs(Arcs, Distances0, Distances, Changed),
		(	Changed == false ->
			true
		;	Count =:= 1 ->
			fail
		;	Count1 is Count - 1,
			bellman_ford(Count1, Arcs, Distances)
		).

	relax_residual_arcs([], Distances, Distances, false).
	relax_residual_arcs([residual_arc(From, To, Cost)| Arcs], Distances0, Distances, Changed) :-
		distance(From, Distances0, FromDistance),
		distance(To, Distances0, ToDistance),
		Candidate is FromDistance + Cost,
		(	Candidate < ToDistance ->
			update_distance(Distances0, To, Candidate, Distances1),
			Changed1 = true
		;	Distances1 = Distances0,
			Changed1 = false
		),
		relax_residual_arcs(Arcs, Distances1, Distances, Changed2),
		(	(	Changed1 == true
			;	Changed2 == true
			) ->
			Changed = true
		;	Changed = false
		).

	distance(Vertex, [Vertex0-Distance| _], Distance) :-
		Vertex == Vertex0,
		!.
	distance(Vertex, [_| Distances], Distance) :-
		distance(Vertex, Distances, Distance).

	update_distance([], Vertex, Distance, [Vertex-Distance]).
	update_distance([Vertex0-_| Distances], Vertex, Distance, [Vertex-Distance| Distances]) :-
		Vertex == Vertex0,
		!.
	update_distance([Pair| Distances0], Vertex, Distance, [Pair| Distances]) :-
		update_distance(Distances0, Vertex, Distance, Distances).

	valid_potentials([], []).
	valid_potentials([Vertex| Vertices], [Vertex-Potential| Potentials]) :-
		integer(Potential),
		valid_potentials(Vertices, Potentials).

	non_negative_reduced_costs([], _).
	non_negative_reduced_costs([residual_arc(From, To, Cost)| Arcs], Potentials) :-
		potential(From, Potentials, FromPotential),
		potential(To, Potentials, ToPotential),
		ReducedCost is Cost + FromPotential - ToPotential,
		ReducedCost >= 0,
		non_negative_reduced_costs(Arcs, Potentials).

	potential(Vertex, [Vertex0-Potential| _], Potential) :-
		Vertex == Vertex0,
		!.
	potential(Vertex, [_| Potentials], Potential) :-
		potential(Vertex, Potentials, Potential).

:- end_object.


:- object(flow_network,
	extends(flow_network(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Flow network construction and inspection using the ``avltree`` dictionary.',
		see_also is [flow_network(_)]
	]).

:- end_object.
