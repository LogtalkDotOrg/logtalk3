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


:- object(successive_shortest_paths(_Dictionary_),
	implements(min_cost_flow_protocol),
	imports(flow_support(_Dictionary_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Minimum cost flow using the successive shortest paths algorithm with node potentials (reduced costs), where each shortest path is found using Dijkstra algorithm and a binary heap. Arcs with negative cost are saturated beforehand so that all residual costs are non-negative, which also handles negative cost cycles. Supplies and demands are routed from a super source to a super sink. Integer capacities and costs only.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network (e.g., ``avltree``).'
		],
		see_also is [dinic(_), flow_network(_), successive_shortest_paths]
	]).

	:- uses(_Dictionary_, [
		new/1 as dict_new/1,
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		as_dictionary/2 as dict_as_dictionary/2
	]).

	:- uses(binary_heap_min, [
		new/1 as heap_new/1,
		insert/4 as heap_insert/4,
		delete/4 as heap_delete/4
	]).

	:- uses(type, [
		check/3
	]).

	min_cost_flow(Network, Supplies, Cost, Flow) :-
		min_cost_flow(Network, Supplies, Cost, Flow, _).

	min_cost_flow(Network, Supplies, Cost, Flow, Potentials) :-
		context(Context),
		Network = network(Vertices, _, _, _),
		validate_supplies(Supplies, Vertices, Context),
		super_vertices(Source, Sink),
		check_reserved(Vertices, Source, Sink, Context),
		flow_network(_Dictionary_)::arcs(Network, Arcs),
		saturation_balances(Arcs, Balances0),
		append(Supplies, Balances0, Balances1),
		keysort(Balances1, Sorted),
		sum_balances(Sorted, Balances),
		super_arcs(Balances, Source, Sink, 0, Demand, SuperArcs),
		append(Arcs, SuperArcs, AllArcs),
		flow_network(_Dictionary_)::new(Vertices, AllArcs, Augmented),
		Augmented = network(AllVertices, _, _, Residual0),
		saturate(Arcs, 0, Residual0, Residual1),
		potentials(AllVertices, Potentials0),
		(	Demand =:= 0 ->
			% nothing to route (the super source and sink are not even in the network)
			Residual = Residual1,
			Potentials1 = Potentials0
		;	shortest_paths(Augmented, Source, Sink, Potentials0, 0, Total, Residual1, Residual, Potentials1),
			Total =:= Demand
		),
		^^flow_cost(Augmented, Residual, Cost),
		^^flow_list(Augmented, Residual, Flow0),
		real_flow(Flow0, Source, Sink, Flow),
		project_potentials(Vertices, Potentials1, Potentials).

	min_cost_max_flow(Network, Source, Sink, Value, Cost, Flow) :-
		min_cost_max_flow(Network, Source, Sink, Value, Cost, Flow, _).

	min_cost_max_flow(Network, Source, Sink, Value, Cost, Flow, Potentials) :-
		^^validate_terminals(Network, Source, Sink),
		dinic(_Dictionary_)::max_flow(Network, Source, Sink, Value, _),
		NegatedValue is -Value,
		min_cost_flow(Network, [Source-Value, Sink-NegatedValue], Cost, Flow, Potentials).

	% supplies validation

	validate_supplies(Supplies, Vertices, Context) :-
		check(list, Supplies, Context),
		validate_supplies_(Supplies, Vertices, 0, Context).

	validate_supplies_([], _, Sum, Context) :-
		(	Sum =:= 0 ->
			true
		;	throw(error(domain_error(balanced_supplies, Sum), Context))
		).
	validate_supplies_([Pair| Pairs], Vertices, Sum0, Context) :-
		(	var(Pair) ->
			instantiation_error
		;	Pair = Vertex-Amount ->
			(	var(Vertex) ->
				instantiation_error
			;	true
			),
			check(integer, Amount, Context),
			(	member_vertex(Vertex, Vertices) ->
				true
			;	existence_error(vertex, Vertex)
			),
			Sum1 is Sum0 + Amount,
			validate_supplies_(Pairs, Vertices, Sum1, Context)
		;	throw(error(type_error(list(pair), [Pair| Pairs]), Context))
		).

	member_vertex(Vertex, [Head| Tail]) :-
		(	Vertex == Head ->
			true
		;	member_vertex(Vertex, Tail)
		).

	super_vertices('$flow_super_source', '$flow_super_sink').

	check_reserved(Vertices, Source, Sink, Context) :-
		(	(	member_vertex(Source, Vertices)
			;	member_vertex(Sink, Vertices)
			) ->
			throw(error(domain_error(unreserved_vertices, Vertices), Context))
		;	true
		).

	% balances

	saturation_balances([], []).
	saturation_balances([arc(From, To, Lower, Capacity, Cost)| Arcs], Balances) :-
		(	Lower > 0 ->
			NegatedLower is -Lower,
			Balances = [From-NegatedLower, To-Lower| Balances1]
		;	Balances = Balances1
		),
		ResidualCapacity is Capacity - Lower,
		(	Cost < 0,
			ResidualCapacity > 0 ->
			NegatedCapacity is -ResidualCapacity,
			Balances1 = [From-NegatedCapacity, To-ResidualCapacity| Balances2]
		;	Balances1 = Balances2
		),
		saturation_balances(Arcs, Balances2).

	sum_balances([], []).
	sum_balances([Vertex-Amount| Pairs], Balances) :-
		sum_same(Pairs, Vertex, Amount, Total, Rest),
		(	Total =:= 0 ->
			Balances = Balances1
		;	Balances = [Vertex-Total| Balances1]
		),
		sum_balances(Rest, Balances1).

	sum_same([Vertex1-Amount1| Pairs], Vertex, Amount0, Total, Rest) :-
		Vertex1 == Vertex,
		!,
		Amount is Amount0 + Amount1,
		sum_same(Pairs, Vertex, Amount, Total, Rest).
	sum_same(Pairs, _, Total, Total, Pairs).

	super_arcs([], _, _, Demand, Demand, []).
	super_arcs([Vertex-Balance| Balances], Source, Sink, Demand0, Demand, Arcs) :-
		(	Balance > 0 ->
			Demand1 is Demand0 + Balance,
			Arcs = [arc(Source, Vertex, 0, Balance, 0)| Arcs1]
		;	Demand1 = Demand0,
			Required is -Balance,
			Arcs = [arc(Vertex, Sink, 0, Required, 0)| Arcs1]
		),
		super_arcs(Balances, Source, Sink, Demand1, Demand, Arcs1).

	% negative cost arcs start saturated: the identifier of the forward arc
	% of the I-th arc is 2*I and the identifier of its reverse arc is 2*I+1

	saturate([], _, Residual, Residual).
	saturate([arc(_, _, Lower, Capacity, Cost)| Arcs], Index, Residual0, Residual) :-
		ResidualCapacity is Capacity - Lower,
		(	Cost < 0, ResidualCapacity > 0 ->
			Forward is 2 * Index,
			Reverse is Forward + 1,
			dict_insert(Residual0, Forward, 0, Residual1),
			dict_insert(Residual1, Reverse, ResidualCapacity, Residual2)
		;	Residual2 = Residual0
		),
		Next is Index + 1,
		saturate(Arcs, Next, Residual2, Residual).

	% node potentials

	potentials(Vertices, Potentials) :-
		potential_pairs(Vertices, Pairs),
		dict_as_dictionary(Pairs, Potentials).

	potential_pairs([], []).
	potential_pairs([Vertex| Vertices], [Vertex-0| Pairs]) :-
		potential_pairs(Vertices, Pairs).

	update_potentials(Vertices, Potentials0, Distances, Limit, Potentials) :-
		potentials_update(Vertices, Potentials0, Distances, Limit, Pairs),
		dict_as_dictionary(Pairs, Potentials).

	project_potentials([], _, []).
	project_potentials([Vertex| Vertices], Potentials0, [Vertex-Potential| Potentials]) :-
		dict_lookup(Vertex, Potential, Potentials0),
		project_potentials(Vertices, Potentials0, Potentials).

	potentials_update([], _, _, _, []).
	potentials_update([Vertex| Vertices], Potentials0, Distances, Limit, [Vertex-Potential| Pairs]) :-
		dict_lookup(Vertex, Potential0, Potentials0),
		(	dict_lookup(Vertex, Distance, Distances) ->
			Step is min(Distance, Limit)
		;	Step = Limit
		),
		Potential is Potential0 + Step,
		potentials_update(Vertices, Potentials0, Distances, Limit, Pairs).

	% successive shortest paths

	shortest_paths(Network, Source, Sink, Potentials0, Total0, Total, Residual0, Residual, Potentials) :-
		dijkstra(Network, Source, Sink, Potentials0, Residual0, Distances, Parents),
		(	dict_lookup(Sink, SinkDistance, Distances),
			dict_lookup(Sink, _, Parents) ->
			Network = network(Vertices, _, _, _),
			update_potentials(Vertices, Potentials0, Distances, SinkDistance, Potentials1),
			parent_path(Network, Sink, Source, Parents, [], Ids),
			^^bottleneck(Ids, Residual0, Amount),
			^^augment(Ids, Amount, Residual0, Residual1),
			Total1 is Total0 + Amount,
			shortest_paths(Network, Source, Sink, Potentials1, Total1, Total, Residual1, Residual, Potentials)
		;	Total = Total0,
			Residual = Residual0,
			Potentials = Potentials0
		).

	parent_path(Network, Vertex, Source, Parents, Ids0, Ids) :-
		(	Vertex == Source ->
			Ids = Ids0
		;	dict_lookup(Vertex, Id, Parents),
			Network = network(_, Arcs, _, _),
			dict_lookup(Id, arc(From, _, _, _), Arcs),
			parent_path(Network, From, Source, Parents, [Id| Ids0], Ids)
		).

	dijkstra(Network, Source, Sink, Potentials, Residual, Distances, Parents) :-
		heap_new(Heap0),
		heap_insert(0, Source, Heap0, Heap1),
		dict_new(Distances0),
		dict_insert(Distances0, Source, 0, Distances1),
		dict_new(Parents0),
		dijkstra_loop(Heap1, Network, Sink, Potentials, Residual, Distances1, Distances, Parents0, Parents).

	dijkstra_loop(Heap0, Network, Sink, Potentials, Residual, Distances0, Distances, Parents0, Parents) :-
		(	heap_delete(Heap0, Distance, Vertex, Heap1) ->
			dict_lookup(Vertex, Best, Distances0),
			(	Distance > Best ->
				dijkstra_loop(Heap1, Network, Sink, Potentials, Residual, Distances0, Distances, Parents0, Parents)
			;	Vertex == Sink ->
				Distances = Distances0,
				Parents = Parents0
			;	^^adjacency(Vertex, Network, Ids),
				dict_lookup(Vertex, Potential, Potentials),
				relax(Ids, Network, Distance, Potential, Potentials, Residual, Heap1, Heap2, Distances0, Distances1, Parents0, Parents1),
				dijkstra_loop(Heap2, Network, Sink, Potentials, Residual, Distances1, Distances, Parents1, Parents)
			)
		;	Distances = Distances0,
			Parents = Parents0
		).

	relax([], _, _, _, _, _, Heap, Heap, Distances, Distances, Parents, Parents).
	relax([Id| Ids], Network, Distance, Potential, Potentials, Residual, Heap0, Heap, Distances0, Distances, Parents0, Parents) :-
		Network = network(_, Arcs, _, _),
		dict_lookup(Id, Capacity, Residual),
		(	Capacity > 0 ->
			dict_lookup(Id, arc(_, To, Cost, _), Arcs),
			dict_lookup(To, ToPotential, Potentials),
			New is Distance + Cost + Potential - ToPotential,
			(	dict_lookup(To, Old, Distances0),
				Old =< New ->
				relax(Ids, Network, Distance, Potential, Potentials, Residual, Heap0, Heap, Distances0, Distances, Parents0, Parents)
			;	dict_insert(Distances0, To, New, Distances1),
				dict_insert(Parents0, To, Id, Parents1),
				heap_insert(New, To, Heap0, Heap1),
				relax(Ids, Network, Distance, Potential, Potentials, Residual, Heap1, Heap, Distances1, Distances, Parents1, Parents)
			)
		;	relax(Ids, Network, Distance, Potential, Potentials, Residual, Heap0, Heap, Distances0, Distances, Parents0, Parents)
		).

	real_flow([], _, _, []).
	real_flow([flow(ArcId, From, To, Amount)| Flow0], Source, Sink, Flow) :-
		(	( From == Source ; To == Sink ) ->
			Flow = Flow1
		;	Flow = [flow(ArcId, From, To, Amount)| Flow1]
		),
		real_flow(Flow0, Source, Sink, Flow1).

	append([], List, List).
	append([Head| Tail], List, [Head| Rest]) :-
		append(Tail, List, Rest).

:- end_object.


:- object(successive_shortest_paths,
	extends(successive_shortest_paths(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Minimum cost flow using the successive shortest paths algorithm and the ``avltree`` dictionary.',
		see_also is [successive_shortest_paths(_), dinic]
	]).

:- end_object.
