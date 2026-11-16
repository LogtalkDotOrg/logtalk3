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


:- category(max_flow_common(_Dictionary_),
	implements(max_flow_protocol),
	extends(flow_support(_Dictionary_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Maximum flow and minimum cut predicates shared by the maximum flow algorithm objects. Importing objects must define the ``maximize/5`` predicate.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network.'
		]
	]).

	:- uses(_Dictionary_, [
		lookup/3 as dict_lookup/3, keys/2 as dict_keys/2, as_list/2 as dict_as_list/2
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- protected(maximize/5).
	:- mode(maximize(+compound, +ground, +ground, -integer, -compound), one).
	:- info(maximize/5, [
		comment is 'Runs the maximum flow algorithm returning the flow value and the final residual dictionary. Must be defined by the importing objects.',
		argnames is ['Network', 'Source', 'Sink', 'Value', 'Residual']
	]).

	max_flow(Network, Source, Sink, Value, Flow) :-
		::validate_terminals(Network, Source, Sink),
		validate_lower_bounds(Network),
		::maximize(Network, Source, Sink, Value, Residual),
		::flow_list(Network, Residual, Flow).

	min_cut(Network, Source, Sink, Value, cut(SourceSide, SinkSide, CutArcs)) :-
		::validate_terminals(Network, Source, Sink),
		validate_lower_bounds(Network),
		::maximize(Network, Source, Sink, Value, Residual),
		::bfs(Network, Residual, Source, all, Info),
		dict_keys(Info, SourceSide0),
		sort(SourceSide0, SourceSide),
		Network = network(Vertices, Arcs, _, ResidualDict),
		sink_side(Vertices, Info, SinkSide),
		dict_as_list(Arcs, Pairs),
		cut_arcs(Pairs, Info, ResidualDict, CutArcs).

	sink_closest_min_cut(Network, Source, Sink, Value, cut(SourceSide, SinkSide, CutArcs)) :-
		::validate_terminals(Network, Source, Sink),
		validate_lower_bounds(Network),
		::maximize(Network, Source, Sink, Value, Residual),
		Network = network(Vertices, Arcs, _, InitialResidual),
		residual_graph(Vertices, Arcs, Residual, ResidualGraph),
		weighted_directed_graph(_Dictionary_)::transpose(ResidualGraph, ReverseGraph),
		weighted_directed_graph(_Dictionary_)::reachable(Sink, ReverseGraph, SinkSide0),
		sort(SinkSide0, SinkSide),
		complement(Vertices, SinkSide, SourceSide),
		dict_as_list(Arcs, Pairs),
		cut_arcs_for_sides(Pairs, SourceSide, SinkSide, InitialResidual, CutArcs).

	unique_min_cut(Network, Source, Sink, Unique) :-
		::validate_terminals(Network, Source, Sink),
		validate_lower_bounds(Network),
		::maximize(Network, Source, Sink, _, Residual),
		Network = network(Vertices, Arcs, _, _),
		::bfs(Network, Residual, Source, all, Info),
		dict_keys(Info, SourceSide0),
		sort(SourceSide0, SourceClosest),
		residual_graph(Vertices, Arcs, Residual, ResidualGraph),
		weighted_directed_graph(_Dictionary_)::transpose(ResidualGraph, ReverseGraph),
		weighted_directed_graph(_Dictionary_)::reachable(Sink, ReverseGraph, SinkSide),
		complement(Vertices, SinkSide, SinkClosest),
		(	SourceClosest == SinkClosest ->
			Unique = true
		;	Unique = false
		).

	min_cuts(Network, Source, Sink, Value, cut(SourceSide, SinkSide, CutArcs)) :-
		::validate_terminals(Network, Source, Sink),
		validate_lower_bounds(Network),
		::maximize(Network, Source, Sink, Value, Residual),
		Network = network(Vertices, Arcs, _, InitialResidual),
		dict_as_list(Arcs, ArcPairs),
		residual_graph(Vertices, Arcs, Residual, ResidualGraph),
		weighted_directed_graph(_Dictionary_)::strongly_connected_components(ResidualGraph, Components),
		component_of(Source, Components, SourceComponent),
		component_of(Sink, Components, SinkComponent),
		choose_components(Components, SourceComponent, SinkComponent, SelectedComponents),
		vertices_in_components(SelectedComponents, SourceVertices0),
		sort(SourceVertices0, SourceSide),
		residual_closed(SourceSide, ArcPairs, Residual),
		complement(Vertices, SourceSide, SinkSide),
		cut_arcs_for_sides(ArcPairs, SourceSide, SinkSide, InitialResidual, CutArcs).

	multi_source_max_flow(Network, Sources, Sinks, Value, Flow) :-
		context(Context),
		Network = network(Vertices, _, _, _),
		validate_lower_bounds(Network),
		validate_terminal_sets(Sources, Sinks, Vertices, Context),
		flow_network(_Dictionary_)::arcs(Network, Arcs),
		capacity_sum(Arcs, 0, Capacity),
		fresh_vertex('$flow_multi_source', Vertices, 0, SuperSource),
		append_list(Vertices, [SuperSource], Vertices1),
		fresh_vertex('$flow_multi_sink', Vertices1, 0, SuperSink),
		source_arcs(Sources, SuperSource, Capacity, SourceArcs),
		sink_arcs(Sinks, SuperSink, Capacity, SinkArcs),
		standard_arcs(Arcs, OriginalArcs),
		append_list(OriginalArcs, SourceArcs, Arcs1),
		append_list(Arcs1, SinkArcs, AugmentedArcs),
		flow_network(_Dictionary_)::new(Vertices, AugmentedArcs, Augmented),
		::maximize(Augmented, SuperSource, SuperSink, Value, Residual),
		::flow_list(Augmented, Residual, Flow0),
		project_flow(Flow0, Arcs, Flow).

	node_capacity_max_flow(Network, Source, Sink, NodeCapacities, Value, Flow) :-
		::validate_terminals(Network, Source, Sink),
		validate_lower_bounds(Network),
		context(Context),
		Network = network(Vertices, _, _, _),
		validate_node_capacities(NodeCapacities, Vertices, Source, Sink, [], Context),
		flow_network(_Dictionary_)::arcs(Network, Arcs),
		capacity_sum(Arcs, 0, DefaultCapacity),
		node_arcs(Arcs, OriginalArcs),
		split_arcs(Vertices, Source, Sink, NodeCapacities, DefaultCapacity, SplitArcs),
		append_list(OriginalArcs, SplitArcs, AugmentedArcs),
		flow_network(_Dictionary_)::new(AugmentedArcs, Augmented),
		node_out(Source, SourceOut),
		node_in(Sink, SinkIn),
		::maximize(Augmented, SourceOut, SinkIn, Value, Residual),
		::flow_list(Augmented, Residual, Flow0),
		project_flow(Flow0, Arcs, Flow),
		!.

	sink_side([], _, []).
	sink_side([Vertex| Vertices], Info, SinkSide) :-
		(	dict_lookup(Vertex, _, Info) ->
			SinkSide = SinkSide1
		;	SinkSide = [Vertex| SinkSide1]
		),
		sink_side(Vertices, Info, SinkSide1).

	% only forward arcs (even identifiers) are original arcs; ResidualDict
	% holds the initial residual capacities, i.e. the original capacities

	validate_lower_bounds(Network) :-
		Network = network(_, Arcs, _, _),
		dict_as_list(Arcs, Pairs),
		(	no_lower_bounds(Pairs) ->
			true
		;	context(Context),
			throw(error(domain_error(zero_lower_bounds, Network), Context))
		).

	no_lower_bounds([]).
	no_lower_bounds([Id-arc(_, _, _, Lower)| Pairs]) :-
		(	0 =:= Id mod 2 ->
			Lower =:= 0
		;	true
		),
		no_lower_bounds(Pairs).

	cut_arcs([], _, _, []).
	cut_arcs([Id-arc(From, To, _, _)| Pairs], Info, ResidualDict, CutArcs) :-
		(	0 =:= Id mod 2,
			dict_lookup(From, _, Info),
			\+ dict_lookup(To, _, Info),
			dict_lookup(Id, ResidualCapacity, ResidualDict),
			Capacity is ResidualCapacity,
			Capacity > 0 ->
			CutArcs = [arc(From, To, Capacity)| CutArcs1]
		;	CutArcs = CutArcs1
		),
		cut_arcs(Pairs, Info, ResidualDict, CutArcs1).

	residual_graph(Vertices, Arcs, Residual, Graph) :-
		dict_as_list(Arcs, ArcPairs),
		residual_graph_edges(ArcPairs, Residual, Edges),
		weighted_directed_graph(_Dictionary_)::new(Vertices, Edges, Graph).

	residual_graph_edges([], _, []).
	residual_graph_edges([Id-arc(From, To, _, _)| Pairs], Residual, Edges) :-
		dict_lookup(Id, Capacity, Residual),
		(	Capacity > 0 ->
			Edges = [((From-To)-1)| Edges1]
		;	Edges = Edges1
		),
		residual_graph_edges(Pairs, Residual, Edges1).

	complement([], _, []).
	complement([Vertex| Vertices], Other, Complement) :-
		(	member(Vertex, Other) ->
			Complement = Complement1
		;	Complement = [Vertex| Complement1]
		),
		complement(Vertices, Other, Complement1).

	cut_arcs_for_sides([], _, _, _, []).
	cut_arcs_for_sides([Id-arc(From, To, _, _)| Pairs], SourceSide, SinkSide, ResidualDict, CutArcs) :-
		(	0 =:= Id mod 2,
			memberchk(From, SourceSide),
			memberchk(To, SinkSide),
			dict_lookup(Id, Capacity, ResidualDict),
			Capacity > 0 ->
			CutArcs = [arc(From, To, Capacity)| CutArcs1]
		;	CutArcs = CutArcs1
		),
		cut_arcs_for_sides(Pairs, SourceSide, SinkSide, ResidualDict, CutArcs1).

	component_of(Vertex, [Component| _], Component) :-
		member(Vertex, Component),
		!.
	component_of(Vertex, [_| Components], Component) :-
		component_of(Vertex, Components, Component).

	choose_components([], _, _, []).
	choose_components([Component| Components], SourceComponent, SinkComponent, Selected) :-
		(	Component == SourceComponent ->
			Selected = [Component| Selected1],
			choose_components(Components, SourceComponent, SinkComponent, Selected1)
		;	Component == SinkComponent ->
			choose_components(Components, SourceComponent, SinkComponent, Selected)
		;	(	Selected = [Component| Selected1],
				choose_components(Components, SourceComponent, SinkComponent, Selected1)
			;	choose_components(Components, SourceComponent, SinkComponent, Selected)
			)
		).

	vertices_in_components([], []).
	vertices_in_components([Component| Components], Vertices) :-
		append_list(Component, RestVertices, Vertices),
		vertices_in_components(Components, RestVertices).

	residual_closed(_, [], _).
	residual_closed(SourceSide, [Id-arc(From, To, _, _)| Arcs], Residual) :-
		dict_lookup(Id, Capacity, Residual),
		(	Capacity > 0,
			member(From, SourceSide) ->
			memberchk(To, SourceSide)
		;	true
		),
		residual_closed(SourceSide, Arcs, Residual).

	validate_terminal_sets(Sources, Sinks, Vertices, Context) :-
		check(list, Sources, Context),
		check(list, Sinks, Context),
		(	Sources == [] ->
			throw(error(domain_error(non_empty_terminal_set, Sources), Context))
		;	Sinks == [] ->
			throw(error(domain_error(non_empty_terminal_set, Sinks), Context))
		;	true
		),
		validate_terminal_set(Sources, Vertices, [], Context),
		validate_terminal_set(Sinks, Vertices, [], Context),
		(	sets_disjoint(Sources, Sinks) ->
			true
		;	throw(error(consistency_error(disjoint_terminals, Sources, Sinks), Context))
		).

	validate_terminal_set([], _, _, _).
	validate_terminal_set([Vertex| Vertices0], Vertices, Seen, Context) :-
		(	ground(Vertex) ->
			true
		;	throw(error(instantiation_error, Context))
		),
		(	member(Vertex, Vertices) ->
			true
		;	throw(error(existence_error(vertex, Vertex), Context))
		),
		(	member(Vertex, Seen) ->
			throw(error(domain_error(unique_terminals, Vertex), Context))
		;	true
		),
		validate_terminal_set(Vertices0, Vertices, [Vertex| Seen], Context).

	sets_disjoint([], _).
	sets_disjoint([Vertex| Vertices], Other) :-
		\+ member(Vertex, Other),
		sets_disjoint(Vertices, Other).

	validate_node_capacities(NodeCapacities, Vertices, Source, Sink, Seen, Context) :-
		check(list, NodeCapacities, Context),
		validate_node_capacities_(NodeCapacities, Vertices, Source, Sink, Seen, Context).

	validate_node_capacities_([], _, _, _, _, _).
	validate_node_capacities_([Pair| Pairs], Vertices, Source, Sink, Seen, Context) :-
		(	var(Pair) ->
			throw(error(instantiation_error, Context))
		;	Pair = Vertex-Capacity ->
			(	ground(Vertex) ->
				true
			;	throw(error(instantiation_error, Context))
			),
			check(non_negative_integer, Capacity, Context),
			(	member(Vertex, Vertices) ->
				true
			;	throw(error(existence_error(vertex, Vertex), Context))
			),
			(	( Vertex == Source ; Vertex == Sink ) ->
				throw(error(domain_error(intermediate_vertex, Vertex), Context))
			;	true
			),
			(	member(Vertex, Seen) ->
				throw(error(domain_error(unique_node_capacities, Vertex), Context))
			;	true
			),
			validate_node_capacities_(Pairs, Vertices, Source, Sink, [Vertex| Seen], Context)
		;	throw(error(type_error(list(pair), [Pair| Pairs]), Context))
		).

	capacity_sum([], Capacity, Capacity).
	capacity_sum([arc(_, _, _, ArcCapacity, _)| Arcs], Capacity0, Capacity) :-
		Capacity1 is Capacity0 + ArcCapacity,
		capacity_sum(Arcs, Capacity1, Capacity).

	fresh_vertex(Tag, Vertices, Index, Vertex) :-
		Candidate =.. [Tag, Index],
		(	member(Candidate, Vertices) ->
			Next is Index + 1,
			fresh_vertex(Tag, Vertices, Next, Vertex)
		;	Vertex = Candidate
		).

	source_arcs([], _, _, []).
	source_arcs([Vertex| Vertices], SuperSource, Capacity, [arc(SuperSource, Vertex, Capacity)| Arcs]) :-
		source_arcs(Vertices, SuperSource, Capacity, Arcs).

	sink_arcs([], _, _, []).
	sink_arcs([Vertex| Vertices], SuperSink, Capacity, [arc(Vertex, SuperSink, Capacity)| Arcs]) :-
		sink_arcs(Vertices, SuperSink, Capacity, Arcs).

	standard_arcs([], []).
	standard_arcs([arc(From, To, 0, Capacity, Cost)| Arcs], [arc(From, To, Capacity, Cost)| StandardArcs]) :-
		standard_arcs(Arcs, StandardArcs).

	node_arcs([], []).
	node_arcs([arc(From, To, 0, Capacity, Cost)| Arcs], [arc(Out, In, Capacity, Cost)| NodeArcs]) :-
		node_out(From, Out),
		node_in(To, In),
		node_arcs(Arcs, NodeArcs).

	node_in(Vertex, '$flow_node_in'(Vertex)).

	node_out(Vertex, '$flow_node_out'(Vertex)).

	split_arcs([], _, _, _, _, []).
	split_arcs([Vertex| Vertices], Source, Sink, NodeCapacities, DefaultCapacity, Arcs) :-
		(	(	Vertex == Source
			;	Vertex == Sink
			) ->
			Arcs = Arcs1
		;	node_capacity(Vertex, NodeCapacities, DefaultCapacity, Capacity),
			node_in(Vertex, In),
			node_out(Vertex, Out),
			Arcs = [arc(In, Out, Capacity)| Arcs1]
		),
		split_arcs(Vertices, Source, Sink, NodeCapacities, DefaultCapacity, Arcs1).

	node_capacity(Vertex, [Vertex0-Capacity| _], _, Capacity) :-
		Vertex == Vertex0,
		!.
	node_capacity(Vertex, [_| NodeCapacities], DefaultCapacity, Capacity) :-
		node_capacity(Vertex, NodeCapacities, DefaultCapacity, Capacity).
	node_capacity(_, [], DefaultCapacity, DefaultCapacity).

	append_list([], List, List).
	append_list([Head| Tail], List, [Head| Rest]) :-
		append_list(Tail, List, Rest).

	project_flow([], _, []).
	project_flow([flow(ArcId, _, _, Amount)| Flow0], Arcs, Flow) :-
		(	arc_at(Arcs, ArcId, arc(From, To, _, _, _)) ->
			Flow = [flow(ArcId, From, To, Amount)| Flow1]
		;	Flow = Flow1
		),
		project_flow(Flow0, Arcs, Flow1).

	arc_at([Arc| _], 1, Arc) :-
		!.
	arc_at([_| Arcs], ArcId, Arc) :-
		ArcId > 1,
		ArcId1 is ArcId - 1,
		arc_at(Arcs, ArcId1, Arc).

:- end_category.
