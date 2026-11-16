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


:- object(time_expanded_network(_Dictionary_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Builds finite, discrete-time flow networks from explicit departure and storage schedules. Expanded vertices are tagged ``time(Vertex, Time)`` terms; schedule arcs retain input order and arc IDs.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the constructed flow network.'
		],
		see_also is [time_expanded_network, flow_network(_), successive_shortest_paths(_), dinic(_)]
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(list, [
		append/3, member/2
	]).

	:- uses(flow_network(_Dictionary_), [
		new/3 as network_new/3
	]).

	:- public(build/6).
	:- mode(build(+list(ground), +integer, +list(compound), +list(compound), --compound, --compound), one_or_error).
	:- info(build/6, [
		comment is 'Builds a time-expanded flow network. ``Vertices`` is a proper list of ground base vertices and ``Horizon`` is a non-negative integer. Each ``travel(From, To, Departure, Duration, Lower, Capacity, Cost)`` creates one arc from ``time(From,Departure)`` to ``time(To,Departure+Duration)``. Each ``store(Vertex, Time, Lower, Capacity, Cost)`` creates one arc from ``time(Vertex,Time)`` to ``time(Vertex,Time+1)``. Durations are positive; schedule endpoints and arrivals must be within the horizon. Missing storage entries mean no waiting capacity. The mapping is ``mapping(TravelEntries, StorageEntries)`` where entries are ``travel(ArcId, EntryId, From, To, Departure, Arrival)`` and ``store(ArcId, EntryId, Vertex, Time, Arrival)``; entry IDs are one-based in their respective input lists and arc IDs follow travel input order then storage input order. Expanded vertices are tagged ``time(Vertex,Time)`` terms, preserving all time copies including isolated vertices. Supplies and demands at exact times can be passed to the existing minimum-cost flow solver. Evacuation by a deadline can be modeled by adding collection arcs from eligible time copies to a fresh terminal. Storage capacities constrain individual intervals, not a shared per-vertex capacity. Non-zero lower bounds require the supply-based minimum-cost flow solver; maximum-flow APIs require zero lower bounds.',
		argnames is ['Vertices', 'Horizon', 'Travels', 'Storage', 'Network', 'Mapping'],
		exceptions is [
			'``Vertices``, ``Travels``, or ``Storage`` is a variable or partial list, or a base vertex or schedule endpoint is non-ground' - instantiation_error,
			'An input list is neither a variable nor a list' - type_error(list, 'Input'),
			'``Horizon``, a departure, a duration, a storage time, a lower bound, a capacity, or a cost is not an integer' - type_error(integer, 'Value'),
			'``Horizon`` is negative' - domain_error(non_negative_integer, 'Horizon'),
			'A travel or storage entry has an invalid shape' - domain_error(time_expanded_schedule, 'Entry'),
			'A scheduled endpoint is not in ``Vertices``' - existence_error(vertex, 'Vertex'),
			'A departure or storage time is outside the horizon, or a travel duration exceeds the remaining horizon' - consistency_error(time_expanded_schedule_and_horizon, 'Entry', 'Horizon'),
			'A travel duration is not positive' - domain_error(positive_integer, 'Duration'),
			'A lower bound or capacity is negative' - domain_error(non_negative_integer, 'Value'),
			'A lower bound exceeds its capacity' - consistency_error(arc_lower_bound_and_capacity, 'Lower', 'Capacity')
		]
	]).

	build(Vertices, Horizon, Travels, Storage, Network, Mapping) :-
		context(Context),
		check(list(ground), Vertices, Context),
		check(non_negative_integer, Horizon, Context),
		check(list, Travels, Context),
		check(list, Storage, Context),
		sort(Vertices, UniqueVertices),
		time_vertices(UniqueVertices, Horizon, TimeVertices),
		travel_arcs(Travels, UniqueVertices, Horizon, 1, TravelArcs, TravelMapping, TravelCount, Context),
		StorageArcId is TravelCount + 1,
		storage_arcs(Storage, UniqueVertices, Horizon, StorageArcId, 1, StorageArcs, StorageMapping, Context),
		append(TravelArcs, StorageArcs, Arcs),
		network_new(TimeVertices, Arcs, Network),
		Mapping = mapping(TravelMapping, StorageMapping).

	time_vertices([], _, []).
	time_vertices([Vertex| Vertices], Horizon, TimeVertices) :-
		time_copies(Vertex, 0, Horizon, Copies),
		time_vertices(Vertices, Horizon, Remaining),
		append(Copies, Remaining, TimeVertices).

	time_copies(_, Time, Horizon, []) :-
		Time > Horizon,
		!.
	time_copies(Vertex, Time, Horizon, [time(Vertex, Time)| Copies]) :-
		Next is Time + 1,
		time_copies(Vertex, Next, Horizon, Copies).

	travel_arcs([], _, _, ArcId, [], [], Count, _) :-
		Count is ArcId - 1.
	travel_arcs([Entry| Entries], Vertices, Horizon, ArcId, [arc(time(From, Departure), time(To, Arrival), Lower, Capacity, Cost)| Arcs], [travel(ArcId, ArcId, From, To, Departure, Arrival)| Mapping], Count, Context) :-
		validate_travel(Entry, Vertices, Horizon, From, To, Departure, Arrival, Lower, Capacity, Cost, Context),
		NextArcId is ArcId + 1,
		travel_arcs(Entries, Vertices, Horizon, NextArcId, Arcs, Mapping, Count, Context).

	validate_travel(Entry, _, _, _, _, _, _, _, _, _, Context) :-
		var(Entry),
		throw(error(instantiation_error, Context)).
	validate_travel(travel(From, To, Departure, Duration, Lower, Capacity, Cost), Vertices, Horizon, From, To, Departure, Arrival, Lower, Capacity, Cost, Context) :-
		!,
		Entry = travel(From, To, Departure, Duration, Lower, Capacity, Cost),
		validate_endpoint(From, Vertices, Context),
		validate_endpoint(To, Vertices, Context),
		check(integer, Departure, Context),
		check(positive_integer, Duration, Context),
		check_bounds_and_cost(Lower, Capacity, Cost, Context),
		(	Departure >= 0, Departure =< Horizon ->
			true
		;	throw(error(consistency_error(time_expanded_schedule_and_horizon, Entry, Horizon), Context))
		),
		(	Duration =< Horizon - Departure ->
			true
		;	throw(error(consistency_error(time_expanded_schedule_and_horizon, Entry, Horizon), Context))
		),
		Arrival is Departure + Duration.
	validate_travel(Entry, _, _, _, _, _, _, _, _, _, Context) :-
		throw(error(domain_error(time_expanded_schedule, Entry), Context)).

	storage_arcs([], _, _, _, _, [], [], _).
	storage_arcs([Entry| Entries], Vertices, Horizon, ArcId, EntryId, [arc(time(Vertex, Time), time(Vertex, Arrival), Lower, Capacity, Cost)| Arcs], [store(ArcId, EntryId, Vertex, Time, Arrival)| Mapping], Context) :-
		validate_storage(Entry, Vertices, Horizon, Vertex, Time, Arrival, Lower, Capacity, Cost, Context),
		NextArcId is ArcId + 1,
		NextEntryId is EntryId + 1,
		storage_arcs(Entries, Vertices, Horizon, NextArcId, NextEntryId, Arcs, Mapping, Context).

	validate_storage(Entry, _, _, _, _, _, _, _, _, Context) :-
		var(Entry),
		throw(error(instantiation_error, Context)).
	validate_storage(store(Vertex, Time, Lower, Capacity, Cost), Vertices, Horizon, Vertex, Time, Arrival, Lower, Capacity, Cost, Context) :-
		!,
		Entry = store(Vertex, Time, Lower, Capacity, Cost),
		validate_endpoint(Vertex, Vertices, Context),
		check(integer, Time, Context),
		check_bounds_and_cost(Lower, Capacity, Cost, Context),
		(	Time >= 0, Time < Horizon ->
			Arrival is Time + 1
		;	throw(error(consistency_error(time_expanded_schedule_and_horizon, Entry, Horizon), Context))
		).
	validate_storage(Entry, _, _, _, _, _, _, _, _, Context) :-
		throw(error(domain_error(time_expanded_schedule, Entry), Context)).

	validate_endpoint(Vertex, Vertices, Context) :-
		(	ground(Vertex) ->
			(	member(Vertex, Vertices) ->
				true
			;	throw(error(existence_error(vertex, Vertex), Context))
			)
		;	throw(error(instantiation_error, Context))
		).

	check_bounds_and_cost(Lower, Capacity, Cost, Context) :-
		check(non_negative_integer, Lower, Context),
		check(non_negative_integer, Capacity, Context),
		check(integer, Cost, Context),
		(	Lower =< Capacity ->
			true
		;	throw(error(consistency_error(arc_lower_bound_and_capacity, Lower, Capacity), Context))
		).

:- end_object.


:- object(time_expanded_network,
	extends(time_expanded_network(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Builds time-expanded flow networks using the ``avltree`` dictionary.',
		see_also is [time_expanded_network(_)]
	]).

:- end_object.
