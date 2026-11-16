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


:- category(flow_support(_Dictionary_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Support predicates shared by the flow algorithm objects: terminal validation, residual graph traversal, path augmentation, and flow extraction. The category parameter is the dictionary object and must be the same one used to construct the flow network.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network.'
		]
	]).

	:- uses(_Dictionary_, [
		new/1 as dict_new/1,
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		as_list/2 as dict_as_list/2
	]).

	:- uses(queue, [
		new/1 as queue_new/1,
		empty/1 as queue_empty/1,
		join/3 as queue_join/3,
		serve/3 as queue_serve/3
	]).

	:- uses(list, [
		member/2
	]).

	:- protected(validate_terminals/3).
	:- mode(validate_terminals(+compound, +ground, +ground), one_or_error).
	:- info(validate_terminals/3, [
		comment is 'Checks that the source and sink are distinct vertices of the network.',
		argnames is ['Network', 'Source', 'Sink'],
		exceptions is [
			'``Source`` or ``Sink`` is not ground' - instantiation_error,
			'``Source`` or ``Sink`` is not a vertex of the network' - existence_error(vertex, 'Vertex'),
			'``Source`` and ``Sink`` are the same vertex' - consistency_error(distinct_vertices, 'Source', 'Sink')
		]
	]).

	:- protected(partner/2).
	:- mode(partner(+integer, -integer), one).
	:- info(partner/2, [
		comment is 'Arc identifier of the opposite arc in a residual pair.',
		argnames is ['Id', 'Partner']
	]).

	:- protected(adjacency/3).
	:- mode(adjacency(+ground, +compound, -list(integer)), one).
	:- info(adjacency/3, [
		comment is 'Outgoing residual arc identifiers of a vertex (empty list if none).',
		argnames is ['Vertex', 'Network', 'Ids']
	]).

	:- protected(bfs/5).
	:- mode(bfs(+compound, +compound, +ground, +nonvar, -compound), one).
	:- info(bfs/5, [
		comment is 'Breadth-first search over arcs with positive residual capacity. ``Stop`` is either ``stop(Vertex)`` (stop as soon as the vertex is reached) or ``all``. ``Info`` is a dictionary mapping each reached vertex to ``Level-ArcId`` where ``ArcId`` is the arc used to reach it (``none`` for the source).',
		argnames is ['Network', 'Residual', 'Source', 'Stop', 'Info']
	]).

	:- protected(bfs/6).
	:- mode(bfs(+compound, +compound, +ground, +nonvar, +positive_integer, -compound), one).
	:- info(bfs/6, [
		comment is 'As ``bfs/5``, traversing only arcs whose residual capacity is at least ``Threshold``.',
		argnames is ['Network', 'Residual', 'Source', 'Stop', 'Threshold', 'Info']
	]).

	:- protected(augment/4).
	:- mode(augment(+list(integer), +integer, +compound, -compound), one).
	:- info(augment/4, [
		comment is 'Pushes the given amount of flow along a list of arc identifiers, updating the residual dictionary.',
		argnames is ['Ids', 'Amount', 'Residual0', 'Residual']
	]).

	:- protected(augment_arc/4).
	:- mode(augment_arc(+integer, +integer, +compound, -compound), one).
	:- info(augment_arc/4, [
		comment is 'Pushes the given amount of flow along a single arc, updating the residual dictionary.',
		argnames is ['Id', 'Amount', 'Residual0', 'Residual']
	]).

	:- protected(path_ids/4).
	:- mode(path_ids(+compound, +ground, +compound, -list(integer)), one).
	:- info(path_ids/4, [
		comment is 'Arc identifiers of the path from the BFS source to the given vertex (in source to vertex order).',
		argnames is ['Network', 'Vertex', 'Info', 'Ids']
	]).

	:- protected(bottleneck/3).
	:- mode(bottleneck(+list(integer), +compound, -integer), one).
	:- info(bottleneck/3, [
		comment is 'Minimum residual capacity along a non-empty list of arc identifiers.',
		argnames is ['Ids', 'Residual', 'Amount']
	]).

	:- protected(flow_list/3).
	:- mode(flow_list(+compound, +compound, -list(compound)), one).
	:- info(flow_list/3, [
		comment is 'Flow on each arc with positive flow as ``flow(ArcId, From, To, Amount)`` terms in arc input order. ``ArcId`` is the one-based input arc position.',
		argnames is ['Network', 'Residual', 'Flow']
	]).

	:- protected(flow_cost/3).
	:- mode(flow_cost(+compound, +compound, -integer), one).
	:- info(flow_cost/3, [
		comment is 'Total cost of the flow described by the residual dictionary.',
		argnames is ['Network', 'Residual', 'Cost']
	]).

	validate_terminals(network(Vertices, _, _, _), Source, Sink) :-
		context(Context),
		(	ground(Source), ground(Sink) ->
			true
		;	instantiation_error
		),
		(	member(Source, Vertices) ->
			true
		;	existence_error(vertex, Source)
		),
		(	member(Sink, Vertices) ->
			true
		;	existence_error(vertex, Sink)
		),
		(	Source == Sink ->
			throw(error(consistency_error(distinct_vertices, Source, Sink), Context))
		;	true
		).

	partner(Id, Partner) :-
		(	0 =:= Id mod 2 ->
			Partner is Id + 1
		;	Partner is Id - 1
		).

	adjacency(Vertex, network(_, _, Adjacency, _), Ids) :-
		(	dict_lookup(Vertex, Ids0, Adjacency) ->
			Ids = Ids0
		;	Ids = []
		).

	bfs(Network, Residual, Source, Stop, Info) :-
		bfs(Network, Residual, Source, Stop, 1, Info).

	bfs(network(_, Arcs, Adjacency, _), Residual, Source, Stop, Threshold, Info) :-
		dict_new(Info0),
		dict_insert(Info0, Source, 0-none, Info1),
		queue_new(Queue0),
		queue_join(Source, Queue0, Queue1),
		bfs_loop(Queue1, Arcs, Adjacency, Residual, Stop, Threshold, Info1, Info).

	bfs_loop(Queue0, Arcs, Adjacency, Residual, Stop, Threshold, Info0, Info) :-
		(	queue_empty(Queue0) ->
			Info = Info0
		;	queue_serve(Queue0, Vertex, Queue1),
			dict_lookup(Vertex, Level-_, Info0),
			Level1 is Level + 1,
			(	dict_lookup(Vertex, Ids, Adjacency) ->
				true
			;	Ids = []
			),
			bfs_expand(Ids, Level1, Arcs, Residual, Stop, Threshold, Queue1, Queue2, Info0, Info1, Found),
			(	Found == true ->
				Info = Info1
			;	bfs_loop(Queue2, Arcs, Adjacency, Residual, Stop, Threshold, Info1, Info)
			)
		).

	bfs_expand([], _, _, _, _, _, Queue, Queue, Info, Info, false).
	bfs_expand([Id| Ids], Level, Arcs, Residual, Stop, Threshold, Queue0, Queue, Info0, Info, Found) :-
		dict_lookup(Id, Capacity, Residual),
		dict_lookup(Id, arc(_, To, _, _), Arcs),
		(	Capacity >= Threshold,
			\+ dict_lookup(To, _, Info0) ->
			dict_insert(Info0, To, Level-Id, Info1),
			queue_join(To, Queue0, Queue1),
			(	Stop == stop(To) ->
				Queue = Queue1,
				Info = Info1,
				Found = true
			;	bfs_expand(Ids, Level, Arcs, Residual, Stop, Threshold, Queue1, Queue, Info1, Info, Found)
			)
		;	bfs_expand(Ids, Level, Arcs, Residual, Stop, Threshold, Queue0, Queue, Info0, Info, Found)
		).

	augment([], _, Residual, Residual).
	augment([Id| Ids], Amount, Residual0, Residual) :-
		augment_arc(Id, Amount, Residual0, Residual1),
		augment(Ids, Amount, Residual1, Residual).

	augment_arc(Id, Amount, Residual0, Residual) :-
		partner(Id, Partner),
		dict_lookup(Id, Capacity0, Residual0),
		Capacity is Capacity0 - Amount,
		dict_insert(Residual0, Id, Capacity, Residual1),
		dict_lookup(Partner, PartnerCapacity0, Residual1),
		PartnerCapacity is PartnerCapacity0 + Amount,
		dict_insert(Residual1, Partner, PartnerCapacity, Residual).

	path_ids(Network, Vertex, Info, Ids) :-
		Network = network(_, Arcs, _, _),
		path_ids(Vertex, Arcs, Info, [], Ids).

	path_ids(Vertex, Arcs, Info, Ids0, Ids) :-
		dict_lookup(Vertex, _-Id, Info),
		(	Id == none ->
			Ids = Ids0
		;	dict_lookup(Id, arc(From, _, _, _), Arcs),
			path_ids(From, Arcs, Info, [Id| Ids0], Ids)
		).

	bottleneck([Id| Ids], Residual, Amount) :-
		dict_lookup(Id, Capacity, Residual),
		bottleneck(Ids, Residual, Capacity, Amount).

	bottleneck([], _, Amount, Amount).
	bottleneck([Id| Ids], Residual, Amount0, Amount) :-
		dict_lookup(Id, Capacity, Residual),
		Amount1 is min(Amount0, Capacity),
		bottleneck(Ids, Residual, Amount1, Amount).

	% the flow on a forward arc is the residual capacity of its reverse arc

	flow_list(network(_, Arcs, _, _), Residual, Flow) :-
		dict_as_list(Arcs, Pairs),
		flow_list_(Pairs, Residual, Flow).

	flow_list_([], _, []).
	flow_list_([Id-arc(From, To, _, Lower)| Pairs], Residual, Flow) :-
		(	0 =:= Id mod 2,
			Reverse is Id + 1,
			dict_lookup(Reverse, ExtraAmount, Residual),
			Amount is Lower + ExtraAmount,
			Amount > 0 ->
			ArcId is Id // 2 + 1,
			Flow = [flow(ArcId, From, To, Amount)| Flow1]
		;	Flow = Flow1
		),
		flow_list_(Pairs, Residual, Flow1).

	flow_cost(network(_, Arcs, _, _), Residual, Cost) :-
		dict_as_list(Arcs, Pairs),
		flow_cost_(Pairs, Residual, 0, Cost).

	flow_cost_([], _, Cost, Cost).
	flow_cost_([Id-arc(_, _, ArcCost, Lower)| Pairs], Residual, Cost0, Cost) :-
		(	0 =:= Id mod 2 ->
			Reverse is Id + 1,
			dict_lookup(Reverse, ExtraAmount, Residual),
			Amount is Lower + ExtraAmount,
			Cost1 is Cost0 + Amount * ArcCost
		;	Cost1 = Cost0
		),
		flow_cost_(Pairs, Residual, Cost1, Cost).

:- end_category.
