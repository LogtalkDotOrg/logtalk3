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


:- object(dinic(_Dictionary_),
	imports(max_flow_common(_Dictionary_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Dinic maximum flow algorithm. Each phase builds a level graph using breadth-first search and then computes a blocking flow using depth-first search. Runs in O(V^2*E) time. A vertex whose depth-first search fails to push the requested amount is recorded as dead for the rest of the phase, which plays the role of the current-arc pointers of imperative implementations.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network (e.g., ``avltree``).'
		],
		see_also is [edmonds_karp(_), dinic, flow_network(_)]
	]).

	:- uses(_Dictionary_, [
		new/1 as dict_new/1, insert/4 as dict_insert/4, lookup/3 as dict_lookup/3
	]).

	maximize(Network, Source, Sink, Value, Residual) :-
		Network = network(_, _, _, Residual0),
		phases(Network, Source, Sink, 0, Value, Residual0, Residual).

	phases(Network, Source, Sink, Value0, Value, Residual0, Residual) :-
		^^bfs(Network, Residual0, Source, stop(Sink), Info),
		(	dict_lookup(Sink, _, Info) ->
			^^adjacency(Source, Network, SourceIds),
			total_residual(SourceIds, Residual0, 0, Limit),
			dict_new(Dead0),
			dfs(Source, Sink, Limit, Network, Info, Residual0, Residual1, Dead0, _, Pushed),
			Value1 is Value0 + Pushed,
			phases(Network, Source, Sink, Value1, Value, Residual1, Residual)
		;	Value = Value0,
			Residual = Residual0
		).

	total_residual([], _, Total, Total).
	total_residual([Id| Ids], Residual, Total0, Total) :-
		dict_lookup(Id, Capacity, Residual),
		Total1 is Total0 + Capacity,
		total_residual(Ids, Residual, Total1, Total).

	% dfs(+Vertex, +Sink, +Limit, +Network, +Levels, +Residual0, -Residual, +Dead0, -Dead, -Pushed)

	dfs(Vertex, Sink, Limit, Network, Levels, Residual0, Residual, Dead0, Dead, Pushed) :-
		(	Vertex == Sink ->
			Pushed = Limit,
			Residual = Residual0,
			Dead = Dead0
		;	dict_lookup(Vertex, Level-_, Levels),
			^^adjacency(Vertex, Network, Ids),
			push(Ids, Vertex, Level, Sink, Limit, 0, Network, Levels, Residual0, Residual, Dead0, Dead1, Pushed),
			(	Pushed < Limit ->
				dict_insert(Dead1, Vertex, true, Dead)
			;	Dead = Dead1
			)
		).

	push([], _, _, _, _, Pushed, _, _, Residual, Residual, Dead, Dead, Pushed).
	push([Id| Ids], Vertex, Level, Sink, Limit, Acc, Network, Levels, Residual0, Residual, Dead0, Dead, Pushed) :-
		(	Acc =:= Limit ->
			Pushed = Acc,
			Residual = Residual0,
			Dead = Dead0
		;	Network = network(_, Arcs, _, _),
			dict_lookup(Id, Capacity, Residual0),
			dict_lookup(Id, arc(_, To, _, _), Arcs),
			Next is Level + 1,
			(	Capacity > 0,
				dict_lookup(To, Next-_, Levels),
				\+ dict_lookup(To, _, Dead0) ->
				Want is min(Limit - Acc, Capacity),
				dfs(To, Sink, Want, Network, Levels, Residual0, Residual1, Dead0, Dead1, Got),
				(	Got > 0 ->
					^^augment_arc(Id, Got, Residual1, Residual2)
				;	Residual2 = Residual1
				),
				Acc1 is Acc + Got,
				push(Ids, Vertex, Level, Sink, Limit, Acc1, Network, Levels, Residual2, Residual, Dead1, Dead, Pushed)
			;	push(Ids, Vertex, Level, Sink, Limit, Acc, Network, Levels, Residual0, Residual, Dead0, Dead, Pushed)
			)
		).

:- end_object.


:- object(dinic,
	extends(dinic(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Dinic maximum flow algorithm using the ``avltree`` dictionary.',
		see_also is [dinic(_), edmonds_karp]
	]).

:- end_object.
