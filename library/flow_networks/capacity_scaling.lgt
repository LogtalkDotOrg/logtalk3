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


:- object(capacity_scaling(_Dictionary_),
	imports(max_flow_common(_Dictionary_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Capacity-scaling augmenting paths. Each phase traverses residual arcs with capacity at least a power-of-two threshold. The standard bound is O(E^2*log(U)), before dictionary costs. This is an alternative to Dinic, not a guaranteed speedup.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network.'
		],
		see_also is [capacity_scaling, dinic(_), edmonds_karp(_)]
	]).

	:- uses(_Dictionary_, [
		as_list/2 as dict_as_list/2, lookup/3 as dict_lookup/3
	]).

	maximize(Network, Source, Sink, Value, Residual) :-
		Network = network(_, _, _, Residual0),
		dict_as_list(Residual0, Pairs),
		max_capacity(Pairs, 0, Maximum),
		(	Maximum =:= 0 ->
			Value = 0,
			Residual = Residual0
		;	initial_threshold(1, Maximum, Threshold),
			scale(Network, Source, Sink, Threshold, 0, Value, Residual0, Residual)
		).

	max_capacity([], Maximum, Maximum).
	max_capacity([_-Capacity| Pairs], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, Capacity),
		max_capacity(Pairs, Maximum1, Maximum).

	initial_threshold(Threshold0, Maximum, Threshold) :-
		(	Threshold0 =< Maximum // 2 ->
			Threshold1 is Threshold0 * 2,
			initial_threshold(Threshold1, Maximum, Threshold)
		;	Threshold = Threshold0
		).

	scale(Network, Source, Sink, Threshold, Value0, Value, Residual0, Residual) :-
		^^bfs(Network, Residual0, Source, stop(Sink), Threshold, Info),
		(	dict_lookup(Sink, _, Info) ->
			^^path_ids(Network, Sink, Info, Ids),
			^^bottleneck(Ids, Residual0, Amount),
			^^augment(Ids, Amount, Residual0, Residual1),
			Value1 is Value0 + Amount,
			scale(Network, Source, Sink, Threshold, Value1, Value, Residual1, Residual)
		;	Threshold > 1 ->
			Next is Threshold // 2,
			scale(Network, Source, Sink, Next, Value0, Value, Residual0, Residual)
		;	Value = Value0,
			Residual = Residual0
		).

:- end_object.


:- object(capacity_scaling,
	extends(capacity_scaling(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Capacity-scaling maximum flow using the ``avltree`` dictionary.',
		see_also is [capacity_scaling(_), dinic]
	]).

:- end_object.
