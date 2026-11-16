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


:- object(edmonds_karp(_Dictionary_),
	imports(max_flow_common(_Dictionary_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Edmonds-Karp maximum flow algorithm (Ford-Fulkerson using shortest augmenting paths found by breadth-first search). Runs in O(V*E^2) time. Mainly intended as a simple reference implementation; see the ``dinic`` object for a faster alternative.',
		parameters is [
			'Dictionary' - 'Dictionary object used by the flow network (e.g., ``avltree``).'
		],
		see_also is [dinic(_), edmonds_karp, flow_network(_)]
	]).

	maximize(Network, Source, Sink, Value, Residual) :-
		Network = network(_, _, _, Residual0),
		augment_loop(Network, Source, Sink, 0, Value, Residual0, Residual).

	augment_loop(Network, Source, Sink, Value0, Value, Residual0, Residual) :-
		^^bfs(Network, Residual0, Source, stop(Sink), Info),
		(	dict_reached(Sink, Info) ->
			^^path_ids(Network, Sink, Info, Ids),
			^^bottleneck(Ids, Residual0, Amount),
			^^augment(Ids, Amount, Residual0, Residual1),
			Value1 is Value0 + Amount,
			augment_loop(Network, Source, Sink, Value1, Value, Residual1, Residual)
		;	Value = Value0,
			Residual = Residual0
		).

	dict_reached(Vertex, Info) :-
		_Dictionary_::lookup(Vertex, _, Info).

:- end_object.


:- object(edmonds_karp,
	extends(edmonds_karp(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Edmonds-Karp maximum flow algorithm using the ``avltree`` dictionary.',
		see_also is [edmonds_karp(_), dinic]
	]).

:- end_object.
