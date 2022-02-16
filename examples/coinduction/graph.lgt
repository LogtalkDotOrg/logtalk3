%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


% Coinductive-based solution for testing for bipartite graphs
%
% For background information, check e.g.
% http://en.wikipedia.org/wiki/Bipartite_graph
%
% For more information on this example, see the paper:
%
% "Regular corecursion in Prolog", Davide Ancona
% http://www.disi.unige.it/person/AnconaD/papers/Reports_abstracts.html#AnconaExtendedSAC12

:- object(graph).

	:- info([
		version is 1:0:0,
		author is 'Davide Ancona. Adapted to Logtalk by Paulo Moura.',
		date is 2012-09-17,
		comment is 'Examples of coinductive predicates over graphs.'
	]).

	:- public(bipartite/1).

	:- coinductive(no_odd_cycle(+, -)).

	bipartite(Vertex) :-
		no_odd_cycle(Vertex, 0).

	no_odd_cycle(v(_, Vertexes), Parity1) :-
		Parity2 is (Parity1 + 1) mod 2,
		no_odd_cycle(Vertexes, Parity2).
	no_odd_cycle([], _).
	no_odd_cycle([Vertex| Vertexes], Parity) :-
		no_odd_cycle(Vertex, Parity),
		no_odd_cycle(Vertexes, Parity).

	coinductive_success_hook(no_odd_cycle(_,Parity1), no_odd_cycle(_,Parity2)) :-
		Parity1 == Parity2.

:- end_object.
