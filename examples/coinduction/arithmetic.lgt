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


% For more information on this example, see the paper:
%
% "Regular corecursion in Prolog", Davide Ancona
% http://www.disi.unige.it/person/AnconaD/papers/Reports_abstracts.html#AnconaExtendedSAC12

:- object(arithmetic).

	:- info([
		version is 1:0:0,
		author is 'Davide Ancona. Adapted to Logtalk by Paulo Moura.',
		date is 2012-09-13,
		comment is 'Examples of coinductive predicates over lists of numbers.'
	]).

	:- public(add/4).
	:- coinductive(add/4).

	add([D1| R1], [D2| R2], [Sd| S], O) :-
		add(R1, R2, S, C),
		Sum is D1 + D2 + C,
		Sd is Sum mod 10,
		O is Sum // 10.

	:- public(max/2).
	:- coinductive(max(+, -)).

	max([N], N).
	max([N| L], M) :-
		max(L, M1),
		(	N > M1 ->
			M = N
		;	M = M1
		).

	coinductive_success_hook(add(_, _, _, 0)).
	coinductive_success_hook(add(_, _, _, 1)).
	coinductive_success_hook(max([N| _], N)).

:- end_object.
