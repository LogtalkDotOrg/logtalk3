%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/05/21,
		comment is 'Unit tests for the "gensym" library.'
	]).

	cover(gensym).

	% gensym/2

	test(gensym_gensym_2_01, deterministic(A1 \== A2)) :-
		gensym::gensym(a, A1),
		gensym::gensym(a, A2),
		ground(A1), ground(A2).

	test(gensym_gensym_2_02, deterministic(Prefix == a)) :-
		gensym::gensym(a, A),
		sub_atom(A, 0, 1, _, Prefix).

	% reset_gensym/0

	test(gensym_reset_gensym_0_01) :-
		gensym::reset_gensym.

	test(gensym_reset_gensym_0_02, deterministic(A1 == A2)) :-
		gensym::gensym(a, A1),
		gensym::reset_gensym,
		gensym::gensym(a, A2).

	% reset_gensym/1

	test(gensym_reset_gensym_1_01) :-
		gensym::reset_gensym(a).

	test(gensym_reset_gensym_1_02, deterministic(A1 == A2)) :-
		gensym::gensym(a, A1),
		gensym::reset_gensym(a),
		gensym::gensym(a, A2).

	test(gensym_reset_gensym_1_03, deterministic(A1 \== A2)) :-
		gensym::gensym(a, A1),
		gensym::reset_gensym(b),
		gensym::gensym(a, A2).

:- end_object.
