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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2022-07-21,
		comment is 'Unit tests for the "gensym" library.'
	]).

	cover(gensym_core).
	cover(gensym).

	setup :-
		create_object(gs1, [imports(gensym_core)], [], []),
		create_object(gs2, [imports(gensym_core)], [], []).

	cleanup :-
		abolish_object(gs1),
		abolish_object(gs2).

	:- uses(gensym, [
		gensym/2, reset_gensym/1, reset_gensym/0
	]).

	% gensym/2

	test(gensym_gensym_2_01, deterministic(A1 \== A2)) :-
		gensym(a, A1),
		gensym(a, A2),
		ground(A1), ground(A2).

	test(gensym_gensym_2_02, deterministic(Prefix == a)) :-
		gensym(a, A),
		sub_atom(A, 0, 1, _, Prefix).

	% reset_gensym/0

	test(gensym_reset_gensym_0_01) :-
		reset_gensym.

	test(gensym_reset_gensym_0_02, deterministic(A1 == A2)) :-
		gensym(a, A1),
		reset_gensym,
		gensym(a, A2).

	% reset_gensym/1

	test(gensym_reset_gensym_1_01) :-
		reset_gensym(a).

	test(gensym_reset_gensym_1_02, deterministic(A1 == A2)) :-
		gensym(a, A1),
		reset_gensym(a),
		gensym(a, A2).

	test(gensym_reset_gensym_1_03, deterministic(A1 \== A2)) :-
		gensym(a, A1),
		reset_gensym(b),
		gensym(a, A2).

	% multiple gensym objects

	test(gensym_multiple_same_counter, deterministic(A1 == A2)) :-
		gs1::gensym(a, A1),
		gs2::gensym(a, A2).

	test(gensym_multiple_reset_one, deterministic((B1 == B2, B2 == B3, B3 \== B4))) :-
		gs1::gensym(b, B1),
		gs2::gensym(b, B2),
		gs1::reset_gensym,
		gs1::gensym(b, B3),
		gs2::gensym(b, B4).

:- end_object.
