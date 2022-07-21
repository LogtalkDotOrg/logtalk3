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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2022-07-21,
		comment is 'Unit tests for the "genint" library.'
	]).

	cover(genint_core).
	cover(genint).

	setup :-
		create_object(gi1, [imports(genint_core)], [], []),
		create_object(gi2, [imports(genint_core)], [], []).

	cleanup :-
		abolish_object(gi1),
		abolish_object(gi2).

	:- uses(genint, [
		genint/2, reset_genint/1, reset_genint/0
	]).

	% genint/2

	test(genint_genint_2_01, deterministic(a(A0,A1,A2) == a(0,1,2))) :-
		genint(a, A0),
		genint(a, A1),
		genint(a, A2).

	test(genint_genint_2_02, deterministic(B0 == 0)) :-
		genint(b, B0).

	% reset_genint/0

	test(genint_reset_genint_0_01, deterministic) :-
		reset_genint.

	test(genint_reset_genint_0_02, deterministic(a(A1,A2) == a(0,0))) :-
		genint(a, A1),
		reset_genint,
		genint(a, A2).

	% reset_genint/1

	test(genint_reset_genint_1_01) :-
		reset_genint(a).

	test(genint_reset_genint_1_02, deterministic(a(A1,A2) == a(0,0))) :-
		genint(a, A1),
		reset_genint(a),
		genint(a, A2).

	test(genint_reset_genint_1_03, deterministic(d(D0,D1) == d(0,1))) :-
		genint(d, D0),
		reset_genint(a),
		genint(d, D1).

	% multiple genint objects

	test(genint_multiple_same_counter, deterministic(a(A1,A2) == a(0,0))) :-
		gi1::genint(a, A1),
		gi2::genint(a, A2).

	test(genint_multiple_reset_one, deterministic(a(B1,B2,B3,B4) == a(0,0,0,1))) :-
		gi1::genint(b, B1),
		gi2::genint(b, B2),
		gi1::reset_genint,
		gi1::genint(b, B3),
		gi2::genint(b, B4).

:- end_object.
