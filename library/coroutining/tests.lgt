%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:3:1,
		author is 'Paulo Moura',
		date is 2021-11-24,
		comment is 'Unit tests for the "coroutining" library.'
	]).

	:- uses(coroutining, [
		dif/2, dif/1, freeze/2, frozen/2, when/2
	]).

	cover(coroutining).

	% dif/2 tests

	test(coroutining_dif_2_01, false) :-
		dif(X, X).

	test(coroutining_dif_2_02, true) :-
		dif(X, Y), X = 1, Y = 2.

	test(coroutining_dif_2_03, false) :-
		dif(X, Y), X = 1, Y = 1.

	% dif/1 tests

	test(coroutining_dif_1_01, false) :-
		dif([X, X, X]).

	test(coroutining_dif_1_02, true) :-
		dif([X, Y, Z]), X = 1, Y = 2, Z = 3.

	test(coroutining_dif_1_03, false) :-
		dif([X, Y, Z]), X = 1, Y = 2, Z = 1.

	% freeze/2 tests

	test(coroutining_freeze_2_01, true(var(Y))) :-
		freeze(_, Y = 2).

	test(coroutining_freeze_2_02, true(Y == 2)) :-
		freeze(X, Y = 2), X = 1.

	test(coroutining_freeze_2_03, true(Y == 2)) :-
		freeze(X, unify_y(Y)), X = 1.

	test(coroutining_freeze_2_04, true(Assertion)) :-
		^^set_text_output(''),
		freeze(X, write(foo)),
		freeze(X, write(bar)),
		X = 1,
		^^text_output_assertion('foobar', Assertion).

	% frozen/2 tests

	test(coroutining_frozen_2_01, true(Goal == true)) :-
		frozen(1, Goal).

	test(coroutining_frozen_2_02, true(Goal == true)) :-
		frozen(_, Goal).

	test(coroutining_frozen_2_03, true(term::subterm(write(done),Goal))) :-
		freeze(X, write(done)),
		frozen(X, Goal).

	test(coroutining_frozen_2_04, true) :-
		^^set_text_output(''),
		freeze(X, write(foo)),
		freeze(X, write(bar)),
		^^assertion(term::subterm(write(foo),Goal)),
		^^assertion(term::subterm(write(bar),Goal)).

	% when/2 tests

	test(coroutining_when_2_01, true(Y == 2)) :-
		when(nonvar(X), Y = 2), X = 1.

	test(coroutining_when_2_02, true(Y == 2)) :-
		when(ground(X), Y = 2), X = 1.

	test(coroutining_when_2_03, true(Z == 3)) :-
		when((nonvar(X), nonvar(Y)), Z = 3), X = 1, Y = 2.

	test(coroutining_when_2_04, true(Z == 3)) :-
		when((ground(X); ground(_)), Z = 3), X = 1.

	test(coroutining_when_2_05, true(Z == 3)) :-
		when((ground(_); ground(Y)), Z = 3), Y = 2.

	test(coroutining_when_2_06, true(Y == 2)) :-
		when(nonvar(X), unify_y(Y)), X = 1.

	test(coroutining_when_2_07, true(X-Y == 1-2)) :-
		when(ground(Z), X = 1),
		when(ground(Z), Y = 2),
		Z = 3.

	cleanup :-
		^^clean_text_output.

	% auxiliary predicates

	unify_y(2).

:- end_object.
