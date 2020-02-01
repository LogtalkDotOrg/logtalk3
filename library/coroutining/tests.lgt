%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2019-11-28,
		comment is 'Unit tests for the "coroutining" library.'
	]).

	:- uses(coroutining, [
		dif/2, freeze/2, frozen/2, when/2
	]).

	cover(coroutining).

	% dif/2 tests

	test(coroutining_dif_2_01, true) :-
		\+ dif(X, X).

	test(coroutining_dif_2_02, true) :-
		dif(X, Y), X = 1, Y = 2.

	% freeze/2 tests

	test(coroutining_freeze_2_01, true(var(Y))) :-
		freeze(_, Y = 2).

	test(coroutining_freeze_2_02, true(Y == 2)) :-
		freeze(X, Y = 2), X = 1.

	test(coroutining_freeze_2_03, true(Y == 2)) :-
		freeze(X, unify_y(Y)), X = 1.

	% frozen/2 tests

	test(coroutining_frozen_2_01, true(Goal == true)) :-
		freeze(_, _ = 2),
		frozen(_, Goal).

	- test(coroutining_frozen_2_02, true) :-
		freeze(X, Y = 2),
		frozen(X, Goal),
		^^variant(Goal, Y = 2).

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

	% auxiliary predicates

	unify_y(2).

:- end_object.
