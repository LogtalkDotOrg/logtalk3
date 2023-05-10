%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2023-05-09,
		comment is 'Unit tests for the de facto Prolog standard numbervars/3 built-in predicate.'
	]).

	test(commons_numbervars_3_01, true(N == 0)) :-
		{numbervars(t, 0, N)}.

	test(commons_numbervars_3_02, false) :-
		{numbervars(t, 0, 1)}.

	test(commons_numbervars_3_03, true(N == 0)) :-
		{numbervars(1, 0, N)}.

	test(commons_numbervars_3_04, false) :-
		{numbervars(1, 0, 1)}.

	test(commons_numbervars_3_05, true(N == 0)) :-
		{numbervars(3.14, 0, N)}.

	test(commons_numbervars_3_06, false) :-
		{numbervars(3.14, 0, 1)}.

	test(commons_numbervars_3_07, true) :-
		{numbervars(T, 0, N)},
		^^assertion(ground(T)),
		^^assertion(N == 1).

	test(commons_numbervars_3_08, true) :-
		T = a(_X,_Y,_Z),
		{numbervars(T, 0, N)},
		^^assertion(ground(T)),
		^^assertion(N == 3).

	test(commons_numbervars_3_09, true) :-
		T = a(_X,_Y,_X),
		{numbervars(T, 0, N)},
		^^assertion(ground(T)),
		^^assertion(N == 2).

	test(commons_numbervars_3_10, true) :-
		T = a(_X, 1, b(_Y, c(_X), 2), 3, _W),
		{numbervars(T, 3, N)},
		^^assertion(ground(T)),
		^^assertion(N == 6).

	test(commons_numbervars_3_11, true) :-
		T = a(X,Y,Z),
		{numbervars(T, 0, _)},
		^^assertion(X == '$VAR'(0)),
		^^assertion(Y == '$VAR'(1)),
		^^assertion(Z == '$VAR'(2)).

	test(commons_numbervars_3_12, true) :-
		T = a(X,Y,Z),
		{numbervars(T, -7, N)},
		^^assertion(X == '$VAR'(-7)),
		^^assertion(Y == '$VAR'(-6)),
		^^assertion(Z == '$VAR'(-5)),
		^^assertion(N == -4).

	test(commons_numbervars_3_13, error(instantiation_error)) :-
		{numbervars(_, _, _)}.

	test(commons_numbervars_3_14, error(type_error(integer,a))) :-
		{numbervars(_, a, _)}.

	test(commons_numbervars_3_15, error(type_error(integer,a))) :-
		{numbervars(_, 1, a)}.

	% tests from the Logtalk portability work

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_numbervars_3_16, true(N == 0)) :-
			X = f(X),
			{numbervars(X, 0, N)}.

		test(lgt_numbervars_3_17, true((N == 3, A == '$VAR'(0), B == '$VAR'(1), C == '$VAR'(2)))) :-
			L = [A,B,C| L],
			{numbervars(L, 0, N)}.

	:- else.

		- test(lgt_numbervars_3_16, true(N == 0), [note('STO')]) :-
			% STO; Undefined.
			X = f(X),
			{numbervars(X, 0, N)}.

		- test(lgt_numbervars_3_17, true((N == 3, A == '$VAR'(0), B == '$VAR'(1), C == '$VAR'(2))), [note('STO')]) :-
			% STO; Undefined.
			L = [A,B,C| L],
			{numbervars(L, 0, N)}.

	:- endif.

:- end_object.
