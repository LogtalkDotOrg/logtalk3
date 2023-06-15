%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the ISO Prolog standard once/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.15.2.4

	test(iso_once_1_01, true) :-
		{once(!)}.

	test(iso_once_1_02, true(L == [1, 2])) :-
		findall(X, {once(!), (X=1; X=2)}, L).

	test(iso_once_1_03, deterministic) :-
		{once(repeat)}.

	test(iso_once_1_04, false) :-
		{once(fail)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_once_1_05, true) :-
			{once((X = f(X)))}.

	:- else.

		- test(iso_once_1_05, true, [note('STO')]) :-
			% STO; Undefined
			{once((X = f(X)))}.

	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_once_1_06, errors([type_error(callable,3), type_error(callable,':'(user,3))])) :-
		% try to delay the error to runtime
		three(G),
		{once(G)}.

	test(eddbali_once_1_07, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{once(X)}.

	% tests from the Logtalk portability work

	test(lgt_once_1_08, errors([existence_error(procedure,foobar/1), existence_error(procedure,':'(user,foobar/1))])) :-
		{	set_prolog_flag(unknown, error),
			once(foobar(_))
		}.

	% auxiliary predicate used to delay errors to runtime

	variable(_).

	three(3).

:- end_object.
