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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the ISO Prolog standard (\\+)/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.15.1.4

	test(iso_not_1_01, false) :-
		{'\\+'(true)}.

	test(iso_not_1_02, false) :-
		{'\\+'(!)}.

	test(iso_not_1_03, true) :-
		{'\\+'((!,fail))}.

	test(iso_not_1_04, true(L == [1, 2])) :-
		findall(X, {(X=1;X=2), '\\+'((!,fail))}, L).

	test(iso_not_1_05, true) :-
		{'\\+'(4 = 5)}.

	test(iso_not_1_06, errors([type_error(callable,3), type_error(callable,'\\+'(3))])) :-
		% the second exception term is a common but not strictly conforming alternative
		% try to force runtime goal checking
		G = '\\+'(3), {G}.

	test(iso_not_1_07, error(instantiation_error)) :-
		% try to force runtime goal checking
		G = '\\+'(_X), {G}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_not_1_08, false) :-
			{'\\+'(X=f(X))}.

	:- else.

		- test(iso_not_1_08, false, [note('STO')]) :-
			% STO; Undefined
			{'\\+'(X=f(X))}.

	:- endif.

	% tests from the Logtalk portability work

	test(lgt_not_1_09, true(var(X))) :-
		{'\\+'('\\+'(X=1))}.

	test(lgt_not_1_10, errors([existence_error(procedure,foobar/1), existence_error(procedure,':'(user,foobar/1))])) :-
		{	set_prolog_flag(unknown, error),
			'\\+'(foobar(_))
		}.

:- end_object.
