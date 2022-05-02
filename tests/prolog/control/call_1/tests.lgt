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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.3.4

b(X) :-
	Y = (write(X), X),
	call(Y).

a(1).
a(2).


% calls to declared predicates with no clauses must fail

:- dynamic(unicorn/0).

:- multifile(fenix/1).

:- discontiguous(scattered/2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:1,
		author is 'Paulo Moura',
		date is 2022-05-02,
		comment is 'Unit tests for the ISO Prolog standard call/1 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.3.4

	test(iso_call_1_01, true) :-
		{call(!)}.

	test(iso_call_1_02, false) :-
		{call(fail)}.

	test(iso_call_1_03, false) :-
		{call((fail, _X))}.

	test(iso_call_1_04, false) :-
		X = 1,
		{call((fail, call(X)))}.

	test(iso_call_1_05, error(instantiation_error)) :-
		^^suppress_text_output,
		{b(_)}.

	test(iso_call_1_06, errors([type_error(callable,(write(3),3)), type_error(callable,3)])) :-
		% the second exception term is a common but not strictly conforming alternative
		^^suppress_text_output,
		{b(3)}.

	test(iso_call_1_07, true(Z-X == !-1)) :-
		{(Z = !, call((Z=!, a(X), Z)))}.

	test(iso_call_1_08, true(L == [!-1, !-2])) :-
		findall(Z-X, {call((Z=!, a(X), Z))}, L).

	test(iso_call_1_09, error(instantiation_error)) :-
		^^suppress_text_output,
		{call((write(3), _X))}.

	test(iso_call_1_10, errors([type_error(callable,1), type_error(callable,':'(user,1))])) :-
		^^suppress_text_output,
		X = 1,
		{call((write(3), call(X)))}.

	test(iso_call_1_11, error(instantiation_error)) :-
		{call(_X)}.

	test(iso_call_1_12, errors([type_error(callable,1), type_error(callable,':'(user,1))])) :-
		X = 1,
		{call(X)}.

	test(iso_call_1_13, errors([type_error(callable,(fail,1)), type_error(callable,1)])) :-
		% the second exception term is a common but not strictly conforming alternative
		X = 1,
		{call((fail, X))}.

	test(iso_call_1_14, errors([type_error(callable,(write(3),1)), type_error(callable,1)])) :-
		% the second exception term is a common but not strictly conforming alternative
		^^suppress_text_output,
		X = 1,
		{call((write(3), X))}.

	test(iso_call_1_15, errors([type_error(callable,(1;true)), type_error(callable,1)])) :-
		% the second exception term is a common but not strictly conforming alternative
		X = 1,
		{call((X; true))}.

	% tests from the Logtalk portability work

	test(lgt_call_1_16, false) :-
		{call(unicorn)}.

	test(lgt_call_1_17, false) :-
		{call(fenix(_))}.

	test(lgt_call_1_18, false) :-
		{call(scattered(_, _))}.

:- end_object.
