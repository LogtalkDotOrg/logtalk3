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


:- object(tests(_Index_),
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2022-05-16,
		comment is 'Unit tests for the uses/2 built-in directive.'
	]).

	:- uses(uses_2_test_object_1, [
		p/1, q/1::qq/1, r(1, Atom) as r(Atom), s(_Index_, Value) as s(Value)
	]).

	test(uses_2_01, true(X == 1)) :-
		p(X).

	test(uses_2_02, true(X == 2)) :-
		qq(X).

	test(uses_2_03, true(Xs == [one])) :-
		findall(X, r(X), Xs).

	test(uses_2_04, true(Xs == [x,y,z])) :-
		findall(X, s(X), Xs).

	test(uses_2_05, true(X == 1)) :-
		uses_2_test_object_2::p(X).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(	Dialect == eclipse; Dialect == lvm; Dialect == sicstus;
			Dialect = swi; Dialect = trealla; Dialect = yap
		)
	)).

		test(uses_2_06, true(X == 2)) :-
			uses_2_test_object_2::mp(X).

	:- else.

		- test(uses_2_06, true).

	:- endif.

	test(uses_2_07, true(X == bar)) :-
		foo(bar)::p(X).

	test(uses_2_08, true(X == baz)) :-
		foo(baz)::p(X).

	test(uses_2_09, true(X == 1)) :-
		foo(bar)::r(X).

	test(uses_2_10, true(X == 2)) :-
		foo(baz)::r(X).

	test(uses_2_11, true(L == [1,2,3])) :-
		findall(X, baz(user)::p(X), L).

	test(uses_2_12, true(L == [cp/1, d1/1, d2/1, d3/1, p/1, pp/2, q/1, r/1])) :-
		setof(P, uses_2_test_object_3(dyn)::cp(P), L).

	test(uses_2_13, true(L == [d1/1, d2/1, d3/1])) :-
		setof(N/A, P^(uses_2_test_object_3(dyn)::cp(N/A), functor(P,N,A), uses_2_test_object_3(dyn)::pp(P,(dynamic))), L).

	test(uses_2_14, true(X == 0)) :-
		uses_2_test_object_3(dyn)::p(d1(X)).

	test(uses_2_15, true(L == [0,1])) :-
		uses_2_test_object_3(dyn)::q(d2(1)),
		findall(X, uses_2_test_object_3(dyn)::p(d2(X)), L).

	test(uses_2_16, true(L == [0,1])) :-
		findall(X, uses_2_test_object_3(dyn)::r(d2(X)), L).

:- end_object.
