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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-08-24,
		comment is 'Unit tests for the ISO Prolog standard functor/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.1.4
	% updated for the changesa in the ISO/IEC 13211-1 Technical Corrigendum 3:2017

	test(iso_functor_3_01, true) :-
		{functor(foo(a,b,c), foo, 3)}.

	test(iso_functor_3_02, true(X-Y == foo-3)) :-
		{functor(foo(a,b,c), X, Y)}.

	test(iso_functor_3_03, variant(X, foo(_A,_B,_C))) :-
		{functor(X, foo, 3)}.

	test(iso_functor_3_04, true(X == foo)) :-
		{functor(X, foo, 0)}.

	test(iso_functor_3_05, true(A-B == mats-2)) :-
		{functor(mats(A,B), A, B)}.

	test(iso_functor_3_06, false) :-
		{functor(foo(a), foo, 2)}.

	test(iso_functor_3_07, false) :-
		{functor(foo(a), fo, 1)}.

	test(iso_functor_3_08, true(X-Y == 1-0)) :-
		{functor(1, X, Y)}.

	test(iso_functor_3_09, true(X == 1.1)) :-
		{functor(X, 1.1, 0)}.

	test(iso_functor_3_10, true) :-
		{functor([_|_], '.', 2)}.

	test(iso_functor_3_11, true) :-
		{functor([], [], 0)}.

	% in the tests that follow, try to delay the expected error to runtime

	test(iso_functor_3_12, error(instantiation_error)) :-
		{G = functor(_X, _Y, 3), call(G)}.

	test(iso_functor_3_13, error(instantiation_error)) :-
		{G = functor(_X, foo, _N), call(G)}.

	test(iso_functor_3_14, error(type_error(integer,a))) :-
		{G = functor(_X, foo, a), call(G)}.

	test(iso_functor_3_15, error(type_error(atom,1.5))) :-
		{G = functor(_F, 1.5, 1), call(G)}.

	test(iso_functor_3_16, error(type_error(atomic,foo(a)))) :-
		{G = functor(_F, foo(a), 1), call(G)}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		test(iso_functor_3_17, error(type_error(evaluable,unbounded/0))) :-
			{current_prolog_flag(max_arity, A), X is A+1, functor(_T, foo, X)}.
	:- else.
		test(iso_functor_3_17, error(representation_error(max_arity))) :-
			{current_prolog_flag(max_arity, A), X is A+1, functor(_T, foo, X)}.
	:- endif.

	test(iso_functor_3_18, error(domain_error(not_less_than_zero,-1))) :-
		{G = functor(_T, foo, -1), call(G)}.

	% tests from the Logtalk portability work

	test(lgt_functor_3_19, true(T == [])) :-
		{functor(T, [], 0)}.

:- end_object.
