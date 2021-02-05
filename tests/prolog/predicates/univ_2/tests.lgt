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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-07-29,
		comment is 'Unit tests for the ISO Prolog standard (=..)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.3.4

	test(iso_univ_2_01, true) :-
		{foo(a,b) =.. [foo,a,b]}.

	test(iso_univ_2_02, true(X == foo(a,b))) :-
		{X =.. [foo,a,b]}.

	test(iso_univ_2_03, true(L == [foo,a,b])) :-
		{foo(a,b) =.. L}.

	test(iso_univ_2_04, true((X == a, Y == b))) :-
		{foo(X,b) =.. [foo,a,Y]}.

	test(iso_univ_2_05, true) :-
		{1 =.. [1]}.

	test(iso_univ_2_06, fail) :-
		{foo(a,b) =.. [foo,b,a]}.

	test(iso_univ_2_07, error(instantiation_error)) :-
		{_ =.. _}.

	test(iso_univ_2_08, error(instantiation_error)) :-
		{_ =.. [foo,a|_]}.

	test(iso_univ_2_09, error(type_error(list,[foo|bar]))) :-
		{_ =.. [foo|bar]}.

	test(iso_univ_2_10, error(instantiation_error)) :-
		{_ =.. [_Foo,bar]}.

	test(iso_univ_2_11, error(type_error(atom,3))) :-
		{_ =.. [3,1]}.

	test(iso_univ_2_12, error(type_error(atom,1.1))) :-
		{_ =.. [1.1,foo]}.

	test(iso_univ_2_13, error(type_error(atom,a(b)))) :-
		{_ =.. [a(b),1]}.

	test(iso_univ_2_14, error(type_error(list,4))) :-
		{_ =.. 4}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		test(iso_univ_2_15, true) :-
			{f(X) =.. [f,u(X)]}.
	:- else.
		- test(iso_univ_2_15, true) :-
			% STO; Undefined
			{f(X) =.. [f,u(X)]}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_univ_2_16, error(type_error(atomic,f(a)))) :-
		{_ =.. [f(a)]}.

	test(sics_univ_2_17, error(domain_error(non_empty_list,[]))) :-
		{_ =.. []}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		test(sics_univ_2_18, true) :-
			true.
	:- else.
		test(sics_univ_2_18, error(representation_error(max_arity))) :-
			{current_prolog_flag(max_arity, Max)},
			N is Max+1, list_of(N, 1, L),
			{_ =.. [f|L]}.
	:- endif.

	% tests from the Logtalk portability work

	test(lgt_univ_2_19, fail) :-
		{1 =.. [_, _]}.

	test(lgt_univ_2_20, fail) :-
		{a =.. [_, _]}.

	test(lgt_univ_2_21, fail) :-
		{1 =.. []}.

	test(lgt_univ_2_22, fail) :-
		{a =.. []}.

	test(lgt_univ_2_23, fail) :-
		{a(1) =.. []}.

	test(lgt_univ_2_24, true(L == [a])) :-
		{a =.. L}.

	test(lgt_univ_2_25, true(L == [1])) :-
		{1 =.. L}.

	test(lgt_univ_2_26, true(L == [1.0])) :-
		{1.0 =.. L}.

	% auxiliary predicates

	:- if(\+ current_prolog_flag(max_arity, unbounded)).
		list_of(0, _, []).
		list_of(N, A, [A|L]) :-
			N > 0, N1 is N-1,
			list_of(N1, A, L).
	:- endif.

:- end_object.
