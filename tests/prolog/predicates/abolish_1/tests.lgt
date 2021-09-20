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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(foo/1).
foo(X) :- call(X), call(X).
foo(X) :- call(X) -> call(X).

bar(_X) :- true.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:1,
		author is 'Paulo Moura',
		date is 2020-09-20,
		comment is 'Unit tests for the ISO Prolog standard abolish/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

	test(iso_abolish_1_01, true) :-
		{abolish(foo/2)}.

	test(iso_abolish_1_02, error(instantiation_error)) :-
		{abolish(foo/_)}.

	test(iso_abolish_1_03, error(type_error(predicate_indicator,foo))) :-
		{abolish(foo)}.

	test(iso_abolish_1_04, error(type_error(predicate_indicator,foo(X)))) :-
		{abolish(foo(X))}.

	test(iso_abolish_1_05, errors([permission_error(modify,static_procedure,abolish/1), permission_error(modify,static_procedure,':'(user,abolish/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{abolish(abolish/1)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_abolish_1_06, true) :-
		{abolish(foo/1)}.

	test(eddbali_abolish_1_07, true(L == [ant, bee])) :-
		findall(X, {insect(X), abolish(insect/1)}, L).

	test(eddbali_abolish_1_08, errors([permission_error(modify,static_procedure,bar/1), permission_error(modify,static_procedure,':'(user,bar/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{abolish(bar/1)}.

	test(eddbali_abolish_1_09, error(type_error(integer,a))) :-
		{abolish(foo/a)}.

	test(eddbali_abolish_1_10, error(domain_error(not_less_than_zero,-1))) :-
		{abolish(foo/(-1))}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		test(eddbali_abolish_1_11).
	:- else.
		test(eddbali_abolish_1_11, error(representation_error(max_arity))) :-
			current_prolog_flag(max_arity, MaxArity),
			X is MaxArity + 1,
			{abolish(foo/X)}.
	:- endif.

	test(eddbali_abolish_1_12, error(type_error(atom,5))) :-
		{abolish(5/2)}.

	test(eddbali_abolish_1_13, error(type_error(predicate_indicator,insect))) :-
		{abolish(insect)}.

	% tests from the ECLiPSe test suite

	test(eclipse_abolish_1_14, error(instantiation_error)) :-
		{abolish(_)}.

	test(eclipse_abolish_1_15, error(instantiation_error)) :-
		{abolish(_/2)}.

	% tests from the Logtalk portability work

	test(lgt_abolish_1_16, errors([existence_error(procedure,baz/2), existence_error(procedure,':'(user,baz/2))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{assertz(baz(1, 2)),
		 abolish(baz/2),
	     baz(_, _)}.

:- end_object.
