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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2021-08-24,
		comment is 'Unit tests for the ISO Prolog standard arg/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.2.4

	test(iso_arg_3_01, true) :-
		{arg(1, foo(a,b), a)}.

	test(iso_arg_3_02, true(X == a)) :-
		{arg(1, foo(a,b), X)}.

	test(iso_arg_3_03, true(X == a)) :-
		{arg(1, foo(X,b), a)}.

	test(iso_arg_3_04, true(Y == X)) :-
		{arg(1, foo(X,b), Y)}.

	test(iso_arg_3_05, false) :-
		{arg(1, foo(a,b), b)}.

	test(iso_arg_3_06, false) :-
		{arg(0, foo(a,b), foo)}.

	test(iso_arg_3_07, false) :-
		{arg(3, foo(3,4), _)}.

	test(iso_arg_3_08, error(instantiation_error)) :-
		% try to delay the expected error to runtime
		{G = arg(_, foo(a,b), a), call(G)}.

	test(iso_arg_3_09, error(instantiation_error)) :-
		% try to delay the expected error to runtime
		{G = arg(1, _, a), call(G)}.

	test(iso_arg_3_10, error(type_error(compound,atom))) :-
		% try to delay the expected error to runtime
		{G = arg(0, atom, _), call(G)}.

	test(iso_arg_3_11, error(type_error(compound,3))) :-
		% try to delay the expected error to runtime
		{G = arg(0, 3, _), call(G)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		test(iso_arg_3_12, true) :-
			{arg(1, foo(X), u(X))}.
	:- else.
		- test(iso_arg_3_12, true) :-
			{arg(1, foo(X), u(X))}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_arg_3_13, error(domain_error(not_less_than_zero,-3))) :-
		% try to delay the expected error to runtime
		{G = arg(-3, foo(a,b), _A), call(G)}.

	test(eddbali_arg_3_14, error(type_error(integer,a))) :-
		% try to delay the expected error to runtime
		{G = arg(a, foo(a,b), _X), call(G)}.

	test(eddbali_arg_3_15, true(X-Y == a-b)) :-
		{arg(2, foo(a,f(X,b),c), f(a,Y))}.

	test(sics_arg_3_16, error(type_error(compound,3))) :-
		% try to delay the expected error to runtime
		{G = arg(1, 3, _A), call(G)}.

	% tests from the Logtalk portability work

	test(lgt_arg_3_17, true(Arg == Head)) :-
		{arg(1, [Head| _], Arg)}.

	test(lgt_arg_3_18) :-
		{arg(2, [_| Tail], Arg)},
		Arg == Tail.

	test(lgt_arg_3_19, true(Arg == [])) :-
		{arg(2, [_], Arg)}.

	test(lgt_arg_3_20, true(Arg == (1,2,3))) :-
		{arg(1, {1,2,3}, Arg)}.

:- end_object.
