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
		version is 1:0:2,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the de facto standard Prolog gcd/2 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_gcd_2_01, true(GCD == 3)) :-
		{GCD is gcd(9, 6)}.

	test(lgt_gcd_2_02, true(GCD == 3)) :-
		{GCD is gcd(6, 9)}.

	test(lgt_gcd_2_03, true(GCD == 3)) :-
		{GCD is gcd(-6, 9)}.

	test(lgt_gcd_2_04, true(GCD == 1)) :-
		{GCD is gcd(7, 5)}.

	test(lgt_gcd_2_05, true(GCD == 1)) :-
		{GCD is gcd(5, 7)}.

	test(lgt_gcd_2_06, true(GCD == 1)) :-
		{GCD is gcd(5, -7)}.

	test(lgt_gcd_2_07, error(instantiation_error)) :-
		variable(Var),
		{_ is gcd(4, Var)}.

	test(lgt_gcd_2_08, error(instantiation_error)) :-
		variable(Var),
		{_ is gcd(Var, 4)}.

	test(lgt_gcd_2_09, error(type_error(integer,2.0))) :-
		fp(Float),
		{_ is gcd(4, Float)}.

	test(lgt_gcd_2_10, error(type_error(integer,2.0))) :-
		fp(Float),
		{_ is gcd(Float, 4)}.

	test(lgt_gcd_2_11, error(type_error(evaluable,a/0))) :-
		a(A),
		{_ is gcd(A, 4)}.

	test(lgt_gcd_2_12, error(type_error(evaluable,a/0))) :-
		a(A),
		{_ is gcd(4, A)}.

	% auxiliary predicates to delay errors to runtime

	variable(_).

	fp(2.0).

	a(a).

:- end_object.
