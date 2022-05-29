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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2022-05-29,
		comment is 'Unit tests for the ISO Prolog standard (;)/2 control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.6.4

	test(iso_disjunction_2_01, true) :-
		{';'(true, fail)}.

	test(iso_disjunction_2_02, false) :-
		{';'((!, fail), true)}.

	test(iso_disjunction_2_03, true) :-
		% try to avoid a compile time error with some backends
		three(Three),
		{';'(!, call(Three))}.

	test(iso_disjunction_2_04, true(X == 1)) :-
		{';'((X=1, !), X=2)}.

	test(iso_disjunction_2_05, true(L == [1,2])) :-
		findall(X, {';'(X=1, X=2)}, L).

	% tests from the Logtalk portability work

	test(lgt_disjunction_2_06, errors([type_error(callable,3), type_error(callable,':'(user,3)), type_error(callable,(3;true))])) :-
		% try to delay the error to runtime; the second exception term
		% is used in some of the Prolog compilers supporting modules
		three(Three),
		{(Three; true)}.

	test(lgt_disjunction_2_07, errors([type_error(callable,3), type_error(callable,':'(user,3)), type_error(callable,(fail;3))])) :-
		% try to delay the error to runtime; the second exception term
		% is used in some of the Prolog compilers supporting modules
		three(Three),
		{(fail; Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
