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
		version is 1:1:1,
		author is 'Paulo Moura',
		date is 2020-11-11,
		comment is 'Unit tests for the ISO Prolog standard (;)/2 control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.6.4

	succeeds(iso_disjunction_2_01) :-
		{';'(true, fail)}.

	fails(iso_disjunction_2_02) :-
		{';'((!, fail), true)}.

	succeeds(iso_disjunction_2_03) :-
		% try to avoid a compile time error with some backends
		three(Three),
		{';'(!, call(Three))}.

	succeeds(iso_disjunction_2_04) :-
		{';'((X=1, !), X=2)},
		X == 1.

	succeeds(iso_disjunction_2_05) :-
		findall(X, {';'(X=1, X=2)}, L),
		L == [1,2].

	% tests from the Logtalk portability work

	throws(lgt_disjunction_2_06, [error(type_error(callable,3),_), error(type_error(callable,(3;true)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{(Three; true)}.

	throws(lgt_disjunction_2_07, [error(type_error(callable,3),_), error(type_error(callable,(fail;3)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{(fail; Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
