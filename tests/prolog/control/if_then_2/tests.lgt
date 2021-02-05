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
		date is 2020-12-26,
		comment is 'Unit tests for the ISO Prolog standard (->)/2 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.7.4

	test(iso_if_then_2_01, true) :-
		{'->'(true, true)}.

	test(iso_if_then_2_02, false) :-
		{'->'(true, fail)}.

	test(iso_if_then_2_03, false) :-
		{'->'(fail, true)}.

	test(iso_if_then_2_04, true(X == 1)) :-
		{'->'(true, X=1)}.

	test(iso_if_then_2_05, true(X == 1)) :-
		{'->'(';'(X=1, X=2), true)}.

	test(iso_if_then_2_06, true(L == [1, 2])) :-
		findall(X, {'->'(true, ';'(X=1, X=2))}, L).

	% tests from the Logtalk portability work

	test(lgt_if_then_2_07, true(L == [1])) :-
		% implicit cut in the if part
		findall(X, {'->'(';'(X=1, X=2), true)}, L).

	test(lgt_if_then_2_08, true(L == [1, 2])) :-
		% if part is cut opaque
		findall(X, {';'(X=1, X=2), '->'(!, true)}, L).

	test(lgt_if_then_2_09, true(L == [1])) :-
		% then part is cut transparent
		findall(X, {';'(X=1, X=2), '->'(true, !)}, L).

	test(lgt_if_then_2_10, errors([type_error(callable,3), type_error(callable,'->'(3,true))])) :-
		% try to delay the error to runtime
		three(Three),
		{'->'(Three, true)}.

	test(lgt_if_then_2_11, errors([type_error(callable,3), type_error(callable,'->'(true,3))])) :-
		% try to delay the error to runtime
		three(Three),
		{'->'(true, Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
