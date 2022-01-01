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


% database for the tests

a(1). a(2). a(3).

b(4). b(5). b(6).

c(7). c(8). c(9).

condition_opaque_to_cut_2 :-
	'*->'((!,fail), true).
condition_opaque_to_cut_2.

condition_opaque_to_cut_2(1) :-
	'*->'(!, true).
condition_opaque_to_cut_2(2).

condition_opaque_to_cut_3 :-
	';'('*->'((!,fail), true), fail).
condition_opaque_to_cut_3.

condition_opaque_to_cut_3(1) :-
	';'('*->'(!, true), fail).
condition_opaque_to_cut_3(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-12-26,
		comment is 'Unit tests for the soft-cut (*->)/2 control construct that is becoming a de facto standard.'
	]).

	condition :-
		predicate_property('*->'(_,_), built_in).

	test(commons_soft_cut_2_3_01, true) :-
		{'*->'(true, true)}.

	test(commons_soft_cut_2_3_02, false) :-
		{'*->'(true, fail)}.

	test(commons_soft_cut_2_3_03, false) :-
		{'*->'(fail, true)}.

	test(commons_soft_cut_2_3_04, false) :-
		{'*->'(fail, fail)}.

	test(commons_soft_cut_2_3_05, true) :-
		{';'('*->'(true, true), fail)}.

	test(commons_soft_cut_2_3_06, true) :-
		{';'('*->'(fail, true), true)}.

	test(commons_soft_cut_2_3_07, false) :-
		{';'('*->'(true, fail), fail)}.

	test(commons_soft_cut_2_3_08, false) :-
		{';'('*->'(fail, true), fail)}.

	test(commons_soft_cut_2_3_09, true(L == [1-4, 1-5, 1-6, 2-4, 2-5, 2-6, 3-4, 3-5, 3-6])) :-
		findall(X-Y, {';'('*->'(a(X),b(Y)), c(_))}, L).

	test(commons_soft_cut_2_3_10, true(L == [7, 8, 9])) :-
		findall(Z, {';'('*->'(fail,b(_)), c(Z))}, L).

	test(commons_soft_cut_2_3_11, true(L == [7, 8, 9])) :-
		findall(Z, {';'('*->'((!,fail),b(_)), c(Z))}, L).

	test(commons_soft_cut_2_3_12, true) :-
		{condition_opaque_to_cut_2}.

	test(commons_soft_cut_2_3_13, true(L == [1, 2])) :-
		findall(X, {condition_opaque_to_cut_2(X)}, L).

	test(commons_soft_cut_2_3_14, true) :-
		{condition_opaque_to_cut_3}.

	test(commons_soft_cut_2_3_15, true(L == [1, 2])) :-
		findall(X, {condition_opaque_to_cut_3(X)}, L).

	% tests from the Logtalk portability work

	test(lgt_soft_cut_2_3_16, true(L == [1, 2])) :-
		% if part is cut opaque
		findall(X, {';'(X=1, X=2), '*->'(!, true)}, L).

	test(lgt_soft_cut_2_3_17, true(L == [1])) :-
		% then part is cut transparent
		findall(X, {';'(X=1, X=2), '*->'(true, !)}, L).

	test(lgt_soft_cut_2_3_18, errors([type_error(callable,3), type_error(callable,';'('*->'(3,true),fail))])) :-
		% try to delay the error to runtime
		three(Three),
		{';'('*->'(Three, true), fail)}.

	test(lgt_soft_cut_2_3_19, errors([type_error(callable,3), type_error(callable,';'('*->'(true,3),fail))])) :-
		% try to delay the error to runtime
		three(Three),
		{';'('*->'(true, Three), fail)}.

	test(lgt_soft_cut_2_3_20, errors([type_error(callable,3), type_error(callable,';'('*->'(fail,true),3))])) :-
		% try to delay the error to runtime
		three(Three),
		{';'('*->'(fail, true), Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
