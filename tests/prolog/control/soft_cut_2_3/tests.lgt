%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2018/02/06,
		comment is 'Unit tests for the soft-cut (*->)/2 control construct that is becoming a de facto standard.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	succeeds(commons_soft_cut_2_3_01) :-
		{'*->'(true, true)}.

	fails(commons_soft_cut_2_3_02) :-
		{'*->'(true, fail)}.

	fails(commons_soft_cut_2_3_03) :-
		{'*->'(fail, true)}.

	fails(commons_soft_cut_2_3_04) :-
		{'*->'(fail, fail)}.

	succeeds(commons_soft_cut_2_3_05) :-
		{';'('*->'(true, true), fail)}.

	succeeds(commons_soft_cut_2_3_06) :-
		{';'('*->'(fail, true), true)}.

	fails(commons_soft_cut_2_3_07) :-
		{';'('*->'(true, fail), fail)}.

	fails(commons_soft_cut_2_3_08) :-
		{';'('*->'(fail, true), fail)}.

	succeeds(commons_soft_cut_2_3_09) :-
		findall(X-Y, {';'('*->'(a(X),b(Y)), c(_))}, L),
		L == [1-4, 1-5, 1-6, 2-4, 2-5, 2-6, 3-4, 3-5, 3-6].

	succeeds(commons_soft_cut_2_3_10) :-
		findall(Z, {';'('*->'(fail,b(_)), c(Z))}, L),
		L == [7, 8, 9].

	succeeds(commons_soft_cut_2_3_11) :-
		findall(Z, {';'('*->'((!,fail),b(_)), c(Z))}, L),
		L == [7, 8, 9].

	succeeds(commons_soft_cut_2_3_12) :-
		{condition_opaque_to_cut_2}.

	succeeds(commons_soft_cut_2_3_13) :-
		findall(X, {condition_opaque_to_cut_2(X)}, L),
		L == [1, 2].

	succeeds(commons_soft_cut_2_3_14) :-
		{condition_opaque_to_cut_3}.

	succeeds(commons_soft_cut_2_3_15) :-
		findall(X, {condition_opaque_to_cut_3(X)}, L),
		L == [1, 2].

	% tests from the Logtalk portability work

	succeeds(lgt_soft_cut_2_3_16) :-
		% if part is cut opaque
		findall(X, {';'(X=1, X=2), '*->'(!, true)}, L),
		L == [1, 2].

	succeeds(lgt_soft_cut_2_3_17) :-
		% then part is cut transparent
		findall(X, {';'(X=1, X=2), '*->'(true, !)}, L),
		L == [1].

:- end_object.
