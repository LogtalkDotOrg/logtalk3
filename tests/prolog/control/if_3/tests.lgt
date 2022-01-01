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

condition_opaque_to_cut :-
	if((!,fail), true, fail).
condition_opaque_to_cut.

condition_opaque_to_cut(1) :-
	if(!, true, fail).
condition_opaque_to_cut(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-09-25,
		comment is 'Unit tests for the soft-cut if/3 built-in predicate.'
	]).

	condition :-
		predicate_property(if(_,_,_), built_in).

	test(if_3_01, true) :-
		{if(true, true, fail)}.

	test(if_3_02, true) :-
		{if(fail, true, true)}.

	test(if_3_03, false) :-
		{if(true, fail, fail)}.

	test(if_3_04, false) :-
		{if(fail, true, fail)}.

	test(if_3_05, true(L == [1-4, 1-5, 1-6, 2-4, 2-5, 2-6, 3-4, 3-5, 3-6])) :-
		findall(X-Y, {if(a(X), b(Y), c(_))}, L).

	test(if_3_06, true(L == [7, 8, 9])) :-
		findall(Z, {if(fail, b(_), c(Z))}, L).

	test(if_3_07, true(L == [7, 8, 9])) :-
		findall(Z, {if((!,fail), b(_), c(Z))}, L).

	test(if_3_08, true) :-
		{condition_opaque_to_cut}.

	test(if_3_09, true(L == [1, 2])) :-
		findall(X, {condition_opaque_to_cut(X)}, L).

	test(if_3_10, errors([type_error(callable,3), type_error(callable,if(3,true,true))])) :-
		% try to delay the error to runtime
		three(Three),
		{if(Three, true, true)}.

	test(if_3_11, errors([type_error(callable,3), type_error(callable,if(true,3,true))])) :-
		% try to delay the error to runtime
		three(Three),
		{if(true, Three, true)}.

	test(if_3_12, errors([type_error(callable,3), type_error(callable,if(fail,true,3))])) :-
		% try to delay the error to runtime
		three(Three),
		{if(fail, true, Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
