%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-06-23,
		comment is 'Unit tests for the soft-cut if/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	succeeds(if_3_01) :-
		{if(true, true, fail)}.

	succeeds(if_3_02) :-
		{if(fail, true, true)}.

	fails(if_3_03) :-
		{if(true, fail, fail)}.

	fails(if_3_04) :-
		{if(fail, true, fail)}.

	succeeds(if_3_05) :-
		findall(X-Y, {if(a(X), b(Y), c(_))}, L),
		L == [1-4, 1-5, 1-6, 2-4, 2-5, 2-6, 3-4, 3-5, 3-6].

	succeeds(if_3_06) :-
		findall(Z, {if(fail, b(_), c(Z))}, L),
		L == [7, 8, 9].

	succeeds(if_3_07) :-
		findall(Z, {if((!,fail), b(_), c(Z))}, L),
		L == [7, 8, 9].

	succeeds(if_3_08) :-
		{condition_opaque_to_cut}.

	succeeds(if_3_09) :-
		findall(X, {condition_opaque_to_cut(X)}, L),
		L == [1, 2].

	throws(if_3_10, [error(type_error(callable,3),_), error(type_error(callable,if(3,true,true)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{if(Three, true, true)}.

	throws(if_3_11, [error(type_error(callable,3),_), error(type_error(callable,if(true,3,true)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{if(true, Three, true)}.

	throws(if_3_12, [error(type_error(callable,3),_), error(type_error(callable,if(fail,true,3)),_)]) :-
		% try to delay the error to runtime
		three(Three),
		{if(fail, true, Three)}.

	% auxiliary predicates

	three(3).

:- end_object.
