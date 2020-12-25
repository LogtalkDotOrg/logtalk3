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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-02-07,
		comment is 'Unit tests for the goal_expansion/2 user-defined hook predicate.'
	]).

	% see also the tests for the expand_goal/2 predicate, which further
	% test the goal_expansion/2 user-defined hook predicate

	test(goal_expansion_2_01, true(L == [1, a, b, c])) :-
		findall(X, object1::a(X), L).

	test(goal_expansion_2_02, true(L == [1, 2, 3])) :-
		findall(X, object1::b(X), L).

	test(goal_expansion_2_03, true(X == 1)) :-
		^^suppress_text_output,
		object1::fp(X).

	test(goal_expansion_2_04, true(p(C,D,E) == p(1,2,3))) :-
		object2::p(C, D, E).

:- end_object.
