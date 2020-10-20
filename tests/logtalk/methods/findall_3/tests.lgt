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


% silence warning about no shared variable between template and test
:- set_logtalk_flag(suspicious_calls, silent).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-10-20,
		comment is 'Unit tests for the findall/3 built-in method.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	test(findall_3_01, true(L == [1, 2, 3, 4])) :-
		findall(X, a(X, _), L).

	test(findall_3_02, true(variant(LL, [_-[1,2,3,4]]))) :-
		findall(Y-L, findall(X, a(X, Y), L), LL).

	% the following tests are taken from the ISO Prolog Core standard

	test(findall_3_03, true(L == [1, 2])) :-
		findall(X, (X=1; X=2), L).

	test(findall_3_04, true(variant(L, [1+_]))) :-
		findall(X+_Y, (X=1), L).

	test(findall_3_05, true(L == [])) :-
		findall(_X, fail, L).

	test(findall_3_06, true(L == [1, 1])) :-
		findall(X, (X=1; X=1), L).

	test(findall_3_07, true(X-Y == 1-2)) :-
		findall(X, (X=1; X=2), [X,Y]).

	test(findall_3_08, false) :-
		findall(X, (X=2; X=1), [1,2]).

	test(findall_3_09, error(instantiation_error)) :-
		findall(_X, _Goal, _L).

	test(findall_3_10, error(type_error(callable,4))) :-
		Goal = 4,
		findall(_X, Goal, _L).

	% data for some of the tests

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
