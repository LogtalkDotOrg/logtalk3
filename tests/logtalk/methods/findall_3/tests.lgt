%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/30,
		comment is 'Unit tests for the findall/3 built-in method.'
	]).

	succeeds(findall_3_01) :-
		findall(X, a(X, _), L),
		L == [1, 2, 3, 4].

	succeeds(findall_3_02) :-
		findall(Y-L, findall(X, a(X, Y), L), LL),
		LL = [_-[1,2,3,4]].

	% the following tests are taken from the ISO Prolog Core standard

	succeeds(findall_3_03) :-
		findall(X, (X=1; X=2), L),
		L == [1, 2].

	succeeds(findall_3_04) :-
		findall(X+_Y, (X=1), L),
		L = [1+_].

	succeeds(findall_3_05) :-
		findall(_X, fail, L),
		L == [].

	succeeds(findall_3_06) :-
		findall(X, (X=1; X=1), L),
		L == [1, 1].

	succeeds(findall_3_07) :-
		findall(X, (X=1; X=2), [X,Y]),
		X == 1, Y == 2.

	fails(findall_3_08) :-
		findall(X, (X=2; X=1), [1,2]).

	throws(findall_3_09, error(instantiation_error, logtalk(call(_),This))) :-
		this(This),
		findall(_X, _Goal, _L).

	throws(findall_3_10, error(type_error(callable,4), logtalk(call(4),This))) :-
		this(This),
		Goal = 4,
		findall(_X, Goal, _L).

	% data for some of the tests

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
