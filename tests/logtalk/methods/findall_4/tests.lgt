%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2017/05/29,
		comment is 'Unit tests for the findall/4 built-in method.'
	]).

	test(findall_4_01) :-
		findall(X, a(X, _), L, []),
		L == [1, 2, 3, 4].

	test(findall_4_02) :-
		findall(X, a(X, _), L, [5, 6, 7]),
		L == [1, 2, 3, 4, 5, 6, 7].

	test(findall_4_03) :-
		findall(Y-L, findall(X, a(X, Y), L, [5, 6, 7]), LL),
		LL = [_-[1,2,3,4,5,6,7]].

	test(findall_4_04) :-
		findall(_, fail, L, [5, 6, 7]),
		L == [5, 6, 7].

	throws(findall_4_05, error(instantiation_error, logtalk(call(_),This))) :-
		this(This),
		findall(_X, _Goal, _L, _T).

	throws(findall_4_06, error(type_error(callable,4), logtalk(call(4),This))) :-
		this(This),
		Goal = 4,
		findall(_X, Goal, _L, _T).

	% data for some of the tests

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
