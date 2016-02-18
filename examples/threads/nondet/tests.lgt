%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/nondet" example.'
	]).

	:- threaded.

	test(nondet_01) :-
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_02) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_03) :-
		threaded_once(lists::member(_, [1,2,3])).

	test(nondet_04) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1].

	test(nondet_05) :-
		threaded_call(lists::member(_, [1,2,3])),
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_06) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_07) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_08) :-
		threaded_call(lists::member(_, [1,2,3]), _),
		threaded_call(lists::member(_, [1,2,3]), Tag),
		findall(X, threaded_exit(lists::member(X, [1,2,3]), Tag), Xs),
		Xs == [1, 2, 3].

	test(nondet_09) :-
		threaded_call(lists::member(_, [1,2,3,2])).

	test(nondet_10) :-
		findall(1, threaded_exit(lists::member(2, [1,2,3,2])), L),
		L == [1, 1].

:- end_object.
