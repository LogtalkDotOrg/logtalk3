%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/06/15,
		comment is 'Unit tests for the threaded_engine_next/2 built-in predicate.'
	]).

	:- threaded.

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% engine name must be bound at runtime (but no error at compile time)
	throws(threaded_engine_next_2_01, error(instantiation_error, logtalk(threaded_engine_next(_,_), This))) :-
		this(This),
		threaded_engine_next(_, _).

	% engine must exist
	throws(threaded_engine_next_2_02, error(existence_error(engine,foo), logtalk(threaded_engine_next(foo,_), This))) :-
		this(This),
		threaded_engine_next(foo, _).

	% create engine for the following tests
	succeeds(threaded_engine_next_2_03) :-
		threaded_engine_create(X, a(X), test_engine_1).

	% all solutions must be retrievable
	succeeds(threaded_engine_next_2_04) :-
		threaded_engine_next(test_engine_1, X),
		threaded_engine_next(test_engine_1, Y),
		threaded_engine_next(test_engine_1, Z),
		X == 1, Y == 2, Z == 3.

	% no more answers
	fails(threaded_engine_next_2_05) :-
		threaded_engine_next(test_engine_1, _).

	% no more answers (must keep failing)
	fails(threaded_engine_next_2_06) :-
		threaded_engine_next(test_engine_1, _).

	% engine with no goal solutions
	fails(threaded_engine_next_2_07) :-
		threaded_engine_create(_, fail, test_engine_2),
		threaded_engine_next(test_engine_2, _).

	% engine with a goal that throws an exception
	throws(threaded_engine_next_2_08, error(error, logtalk(threaded_engine_next(_,_), This))) :-
		this(This),
		threaded_engine_create(_, throw(error), test_engine_3),
		threaded_engine_next(test_engine_3, _).

	% after the exception, there cannot be any solutions
	fails(threaded_engine_next_2_09) :-
		threaded_engine_next(test_engine_3, _).

	% auxiliary predicates

	a(1). a(2). a(3).

:- end_object.
