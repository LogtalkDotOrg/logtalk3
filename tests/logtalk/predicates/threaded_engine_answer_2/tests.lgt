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
		author is 'Paulo Moura',
		date is 2016/05/29,
		comment is 'Unit tests for the threaded_engine_answer/2 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_engine_answer_2_01, error(instantiation_error, logtalk(threaded_engine_answer(_,_), This))) :-
		this(This),
		threaded_engine_answer(_, _).

	throws(threaded_engine_answer_2_02, error(existence_error(engine,foo), logtalk(threaded_engine_answer(foo,_), This))) :-
		this(This),
		threaded_engine_answer(foo, _).

	succeeds(threaded_engine_create_3_03) :-
		threaded_engine_create(X, a(X), test_engine_1).

	succeeds(threaded_engine_create_3_04) :-
		threaded_engine_answer(test_engine_1, X),
		threaded_engine_answer(test_engine_1, Y),
		threaded_engine_answer(test_engine_1, Z),
		X == 1, Y == 2, Z == 3.

	fails(threaded_engine_create_3_05) :-
		threaded_engine_answer(test_engine_1, _).

	succeeds(threaded_engine_create_3_06) :-
		threaded_engine_stop(test_engine_1).

	% auxiliary predicates

	a(1). a(2). a(3).

:- end_object.
