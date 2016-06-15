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
		date is 2016/06/15,
		comment is 'Unit tests for the threaded_engine/1 built-in predicate.'
	]).

	:- threaded.

	% no engines exist
	fails(threaded_engine_1_01) :-
		threaded_engine(_).

	% no engine named "foo" exists
	fails(threaded_engine_1_02) :-
		threaded_engine(foo).

	% engine with a single solution
	succeeds(threaded_engine_1_03) :-
		threaded_engine_create(none, true, Engine),
		threaded_engine(Engine),
		threaded_engine_destroy(Engine).

	% engine with multiple solutions
	succeeds(threaded_engine_1_04) :-
		threaded_engine_create(none, repeat, Engine),
		threaded_engine(Engine),
		threaded_engine_destroy(Engine).

	% engine with no solutions
	succeeds(threaded_engine_1_05) :-
		threaded_engine_create(none, fail, Engine),
		threaded_engine(Engine),
		threaded_engine_destroy(Engine).

	% the predicate must also work as expected
	% when called with an unbound argument
	succeeds(threaded_engine_1_06) :-
		threaded_engine_create(none, true, Engine),
		threaded_engine(ReturnedEngine),
		threaded_engine_destroy(Engine),
		ReturnedEngine == Engine.

	% all existing engines must be returned
	succeeds(threaded_engine_1_07) :-
		threaded_engine_create(none, true, test_engine_1),
		threaded_engine_create(none, true, test_engine_2),
		threaded_engine_create(none, true, test_engine_3),
		setof(Engine, threaded_engine(Engine), Engines),
		Engines == [test_engine_1, test_engine_2, test_engine_3].

:- end_object.
