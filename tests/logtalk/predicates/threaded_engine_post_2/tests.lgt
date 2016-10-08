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
		date is 2016/06/10,
		comment is 'Unit tests for the threaded_engine_post/2 built-in predicate.'
	]).

	:- threaded.

	% engine argument must be bound at runtime (but no error at compile time)
	throws(threaded_engine_post_2_01, error(instantiation_error, logtalk(threaded_engine_post(_,_), This))) :-
		this(This),
		threaded_engine_post(_, _).

	% engine must exist
	throws(threaded_engine_post_2_02, error(existence_error(engine,foo), logtalk(threaded_engine_post(foo,_), This))) :-
		this(This),
		threaded_engine_post(foo, _).

	% posting terms to an engine term queue is independent of
	% the status of the engine goal and its solutions if any

	succeeds(threaded_engine_post_2_03) :-
		threaded_engine_create(none, repeat, test_engine_1),
		threaded_engine_post(test_engine_1, term).

	succeeds(threaded_engine_post_2_04) :-
		threaded_engine_create(none, true, test_engine_2),
		threaded_engine_post(test_engine_1, term).

	succeeds(threaded_engine_post_2_05) :-
		threaded_engine_create(none, fail, test_engine_3),
		threaded_engine_post(test_engine_1, term).

	succeeds(threaded_engine_post_2_06) :-
		threaded_engine_create(none, throw(error), test_engine_4),
		threaded_engine_post(test_engine_1, term).

:- end_object.
