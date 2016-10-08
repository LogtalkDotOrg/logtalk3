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
		comment is 'Unit tests for the threaded_engine_destroy/1 built-in predicate.'
	]).

	:- threaded.

	% engine argument must be bound at runtime (but no error at compile time)
	throws(threaded_engine_destroy_1_01, error(instantiation_error, logtalk(threaded_engine_destroy(_), This))) :-
		this(This),
		threaded_engine_destroy(_).

	% engine must exist
	throws(threaded_engine_destroy_1_02, error(existence_error(engine,foo), logtalk(threaded_engine_destroy(foo), This))) :-
		this(This),
		threaded_engine_destroy(foo).

	% engine destroy should always be successful independent
	% of engine thread state and engine goal results 

	succeeds(threaded_engine_destroy_1_03) :-
		threaded_engine_create(none, repeat, test_engine_1),
		threaded_engine_destroy(test_engine_1),
		\+ threaded_engine(test_engine_1),
		this(This), logtalk::entity_prefix(This, Prefix), \+ thread_peek_message(Prefix, _).

	succeeds(threaded_engine_destroy_1_04) :-
		threaded_engine_create(none, true, test_engine_2),
		threaded_engine_destroy(test_engine_2),
		\+ threaded_engine(test_engine_2),
		this(This), logtalk::entity_prefix(This, Prefix), \+ thread_peek_message(Prefix, _).

	succeeds(threaded_engine_destroy_1_05) :-
		threaded_engine_create(none, fail, test_engine_3),
		threaded_engine_destroy(test_engine_3),
		\+ threaded_engine(test_engine_3),
		this(This), logtalk::entity_prefix(This, Prefix), \+ thread_peek_message(Prefix, _).

	succeeds(threaded_engine_destroy_1_06) :-
		threaded_engine_create(none, throw(error), test_engine_4),
		threaded_engine_destroy(test_engine_4),
		\+ threaded_engine(test_engine_4),
		this(This), logtalk::entity_prefix(This, Prefix), \+ thread_peek_message(Prefix, _).

:- end_object.
