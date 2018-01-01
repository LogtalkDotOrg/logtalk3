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
		comment is 'Unit tests for the threaded_engine_self/1 built-in predicate.'
	]).

	:- threaded.

	% test calling threaded_engine_self/1 with unbound argument
	succeeds(threaded_engine_self_1_01) :-
		threaded_engine_create(none, return, test_engine_1),
		threaded_engine_next(test_engine_1, Engine),
		Engine == test_engine_1.

	% test calling threaded_engine_self/1 with bound and correct argument
	succeeds(threaded_engine_self_1_02) :-
		threaded_engine_create(none, correct, test_engine_2),
		threaded_engine_next(test_engine_2, Answer),
		Answer == none.

	% test calling threaded_engine_self/1 with bound but incorrect argument
	fails(threaded_engine_self_1_03) :-
		threaded_engine_create(none, wrong, test_engine_3),
		threaded_engine_next(test_engine_3, _).

	% calls outside the context of an engine must fail
	fails(threaded_engine_self_1_04) :-
		threaded_engine_self(_).

	% auxiliary predicates

	return :-
		threaded_engine_self(Engine),
		threaded_engine_yield(Engine).

	correct :-
		threaded_engine_self(test_engine_2).

	wrong :-
		threaded_engine_self(wrong).

:- end_object.
