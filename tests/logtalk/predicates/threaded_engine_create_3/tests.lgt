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
		date is 2016/05/28,
		comment is 'Unit tests for the threaded_engine_create/3 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_engine_create_3_01, error(instantiation_error, logtalk(threaded_engine_create(_,_,_), _))) :-
		{threaded_engine_create(_, _, _)}.

	throws(threaded_engine_create_3_02, error(type_error(callable,1), logtalk(threaded_engine_create(_,1,_), _))) :-
		{threaded_engine_create(_, 1, _)}.

	throws(threaded_engine_create_3_03, error(permission_error(create,engine,test_engine_1), logtalk(threaded_engine_create(none,true,test_engine_1), _))) :-
		{threaded_engine_create(none, true, test_engine_1),
		 threaded_engine_create(none, true, test_engine_1)}.

	succeeds(threaded_engine_create_3_04) :-
		{threaded_engine_create(X, member(X,[1,2,3]), test_engine_2)}.

	succeeds(threaded_engine_create_3_05) :-
		{threaded_engine_answer(test_engine_2, X)},
		X == 1.

	succeeds(threaded_engine_create_3_06) :-
		{threaded_engine_answer(test_engine_2, X)},
		X == 2.

	succeeds(threaded_engine_create_3_07) :-
		{threaded_engine_answer(test_engine_2, X)},
		X == 3.

	fails(threaded_engine_create_3_08) :-
		{threaded_engine_answer(test_engine_2, _)}.

	succeeds(threaded_engine_create_3_09) :-
		{threaded_engine_stop(test_engine_2)}.

:- end_object.
