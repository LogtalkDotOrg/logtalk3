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
		comment is 'Unit tests for the threaded_engine_stop/1 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_engine_stop_1_1, error(instantiation_error, logtalk(threaded_engine_stop(_), _))) :-
		{threaded_engine_stop(_)}.

	throws(threaded_engine_stop_1_2, error(existence_error(engine,foo), logtalk(threaded_engine_stop(foo), _))) :-
		{threaded_engine_stop(foo)}.

	succeeds(threaded_engine_stop_1_3) :-
		{threaded_engine_create(none, true, test_engine_1),
		 threaded_engine_stop(test_engine_1)}.

:- end_object.
