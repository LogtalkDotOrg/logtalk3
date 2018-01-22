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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the threaded/1 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_1_1, error(instantiation_error, logtalk(threaded(_), _))) :-
		{threaded(_)}.

	throws(threaded_1_2, error(type_error(callable, 1), logtalk(threaded(_), _))) :-
		{threaded(1)}.

	succeeds(threaded_1_3) :-
		{threaded((true, true))}.

	succeeds(threaded_1_4) :-
		{threaded((fail; true))}.

	fails(threaded_1_5) :-
		{threaded((true, fail))}.

	fails(threaded_1_6) :-
		{threaded((fail; fail))}.

:- end_object.
