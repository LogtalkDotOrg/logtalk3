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
		date is 2012/11/19,
		comment is 'Unit tests for the threaded_exit/1-2 built-in predicate.'
	]).

	throws(threaded_exit_1_1, error(instantiation_error, logtalk(threaded_exit(_), _))) :-
		{threaded_exit(_)}.

	throws(threaded_exit_1_2, error(type_error(callable, 1), logtalk(threaded_exit(_), _))) :-
		{threaded_exit(1)}.

	throws(threaded_exit_2_1, error(instantiation_error, logtalk(threaded_exit(_,_), _))) :-
		{threaded_exit(_, _)}.

	throws(threaded_exit_2_2, error(type_error(callable, 1), logtalk(threaded_exit(_,_), _))) :-
		{threaded_exit(1, _)}.

	succeeds(threaded_exit_2_3) :-
		{threaded_call(true, Tag),
		 threaded_exit(true, Tag)}.

:- end_object.
