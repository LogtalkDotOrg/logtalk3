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
		date is 2013/05/04,
		comment is 'Unit tests for the current_object/1 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(current_object_1_1, error(type_error(object_identifier, 1), logtalk(current_object(1), _))) :-
		current_object(1).

	succeeds(current_object_1_2) :-
		current_object(logtalk).

	fails(current_object_1_3) :-
		current_object(non_exisiting_object).

	% built-in entitiy tests

	succeeds(current_object_1_4) :-
		current_object(user),
		object_property(user, built_in),
		object_property(user, static),
		(	current_logtalk_flag(threads, supported) ->
			object_property(user, threaded)
		;	true
		).

	succeeds(current_object_1_5) :-
		current_object(logtalk),
		object_property(logtalk, built_in),
		object_property(logtalk, static),
		(	current_logtalk_flag(threads, supported) ->
			object_property(logtalk, threaded)
		;	true
		).

:- end_object.
