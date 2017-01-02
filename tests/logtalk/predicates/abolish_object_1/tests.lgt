%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		date is 2012/12/12,
		comment is 'Unit tests for the abolish_object/1 built-in predicate.'
	]).

	throws(abolish_object_1_1, error(instantiation_error, logtalk(abolish_object(_), _))) :-
		abolish_object(_).

	throws(abolish_object_1_2, error(type_error(object_identifier, 1), logtalk(abolish_object(1), _))) :-
		abolish_object(1).

	throws(abolish_object_1_3, error(existence_error(object, non_exisiting_object), logtalk(abolish_object(non_exisiting_object), _))) :-
		abolish_object(non_exisiting_object).

	throws(abolish_object_1_4, error(permission_error(modify, static_object, logtalk), logtalk(abolish_object(logtalk), _))) :-
		abolish_object(logtalk).

	succeeds(abolish_object_1_5) :-
		create_object(Object, [], [], []),
		current_object(Object),
		abolish_object(Object),
		\+ current_object(Object).

	succeeds(abolish_object_1_6) :-
		create_object(a_object, [], [], []),
		current_object(a_object),
		abolish_object(a_object),
		\+ current_object(a_object).

:- end_object.
