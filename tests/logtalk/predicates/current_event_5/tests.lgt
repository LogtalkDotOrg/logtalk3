%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the current_event/5 built-in predicate.'
	]).

	throws(current_event_5_1, error(type_error(event, foo), logtalk(current_event(foo,_,_,_,_), _))) :-
		current_event(foo, _, _, _, _).

	throws(current_event_5_2, error(type_error(object_identifier, 1), logtalk(current_event(_,1,_,_,_), _))) :-
		current_event(_, 1, _, _, _).

	throws(current_event_5_3, error(type_error(callable, 1), logtalk(current_event(_,_,1,_,_), _))) :-
		current_event(_, _, 1, _, _).

	throws(current_event_5_4, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,1,_), _))) :-
		current_event(_, _, _, 1, _).

	throws(current_event_5_5, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,_,1), _))) :-
		current_event(_, _, _, _, 1).

:- end_object.
