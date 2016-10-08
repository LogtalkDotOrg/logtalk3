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
		date is 2012/12/12,
		comment is 'Unit tests for the abolish_events/5 built-in predicate.'
	]).

	throws(abolish_events_5_1, error(type_error(event, foo), logtalk(abolish_events(foo,_,_,_,_), _))) :-
		abolish_events(foo, _, _, _, _).

	throws(abolish_events_5_2, error(type_error(object_identifier, 1), logtalk(abolish_events(_,1,_,_,_), _))) :-
		abolish_events(_, 1, _, _, _).

	throws(abolish_events_5_3, error(type_error(callable, 1), logtalk(abolish_events(_,_,1,_,_), _))) :-
		abolish_events(_, _, 1, _, _).

	throws(abolish_events_5_4, error(type_error(object_identifier, 1), logtalk(abolish_events(_,_,_,1,_), _))) :-
		abolish_events(_, _, _, 1, _).

	throws(abolish_events_5_5, error(type_error(object_identifier, 1), logtalk(abolish_events(_,_,_,_,1), _))) :-
		abolish_events(_, _, _, _, 1).

	succeeds(abolish_events_5_6) :-
		create_object(Monitor, [implements(monitoring)], [], [before(_,_,_), after(_,_,_)]),
		define_events(_, _, _, _ , Monitor),
		current_event(_, _, _, _, Monitor),
		abolish_events(_, _, _, _, Monitor),
		\+ current_event(_, _, _, _, Monitor),
		abolish_object(Monitor).

:- end_object.
