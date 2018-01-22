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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2017/06/19,
		comment is 'Unit tests for the current_event/5 built-in predicate.'
	]).

	throws(current_event_5_01, error(type_error(event, foo), logtalk(current_event(foo,_,_,_,_), _))) :-
		current_event(foo, _, _, _, _).

	throws(current_event_5_02, error(type_error(object_identifier, 1), logtalk(current_event(_,1,_,_,_), _))) :-
		current_event(_, 1, _, _, _).

	throws(current_event_5_03, error(type_error(callable, 1), logtalk(current_event(_,_,1,_,_), _))) :-
		current_event(_, _, 1, _, _).

	throws(current_event_5_04, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,1,_), _))) :-
		current_event(_, _, _, 1, _).

	throws(current_event_5_05, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,_,1), _))) :-
		current_event(_, _, _, _, 1).

	succeeds(current_event_5_06) :-
		create_object(Monitor, [implements(monitoring)], [], [before(_,_,_)]),
		define_events(before, foo, bar, baz, Monitor),
		findall(
			current_event(Event, Object, Message, Sender, Monitor),
			current_event(Event, Object, Message, Sender, Monitor),
			Events
		),
		Events == [current_event(before, foo, bar, baz, Monitor)].

	succeeds(current_event_5_07) :-
		create_object(Monitor, [implements(monitoring)], [], [after(_,_,_)]),
		define_events(after, foo, bar, baz, Monitor),
		findall(
			current_event(Event, Object, Message, Sender, Monitor),
			current_event(Event, Object, Message, Sender, Monitor),
			Events
		),
		Events == [current_event(after, foo, bar, baz, Monitor)].

	succeeds(current_event_5_08) :-
		create_object(Monitor, [implements(monitoring)], [], [before(_,_,_), after(_,_,_)]),
		define_events(_, foo, bar, baz, Monitor),
		findall(
			current_event(Event, Object, Message, Sender, Monitor),
			current_event(Event, Object, Message, Sender, Monitor),
			Events
		),
		sort(Events, SortedEvents),
		SortedEvents == [
			current_event(after,  foo, bar, baz, Monitor),
			current_event(before, foo, bar, baz, Monitor)
		].

:- end_object.
