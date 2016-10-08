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



:- category(monitor,
	implements(monitorp)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2009/10/8,
		comment is 'Monitor predicates.'
	]).

	:- private(spy_point_/4).
	:- dynamic(spy_point_/4).
	:- mode(spy_point_(?event, ?object, ?callable, ?object), zero_or_more).
	:- info(spy_point_/4, [
		comment is 'Stores current spy points.',
		argnames is ['Event', 'Object', 'Message', 'Sender']
	]).

	monitor_activated :-
		self(Self),
		current_event(_, _, _, _, Self) ->
		true.

	activate_monitor :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self),
		forall(
			::spy_point_(Event, Object, Message, Sender),
			define_events(Event, Object, Message, Sender, Self)).

	suspend_monitor :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self).

	reset_monitor :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self),
		::retractall(spy_point_(_, _, _, _)).

	spy_point(Event, Object, Message, Sender) :-
		::spy_point_(Event, Object, Message, Sender).

	set_spy_point(Event, Object, Message, Sender) :-
		::retractall(spy_point_(Event, Object, Message, Sender)),
		once((var(Event); Event = before; Event = after)),
		::assertz(spy_point_(Event, Object, Message, Sender)).

	del_spy_points(Event, Object, Message, Sender) :-
		::retractall(spy_point_(Event, Object, Message, Sender)).

:- end_category.
