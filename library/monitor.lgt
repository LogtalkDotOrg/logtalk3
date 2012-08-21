%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- category(monitor,
	implements(monitorp)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2009/10/8,
		comment is 'Monitor predicates.']).

	:- private(spy_point_/4).
	:- dynamic(spy_point_/4).
	:- mode(spy_point_(?event, ?object, ?callable, ?object), zero_or_more).
	:- info(spy_point_/4, [
		comment is 'Stores current spy points.',
		argnames is ['Event', 'Object', 'Message', 'Sender']]).

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
