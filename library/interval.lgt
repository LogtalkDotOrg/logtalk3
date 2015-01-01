%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(interval,
	implements(intervalp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2014/04/26,
		comment is 'Basic temporal interval relations. An interval is represented by a compound term, i/2, with two ground arguments, the start and end points.'
	]).

	new(Start, End, Interval) :-
		ground(Start),
		ground(End),
		Start @< End,
		Interval = i(Start, End).

	valid(i(Start, End)) :-
		ground(Start),
		ground(End),
		Start @< End.

	before(i(_, End1), i(Start2, _)) :-
		End1 @< Start2.

	after(Interval1, Interval2) :-
		before(Interval2, Interval1).

	meets(i(_, Point), i(Point, _)).

	met_by(Interval1, Interval2) :-
		meets(Interval2, Interval1).

	overlaps(i(Start1, End1), i(Start2, End2)) :-
		Start1 @< Start2,
		End1 @< End2.

	overlapped_by(Interval1, Interval2) :-
		overlaps(Interval2, Interval1).

	starts(i(Start, End1), i(Start, End2)) :-
		End1 @< End2.

	started_by(Interval1, Interval2) :-
		starts(Interval2, Interval1).

	during(i(Start1, End1), i(Start2, End2)) :-
		Start2 @< Start1,
		End1 @< End2.

	contains(Interval1, Interval2) :-
		during(Interval2, Interval1).

	finishes(i(Start1, End), i(Start2, End)) :-
		Start2 @< Start1.

	finished_by(Interval1, Interval2) :-
		finishes(Interval2, Interval1).

	equal(Interval, Interval).

	:- alias(intervalp, [
		before/2 as        b/2,
		after/2 as         bi/2,
		meets/2 as         m/2,
		met_by/2 as        mi/2,
		overlaps/2 as      o/2,
		overlapped_by/2 as oi/2,
		starts/2 as        s/2,
		started_by/2 as    si/2,
		during/2 as        d/2,
		contains/2 as      di/2,
		contains/2 as      f/2,
		finished_by/2 as   fi/2,
		equal/2 as         eq/2
	]).

	b(Interval1, Interval2) :-
		before(Interval1, Interval2).

	bi(Interval1, Interval2) :-
		after(Interval1, Interval2).

	m(Interval1, Interval2) :-
		meets(Interval1, Interval2).

	mi(Interval1, Interval2) :-
		met_by(Interval1, Interval2).

	o(Interval1, Interval2) :-
		overlaps(Interval1, Interval2).

	oi(Interval1, Interval2) :-
		overlapped_by(Interval1, Interval2).

	s(Interval1, Interval2) :-
		starts(Interval1, Interval2).

	si(Interval1, Interval2) :-
		started_by(Interval1, Interval2).

	d(Interval1, Interval2) :-
		during(Interval1, Interval2).

	di(Interval1, Interval2) :-
		contains(Interval1, Interval2).

	f(Interval1, Interval2) :-
		contains(Interval1, Interval2).

	fi(Interval1, Interval2) :-
		finished_by(Interval1, Interval2).

	eq(Interval1, Interval2) :-
		equal(Interval1, Interval2).

:- end_object.
