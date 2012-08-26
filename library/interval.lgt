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
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(interval,
	implements(intervalp)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/01/06,
		comment is 'Basic temporal interval relations. An interval is represented by a list with two ground terms, the start and end points.']).

	valid([Start, End]) :-
		ground(Start),
		ground(End),
		Start @< End.

	before([_, End1], [Start2, _]) :-
		End1 @< Start2.

	after(Interval1, Interval2) :-
        before(Interval2, Interval1).

	meets([_, Point], [Point, _]).

	met_by(Interval1, Interval2) :-
		meets(Interval2, Interval1).

	overlaps([Start1, End1], [Start2, End2]) :-
		Start1 @< Start2,
		End1 @< End2.

	overlapped_by(Interval1, Interval2) :-
		overlaps(Interval2, Interval1).

	starts([Start, End1], [Start, End2]) :-
		End1 @< End2.

	started_by(Interval1, Interval2) :-
		starts(Interval2, Interval1).

	during([Start1, End1], [Start2, End2]) :-
		Start2 @< Start1,
		End1 @< End2.

	contains(Interval1, Interval2) :-
		during(Interval2, Interval1).

	finishes([Start1, End], [Start2, End]) :-
		Start2 @< Start1.

	finished_by(Interval1, Interval2) :-
		finishes(Interval2, Interval1).

	equal(Interval, Interval).

	:- alias(intervalp, before/2,        b/2).
	b(Interval1, Interval2) :- before(Interval1, Interval2).

	:- alias(intervalp, after/2,         bi/2).
	bi(Interval1, Interval2) :- after(Interval1, Interval2).

	:- alias(intervalp, meets/2,         m/2).
	m(Interval1, Interval2) :- meets(Interval1, Interval2).

	:- alias(intervalp, met_by/2,        mi/2).
	mi(Interval1, Interval2) :- met_by(Interval1, Interval2).

	:- alias(intervalp, overlaps/2,      o/2).
	o(Interval1, Interval2) :- overlaps(Interval1, Interval2).

	:- alias(intervalp, overlapped_by/2, oi/2).
	oi(Interval1, Interval2) :- overlapped_by(Interval1, Interval2).

	:- alias(intervalp, starts/2,        s/2).
	s(Interval1, Interval2) :- starts(Interval1, Interval2).

	:- alias(intervalp, started_by/2,    si/2).
	si(Interval1, Interval2) :- started_by(Interval1, Interval2).

	:- alias(intervalp, during/2,        d/2).
	d(Interval1, Interval2) :- during(Interval1, Interval2).

	:- alias(intervalp, contains/2,      di/2).
	di(Interval1, Interval2) :- contains(Interval1, Interval2).

	:- alias(intervalp, contains/2,      f/2).
	f(Interval1, Interval2) :- contains(Interval1, Interval2).

	:- alias(intervalp, finished_by/2,   fi/2).
	fi(Interval1, Interval2) :- finished_by(Interval1, Interval2).

	:- alias(intervalp, equal/2,         eq/2).
	eq(Interval1, Interval2) :- equal(Interval1, Interval2).

:- end_object.
