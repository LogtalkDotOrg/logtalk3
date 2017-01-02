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
