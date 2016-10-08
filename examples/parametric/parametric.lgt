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


/*	This example illustrates how to associate a set of predicates with a
	compound term.   Parameters can be accessed from within an object by
	using the execution-context built-in methods this/1 and parameter/2;
	both alternatives are illustrated below.
*/



/*	The first parametric object defines some useful predicates for working
	with lists.
*/


% dealing with non-empty lists is easy:

:- object([_| _]).			% note that the lists are compound terms

	:- public(last/1).
	:- mode(last(?term), zero_or_one).

	:- public(member/1).
	:- mode(member(?term), zero_or_more).

	:- public(nextto/2).
	:- mode(nextto(?term, ?term), zero_or_more).

	last(Last) :-
		this([Head| Tail]),
		last(Tail, Head, Last).

	last([], Last, Last).
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

	member(Element) :-
		this(List),
		member(Element, List).

	member(Element, [Element| _]).
	member(Element, [_| Tail]) :-
		member(Element, Tail).

	nextto(X, Y) :-
		this([Head| Tail]),
		nextto(X, Y, [Head| Tail]).

	nextto(X, Y, [X, Y| _]).
	nextto(X, Y, [_| Tail]) :-
		nextto(X, Y, Tail).

:- end_object.


% dealing with empty lists must also be done but it's a bit tricky:

:- object('[]',				% the empty list is usually an atom, not a compound term,
	extends([[_| _]])).		% so the "extends" relation would be always wrong

	last(_) :-				% the trick is to redefine all inherited predicates
		fail.				% to do the right thing for empty lists

	member(_) :-
		fail.

	nextto(_, _) :-
		fail.

:- end_object.



/*	The next two parametric objects represent time and date values as
	compound terms using the object's identifiers.
*/


:- object(date(_Year, _Month, _Day)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/9/5,
		comment is 'Dates as parametric objects.',
		parnames is ['Year', 'Month', 'Day']
	]).

	:- public(year/1).
	:- mode(year(?integer), one).

	:- public(month/1).
	:- mode(month(?integer), one).

	:- public(day/1).
	:- mode(day(?integer), one).

	:- public(today/0).
	:- mode(today, one).

	:- public(leap_year/0).
	:- mode(leap_year, zero_or_one).

	year(Year) :-
		parameter(1, Year).

	month(Month) :-
		parameter(2, Month).

	day(Day) :-
		parameter(3, Day).

	today :-
		date::today(Year, Month, Day),
		parameter(1, Year),
		parameter(2, Month),
		parameter(3, Day).

/*	Alternative predicate definitions using this/1 instead of parameter/2
	(see the User Manual for the pros and cons of both alternatives):

	year(Year) :-
		this(date(Year, _, _)).

	month(Month) :-
		this(date(_, Month, _)).

	day(Day) :-
		this(date(_, _, Day)).

	today :-
		date::today(Year, Month, Day),
		this(date(Year, Month, Day)).

*/

	leap_year :-
		parameter(1, Year),
		(	0 =:= mod(Year, 4), 0 =\= mod(Year, 100)
		;	0 =:= mod(Year, 400)
		),
		!.

:- end_object.


:- object(time(_Hours, _Mins, _Secs)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/9/5,
		comment is 'Time as parametric objects.',
		parnames is ['Hours', 'Mins', 'Secs']
	]).

	:- public(hours/1).
	:- mode(hours(?integer), one).

	:- public(mins/1).
	:- mode(mins(?integer), one).

	:- public(secs/1).
	:- mode(secs(?integer), one).

	:- public(now/0).
	:- mode(now, one).

	hours(Hours) :-
		parameter(1, Hours).

	mins(Mins) :-
		parameter(2, Mins).

	secs(Secs) :-
		parameter(3, Secs).

	now :-
		time::now(Hours, Mins, Secs),
		parameter(1, Hours),
		parameter(2, Mins),
		parameter(3, Secs).

/*	Alternative predicate definitions using this/1 instead of parameter/2
	(see the User Manual for the pros and cons of both alternatives):

	hours(Hours) :-
		this(time(Hours, _, _)).

	mins(Mins) :-
		this(time(_, Mins, _)).

	secs(Secs) :-
		this(time(_, _, Secs)).

	now :-
		time::now(Hours, Mins, Secs),
		this(time(Hours, Mins, Secs)).

*/

:- end_object.



/*	The following parametric object illustrates a solution for implementing
	backtracable object state. The idea is to represent object state by using
	object parameters, defining "setter" predicates/methods that return the
	updated object identifier.
*/

:- object(rectangle(_Width, _Height, _X, _Y)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2005/9/5,
		comment is 'A simple implementation of a geometric rectangle using parametric objects.',
		parnames is ['Width', 'Height', 'X', 'Y']
	]).

	:- public(init/0).
	:- mode(init, one).
	:- info(init/0, [
		comment is 'Initialize rectangle position.'
	]).

	:- public(area/1).
	:- mode(area(-integer), one).
	:- info(area/1, [
		comment is 'Rectangle area.',
		argnames is ['Area']
	]).

	:- public(move/3).
	:- mode(move(+integer, +integer, -compound), one).
	:- info(move/3, [
		comment is 'Moves a rectangle to a new position, returning the updated rectangle.',
		argnames is ['X', 'Y', 'NewRectangle']
	]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Rectangle current position.',
		argnames is ['X', 'Y']
	]).

	init :-
		parameter(1, 2),	% Width
		parameter(2, 1),	% Height
		parameter(3, 0),	% X
		parameter(4, 0).	% Y

	area(Area) :-
		parameter(1, Width),
		parameter(2, Height),
		Area is Width*Height.

	move(X, Y, rectangle(Width, Height, X, Y)) :-
		parameter(1, Width),
		parameter(2, Height).

	position(X, Y) :-
		parameter(3, X),
		parameter(4, Y).

/*	Alternative predicate definitions using this/1 instead of parameter/2
	(see the User Manual for the pros and cons of both alternatives):

	init :-
		this(rectangle(2, 1, 0, 0)).

	area(Area) :-
		this(rectangle(Width, Height, _, _)),
		Area is Width*Height.

	move(X, Y, rectangle(Width, Height, X, Y)) :-
		this(rectangle(Width, Height, _, _)).

	position(X, Y) :-
		this(rectangle(_, _, X, Y)).

*/

:- end_object.



/*	The following parametric objects show a solution for dealing with inheritance when
	defining "setter" predicates/methods that return updated object identifiers.
*/

:- object(person(_Name, _Age)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/6/19,
		comment is 'A simple representation for people using parametric objects.',
		parnames is ['Name', 'Age']
	]).

	:- public(grow_older/1).
	:- mode(grow_older(-object_identifier), one).
	:- info(grow_older/1, [
		comment is 'Increments the person''s age, returning the updated object identifier.',
		argnames is ['NewId']
	]).

	grow_older(NewId) :-
		::age(OldAge, NewAge, NewId),
		NewAge is OldAge + 1.

	:- protected(age/3).
	:- mode(age(?integer, ?integer, -object_identifier), zero_or_one).
	:- info(age/3, [
		comment is 'Rectangle area.',
		argnames is ['OldAge', 'NewAge', 'NewId']
	]).

	age(OldAge, NewAge, person(Name, NewAge)) :-	% this rule is compiled into a fact due to
		this(person(Name, OldAge)).					% compilation of the this/1 call inline

:- end_object.


:- object(employee(Name, Age, _Salary),
	extends(person(Name, Age))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/6/19,
		comment is 'A simple representation for employees using parametric objects.',
		parnames is ['Name', 'Age', 'Salary']
	]).

	:- public(give_raise/2).
	:- mode(give_raise(+integer, -object_identifier), one).
	:- info(give_raise/2, [
		comment is 'Gives a raise to the employee, returning the updated object identifier.',
		argnames is ['Amount', 'NewId']
	]).

	give_raise(Amount, NewId) :-
		::salary(OldSalary, NewSalary, NewId),
		NewSalary is OldSalary + Amount.

	:- protected(salary/3).
	:- mode(salary(?integer, ?integer, -object_identifier), zero_or_one).
	:- info(salary/3, [
		comment is 'Rectangle area.',
		argnames is ['OldSalary', 'NewSalary', 'NewId']
	]).

	salary(OldSalary, NewSalary, employee(Name, Age, NewSalary)) :-
		this(employee(Name, Age, OldSalary)).

	age(OldAge, NewAge, employee(Salary, Name, NewAge)) :-
		this(employee(Salary, Name, OldAge)).

:- end_object.



/*	The following entities illustrate the use of parametric categories.
*/

:- category(dress(_Season)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/02/17,
		comment is 'Description',
		parnames is ['Season']
	]).

	:- public(clothes/1).

	clothes(Clothes) :-
		parameter(1, Season),
		clothes(Season, Clothes).

	clothes(winter, [pants, sleeves, heavy]).
	clothes(spring, [shorts, sleeves, light]).
	clothes(summer, [shorts, light, white]).
	clothes(autumn, [pants, sleeves, light]).

:- end_category.


:- category(speech(_Event)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/02/17,
		comment is 'Description',
		parnames is ['Event']
	]).

	:- public(speech/1).

	speech(Speech) :-
		parameter(1, Event),
		speech(Event, Speech).

	speech(wedding, [happy, jokes]).
	speech(inauguration, [formal, long]).

:- end_category.


:- object(speech(Season, Event),
	imports((dress(Season), speech(Event)))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/08/14,
		comment is 'Description',
		parnames is ['Season', 'Event']
	]).

	:- public(advice/0).
	advice :-
		^^clothes(Clothes),
		write('Clothes: '), write(Clothes), nl,
		^^speech(Speech),
		write('Speech:  '), write(Speech), nl, nl.

	:- public(advice/2).
	advice(Clothes, Speech) :-
		^^clothes(Clothes),
		^^speech(Speech).

:- end_object.
