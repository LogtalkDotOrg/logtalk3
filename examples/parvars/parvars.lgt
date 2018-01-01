%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


/*	This example illustrates how to associate a set of predicates with a
	compound term.   Parameters can be accessed from within an object by
	using the execution-context built-in methods this/1 and parameter/2;
	both alternatives are illustrated below.
*/



/*	The first parametric object defines some useful predicates for working
	with lists.
*/


% dealing with non-empty lists is easy;
% note that the lists are compound terms

:- object([_Head_| _Tail_]).

	:- public(last/1).
	:- mode(last(?term), zero_or_one).

	:- public(member/1).
	:- mode(member(?term), zero_or_more).

	:- public(nextto/2).
	:- mode(nextto(?term, ?term), zero_or_more).

	last(Last) :-
		last(_Tail_, _Head_, Last).

	last([], Last, Last).
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

	member(Element) :-
		member(Element, [_Head_| _Tail_]).

	member(Element, [Element| _]).
	member(Element, [_| Tail]) :-
		member(Element, Tail).

	nextto(X, Y) :-
		nextto(X, Y, [_Head_| _Tail_]).

	nextto(X, Y, [X, Y| _]).
	nextto(X, Y, [_| Tail]) :-
		nextto(X, Y, Tail).

:- end_object.


% dealing with empty lists must also be done but it's a bit tricky:

% the empty list is usually an atom, not a compound term,
% so the "extends" relation would be always wrong
:- object('[]',
	extends([[_| _]])).

	% the trick is to redefine all inherited predicates
	% to do the right thing for empty lists
	last(_) :-
		fail.

	member(_) :-
		fail.

	nextto(_, _) :-
		fail.

:- end_object.



/*	The next two parametric objects represent time and date values as
	compound terms using the object's identifiers.
*/


:- object(date(_Year_, _Month_, _Day_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
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

	year(_Year_).

	month(_Month_).

	day(_Day_).

	today :-
		date::today(_Year_, _Month_, _Day_).

	leap_year :-
		(	0 =:= mod(_Year_, 4), 0 =\= mod(_Year_, 100)
		;	0 =:= mod(_Year_, 400)
		),
		!.

:- end_object.


:- object(time(_Hours_, _Mins_, _Secs_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
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

	hours(_Hours_).

	mins(_Mins_).

	secs(_Secs_).

	now :-
		time::now(_Hours_, _Mins_, _Secs_).

:- end_object.



/*	The following parametric object illustrates a solution for implementing
	backtracable object state. The idea is to represent object state by using
	object parameters, defining "setter" predicates/methods that return the
	updated object identifier.
*/

:- object(rectangle(_Width_, _Height_, _X_, _Y_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
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
		Area is _Width_ * _Height_.

	move(X, Y, rectangle(_Width_, _Height_, X, Y)).

	position(_X_, _Y_).

:- end_object.



/*	The following parametric objects show a solution for dealing with inheritance when
	defining "setter" predicates/methods that return updated object identifiers.
*/

:- object(person(_Name_, _Age_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
		comment is 'A simple representation for persons data using parametric objects.',
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
		comment is 'Constructs a new object identifier with an updated age.',
		argnames is ['Age', 'NewAge', 'NewId']
	]).

	age(_Age_, NewAge, person(_Name_, NewAge)).

:- end_object.


:- object(employee(_Name_, _Age_, _Salary_),
	extends(person(_Name_, _Age_))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
		comment is 'A simple representation for employees data using parametric objects.',
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
		comment is 'Constructs a new object identifier with an updated salary.',
		argnames is ['Salary', 'NewSalary', 'NewId']
	]).

	salary(_Salary_, NewSalary, employee(_Name_, _Age_, NewSalary)).

	age(_Age_, NewAge, employee(_Salary_, _Name_, NewAge)).

:- end_object.



/*	The following entities illustrate the use of parametric categories.
*/

:- category(dress(_Season_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
		comment is 'Dress advice according to the season.',
		parnames is ['Season']
	]).

	:- public(clothes/1).

	clothes(Clothes) :-
		clothes(_Season_, Clothes).

	clothes(winter, [pants, sleeves, heavy]).
	clothes(spring, [shorts, sleeves, light]).
	clothes(summer, [shorts, light, white]).
	clothes(autumn, [pants, sleeves, light]).

:- end_category.


:- category(speech(_Event_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
		comment is 'Speech advice according to the event.',
		parnames is ['Event']
	]).

	:- public(speech/1).

	speech(Speech) :-
		speech(_Event_, Speech).

	speech(wedding, [happy, jokes]).
	speech(inauguration, [formal, long]).

:- end_category.


:- object(speech(Season, Event),
	imports((dress(Season), speech(Event)))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/11/16,
		comment is 'Speech and dress advice according to the season and the event.',
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
