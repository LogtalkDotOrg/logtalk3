%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(person).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-10-12,
		comment is 'Generic person.'
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_one).
	:- info(name/1, [
		comment is 'Person name.',
		argnames is ['Name']
	]).

	:- public(birth/1).
	:- mode(birth(?atom), zero_or_one).
	:- info(birth/1, [
		comment is 'Person birth data.',
		argnames is ['Year/Month/Day']
	]).

	:- public(new/3).
	:- mode(new(-object_identifier, +atom, +atom), one).
	:- info(new/3, [
		comment is 'Creates a new person.',
		argnames is ['Id', 'Name', 'Birth']
	]).

	new(Person, Name, Birth) :-
		self(Self),
		create_object(Person, [extends(Self)], [], [name(Name), birth(Birth)]).

	/* an alternative but equivalent constructor could be:
	new(Person, Name, Birth) :-
		self(Self),
		create_object(Person, [extends(Self)], [], []),
		Person::assertz(name(Name)),
		Person::assertz(birth(Birth)).
	*/

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0, [
		comment is 'Prints a person description.'
	]).

	print :-
		::name(Name),   write('Name:   '), write(Name), nl,
		::birth(Birth), write('Birth:  '), write(Birth), nl.

:- end_object.


:- object(teacher,
	extends(person)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-10-12,
		comment is 'Teacher person.'
	]).

	:- public(office/1).
	:- mode(office(?atom), zero_or_one).
	:- info(office/1, [
		comment is 'Person office.',
		argnames is ['Office']
	]).

	:- public(new/4).
	:- mode(new(-object_identifier, +atom, +atom, +atom), one).
	:- info(new/4, [
		comment is 'Creates a new teacher.',
		argnames is ['Id', 'Name', 'Birth', 'Office']
	]).

	new(Person, Name, Birth, Office) :-
		% create a "generic" person and ...
		::new(Person, Name, Birth),
		% ... add "teacher" specific data
		Person::assertz(office(Office)).

	print :-
		^^print,
		::office(Office), write('Office: '), write(Office), nl.

:- end_object.


:- object(student,
	extends(person)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-10-12,
		comment is 'Student person.'
	]).

	:- public(dorm/1).
	:- mode(dorm(?atom), zero_or_one).
	:- info(dorm/1, [
		comment is 'Student dorm.',
		argnames is ['Dorm']
	]).

	:- public(new/4).
	:- mode(new(-object_identifier, +atom, +atom, +atom), one).
	:- info(new/4, [
		comment is 'Creates a new student.',
		argnames is ['Id', 'Name', 'Birth', 'Dorm']
	]).

	new(Person, Name, Birth, Dorm) :-
		% create a "generic" person and ...
		::new(Person, Name, Birth),
		% ... add "student" specific data
		Person::assertz(dorm(Dorm)).

	print :-
		^^print,
		::dorm(Dorm), write('Dorm:   '), write(Dorm), nl.

:- end_object.


% some parametric objects for working with object proxies:

:- object(person(_Name, _Birth),
	extends(person)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-10-12,
		comment is 'Person as a parametric object.'
	]).

	name(Name) :-
		parameter(1, Name).

	birth(Birth) :-
		parameter(2, Birth).

:- end_object.


:- object(teacher(_Name, _Birth, _Office),
	extends(teacher)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-10-12,
		comment is 'Teacher as a parametric object.'
	]).

	name(Name) :-
		parameter(1, Name).

	birth(Birth) :-
		parameter(2, Birth).

	office(Office) :-
		parameter(3, Office).

:- end_object.


:- object(student(_Name, _Birth, _Dorm),
	extends(student)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2008-10-12,
		comment is 'Student as a parametric object.'
	]).

	name(Name) :-
		parameter(1, Name).

	birth(Birth) :-
		parameter(2, Birth).

	dorm(Dorm) :-
		parameter(3, Dorm).

:- end_object.


% some object proxies:

person('Oscar the Grouch', '1969/11/10').
person('Cookie Monster', '1969/12/02').

teacher('Gordon Robinson', '1969/11/10', '3.2').

student('Roosevelt Franklin', '1969/11/10', 'Blue').
