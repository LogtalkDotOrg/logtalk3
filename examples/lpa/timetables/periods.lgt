%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(periods).


	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all periods.'
	]).


	:- public(print/0).
	:- info(print/0, [
		comment is 'Print period timetable.'
	]).


	print :-
		nl, write('PERIOD TIMETABLE ...'), nl, nl,
		forall(extends_object(Period, period), Period::print), nl.


:- end_object.


:- object(period).


	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all periods.'
	]).


	:- public(print/0).
	:- info(print/0, [
		comment is 'Print complete timetable from the period viewpoint.'
	]).

	:- public(print_teacher/1).
	:- info(print_teacher/1, [
		comment is 'Print entry for a specific teacher in this period.',
		argnames is ['Teacher']
	]).

	:- public(print_form/1).
	:- info(print_form/1, [
		comment is 'Print entry for a specific form in this period.',
		argnames is ['Form']
	]).

	:- public(print_subject/1).
	:- info(print_subject/1, [
		comment is 'Print entry for a specific subject in this period.',
		argnames is ['Subject']
	]).



	print :-
		self(Self),
		write('PERIOD: '), write(Self), nl,
		forall(extends_object(Form, form), Form::print_period(Self)), nl.


	print_teacher(Teacher) :-
		self(Self),
		timetable::filled_entry(Form, Self, Teacher, Subject),
		!,
		write(Self), write(': teach '),
		write(Subject), write(' to '),
		write(Form), nl. 

	print_teacher(_) :-
		self(Self),
		write(Self), write(':'), nl.


	print_form(Form) :-
		self(Self),
		timetable::filled_entry(Form, Self, Teacher, Subject),
		!,
		write(Self), write(': '),
		write(Teacher), write(' teaches '),
		write(Subject), nl. 

	print_form(_) :-
		self(Self),
		write(Self), write(':'), nl.


	print_subject(Subject) :-
		self(Self),
		timetable::filled_entry(Form, Self, Teacher, Subject),
		write(Self), write(': '),
		write(Form), write(' taught by '),
		write(Teacher), nl,
		fail. 

	print_subject(_).


:- end_object.


:- object(p1,
	extends(period)).

:- end_object.


:- object(p2,
	extends(period)).

:- end_object.


:- object(p3,
	extends(period)).

:- end_object.


:- object(p4,
	extends(period)).

:- end_object.


:- object(p5,
	extends(period)).

:- end_object.
