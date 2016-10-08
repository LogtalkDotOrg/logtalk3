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


:- object(forms).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all forms.'
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print the complete timetable from the pupil viewpoint.'
	]).

	print :-
		nl, write('FORM TIMETABLE...'), nl, nl,
		forall(extends_object(Form, form), Form::print), nl.

:- end_object.


:- object(form).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all forms.'
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print the complete timetable from the pupil viewpoint.'
	]).

	:- public(print_period/1).
	:- info(print_period/1, [
		comment is 'Print the pupil timetable for a specific period.',
		argnames is ['Period']
	]).

	print :-
		self(Self),
		write('FORM: '), write(Self), nl,
		forall(extends_object(Period, period), Period::print_form(Self)), nl.

	print_period(Period) :-
		self(Self),
		timetable::filled_entry(Self, Period, Teacher, Subject),
		!,
		write(Self), write(': '),
		write(Teacher), write(' teaches '),
		write(Subject), nl.

	print_period(_) :-
		self(Self),
		write(Self), write(': '), nl.

:- end_object.


:- object(first_year,
	extends(form)).

:- end_object.


:- object(second_year,
	extends(form)).

:- end_object.


:- object(third_year,
	extends(form)).

:- end_object.


:- object(fourth_year,
	extends(form)).

:- end_object.

