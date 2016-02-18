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


:- object(timetable).


	:- info([
		version is 1.02,
		date is 2013/04/23,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'Set up & create a timetable satisfying all of the constraints.'
	]).


	:- public(setup/0).
	:- info(setup/0, [
		comment is 'Set up the teachers, subjects, forms & periods for this school.'
	]).

	:- public(make/0).
	:- info(make/0, [
		comment is 'Make the timetable according to the school setup.'
	]).

	:- public(make/1).
	:- info(make/1, [
		comment is 'Make with max. depth of swaps.',
		argnames is ['Effort']
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print from different perspectives.'
	]).

	:- public(filled_entry/4).
	:- info(filled_entry/4, [
		comment is 'Timetable entry.',
		argnames is ['Form', 'Period', 'Teacher', 'Subject']
	]).

	:- private(entry/4).
	:- dynamic(entry/4).
	:- info(entry/4, [
		comment is 'Timetable entry.',
		argnames is ['Form', 'Period', 'Teacher', 'Subject']
	]).


	:- uses(list, [length/2]).


	print :-
		forms::print,
		periods::print,
		teachers::print,
		subjects::print.


	setup :-
		retractall(entry(_, _, _, _)).


	make :-
		make(3).


	make(Effort) :-
		length(E, Effort),
		forall(
			(extends_object(Form, form), extends_object(Period, period)),
			fill_entry(E, Form, Period, _Teacher, _Subject)).


	unfilled_entry(Form, Period) :-
		extends_object(Form, form),
		extends_object(Period, period),
		\+ filled_entry(Form, Period, _, _).


	filled_entry(Form, Period, Teacher, Subject) :-
		entry(Form, Period, Teacher, Subject).


	fill_entry(E, Form, Period, Teacher, Subject) :-
		find_entry(E, Form, Period, Teacher, Subject),
		!,
		assert(Form, Period, Teacher, Subject).

	fill_entry(_, _, _, _, _).


	find_entry(_, Form, Period, Teacher, Subject) :-
		extends_object(Teacher, teacher),
		Teacher::teach_period(Period),
		\+ filled_entry(_, Period, Teacher, _),
		extends_object(Subject, subject),
		Teacher::teach_subject(Subject),
		\+ filled_entry(Form, _, _, Subject).

	find_entry([_| E], FormA, Period, TeacherA, SubjectA) :-
		extends_object(Teacher, teacher),
		Teacher::teach_period(Period),
		filled_entry(FormB, Period, TeacherA, _),
		extends_object(SubjectA, subject),
		TeacherA::teach_subject(SubjectA),
		\+ filled_entry(FormA, _, _, SubjectA),
		find_entry(E, FormB, Period, TeacherB, SubjectB),
		TeacherB \= TeacherA,
		write('Swap teacher... '), nl,
		retract(FormB, Period, TeacherA, _),
		assert(FormB, Period, TeacherB, SubjectB).

	find_entry([_| E], Form, PeriodA, TeacherA, SubjectA) :-
		extends_object(TeacherA, teacher),
		TeacherA::teach_period(PeriodA),
		\+ filled_entry(_, PeriodA, TeacherA, _),
		extends_object(SubjectA, subject),
		TeacherA::teach_subject(SubjectA),
		filled_entry(Form, PeriodB, _, SubjectA),
		find_entry(E, Form, PeriodB, TeacherB, SubjectB),
		SubjectA \= SubjectB,
		write('Swap subject... '), nl,
		retract(Form, PeriodB, _, SubjectA),
		assert(Form, PeriodB, TeacherB, SubjectB).


	assert(Form, Period, Teacher, Subject) :-
		assertz(entry(Form, Period, Teacher, Subject)),
		write('+ '),
		write(Form), write(' - '),
		write(Period), write(' - '),
		write(Teacher), write(' - '),
		write(Subject), nl.


	retract(Form, Period, Teacher, Subject) :-
		retract(entry(Form, Period, Teacher, Subject)),
		write('- '),
		write(Form), write(' - '),
		write(Period), write(' - '),
		write(Teacher), write(' - '),
		write(Subject), nl.


:- end_object.
