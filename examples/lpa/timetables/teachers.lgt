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


:- object(teachers).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all teachers.'
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print teachers timetable.'
	]).

	print :-
		nl, write('TEACHER TIMETABLE ...'), nl, nl,
		forall(extends_object(Teacher, teacher), Teacher::print),
		nl.

:- end_object.


:- object(teacher).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all teachers.'
	]).

	:- public(teach_period/1).
	:- info(teach_period/1, [
		comment is 'A period for which the teacher can be assigned.'
	]).

	:- public(teach_subject/1).
	:- info(teach_subject/1, [
		comment is 'A subject which the teacher can teach.'
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print complete timetable from the teacher viewpoint.'
	]).

	:- public(freetime/1).
	:- info(freetime/1, [
		comment is '.',
		argnames is ['Freetime']
	]).

	:- public(subject/1).
	:- info(subject/1, [
		comment is '.',
		argnames is ['Subject']
	]).

	teach_period(Period) :-
		\+ ::freetime(Period).

	teach_subject(Subject) :-
		::subject(Subject).

	print :-
		self(Self),
		write('TEACHER: '), write(Self), nl,
		forall(extends_object(Period, period), Period::print_teacher(Self)),
		nl.

:- end_object.


:- object(nicky,
	extends(teacher)).

	subject(french).
	subject(biology).

	freetime(1).
	freetime(4).

:- end_object.


:- object(brian,
	extends(teacher)).

	subject(maths).
	subject(music).

:- end_object.


:- object(dave,
	extends(teacher)).

	subject(maths).

:- end_object.


:- object(clive,
	extends(teacher)).

	subject(french).
	subject(prolog).

	freetime(2).
	freetime(3).
	freetime(5).

:- end_object.


:- object(diane,
	extends(teacher)).

	subject(accountancy).

	freetime(2).
	freetime(4).

:- end_object.


:- object(phil,
	extends(teacher)).

	subject(maths).
	subject('prolog++').

	freetime(3).

:- end_object.
