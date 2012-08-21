%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(forms).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all forms.']).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print the complete timetable from the pupil viewpoint.']).

	print :-
		nl, write('FORM TIMETABLE...'), nl, nl,
		forall(extends_object(Form, form), Form::print), nl.

:- end_object.


:- object(form).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all forms.']).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print the complete timetable from the pupil viewpoint.']).

	:- public(print_period/1).
	:- info(print_period/1, [
		comment is 'Print the pupil timetable for a specific period.',
		argnames is ['Period']]).

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

