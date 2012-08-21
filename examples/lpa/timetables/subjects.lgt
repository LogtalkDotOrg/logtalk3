%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(subjects).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all subjects.']).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print complete timetable from the subject viewpoint.']).

	print :-
		nl, write('SUBJECT TIMETABLE ...'), nl, nl,
		forall(extends_object(Subject, subject), Subject::print),
		nl.

:- end_object.


:- object(subject).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all subjects.']).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print complete timetable from the subject viewpoint.']).

	print :-
		self(Self),
		write('SUBJECT: '), write(Self), nl,
		forall(extends_object(Period, period), Period::print_subject(Self)),
		nl.

:- end_object.


:- object(maths,
	extends(subject)).

:- end_object.


:- object(music,
	extends(subject)).

:- end_object.


:- object(french,
	extends(subject)).

:- end_object.


:- object(prolog,
	extends(subject)).

:- end_object.


:- object(biology,
	extends(subject)).

:- end_object.


:- object('prolog++',
	extends(subject)).

:- end_object.


:- object(accountancy,
	extends(subject)).

:- end_object.
