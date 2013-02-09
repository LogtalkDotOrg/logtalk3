%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/25,
		comment is 'Unit tests for the "threads/blackboard" example.'
	]).

	:- threaded.

	unit(using).
	unit(chalk).
	unit(eraser).
	unit(running).
	unit(teacher).
	unit(student).

	test(blackboard_1) :-
		threaded_ignore(teacher::run(4)),
		threaded_ignore(student::run(10)).

:- end_object.
