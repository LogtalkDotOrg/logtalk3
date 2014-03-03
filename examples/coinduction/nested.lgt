%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(nested).

	:- info([
		version is 0.3,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2012/09/11,
		comment is 'Nested automaton example.'
	]).

	:- public(state/2).
	:- coinductive(state/2).

	state(s0, [s0| T]) :- enter, state(s1, T).
	state(s0, [s0| T]) :- error, state(s3, T).
	state(s1, [s1| T]) :- work, state(s1, T).
	state(s1, [s1| T]) :- exit, state(s2, T).
	state(s2, [s2| T]) :- recur, state(s0, T).
	state(s3, [s3| T]) :- recur, state(s0, T).

	work.

	enter.	recur.
	exit.	error.

:- end_object.
