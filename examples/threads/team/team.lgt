%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(team).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/10/14,
		comment is 'Example of synchronous concurrency as described in the corresponding Rosetta Code task.']).

	:- threaded.

	:- public(start/0).
	start :-
		threaded((
			reader,
			writer(0)
		)).

	reader :-
		open('input.txt', read, Stream),
		repeat,
			read_term(Stream, Term, []),
			threaded_notify(term(Term)),
		Term == end_of_file,
		!,
		close(Stream),
		threaded_wait(lines(Lines)),
		write('Number of lines: '), write(Lines), nl.

	writer(N0) :-
		threaded_wait(term(Term)),
		(	Term == end_of_file ->
			threaded_notify(lines(N0))
		;	N is N0 + 1,
			write(Term), nl,
			writer(N)
		).

:- end_object.
