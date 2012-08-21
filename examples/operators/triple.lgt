%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(triple).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2011/05/18,
		comment is 'Read and asserts a simple table of facts from a file for testing operator handling code.']).

	:- public(triple/2).
	:- dynamic(triple/2).

	:- op(500, xfx, triple).

	:- public(read_from_file/0).

	read_from_file :-
		retractall(triple(_, _)),
		open('triple.txt', read, Stream),
		read(Stream, Term),
		process(Stream, Term).

	process(Stream, end_of_file) :-
		close(Stream),
		!.
	process(Stream, Term) :-
		assertz(Term),
		read(Stream, Next),
		process(Stream, Next).

:- end_object.
