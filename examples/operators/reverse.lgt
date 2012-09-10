%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(reverse).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2011/05/18,
		comment is 'Reads and writes a simple table of facts from and to files for testing operator handling code.']).

	:- op(500, xfx, next).				% local object operators, not visible outside this object
	:- op(500, xfx, previous).

	:- public(reverse_file/0).

	reverse_file :-
		open('next.txt', read, RStream),
		open('previous.txt', write, WStream),
		read(RStream, Term),			% local operators are used when reading terms ...
		process(Term, RStream, WStream).

	process(end_of_file, RStream, WStream) :-
		close(RStream),
		close(WStream).
	process(X next Y, RStream, WStream) :-
		write(WStream, Y previous X),	% ... and when writing terms
		write(WStream, '.'), nl(WStream),
		read(RStream, Next),
		process(Next, RStream, WStream).

:- end_object.
