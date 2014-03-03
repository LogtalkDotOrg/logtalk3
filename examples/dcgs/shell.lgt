%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(shell).

	:- info([
		version is 1.0,
		date is 2004/4/29,
		author is 'Paulo Moura',
		comment is 'Simple example of command-line shell parsing.'
	]).

	:- public(parse/2).
	:- mode(parse(@list, -list), zero_or_one).
	:- info(parse/2, [
		comment is 'Parses a sequence of commands.',
		argnames is ['Sequence', 'Commands']
	]).

	parse(Sequence, Commands) :-
		phrase(commands(Commands), Sequence).

	commands([C| Cs]) -->
		command(C), separator, commands(Cs).
	commands([C]) -->
		command(C).

	separator --> ";".

	whitespace --> " ", whitespace.
	whitespace --> [].

	command(Cd) -->
		whitespace, "cd", whitespace, cdargs(Args), whitespace,
		{atom_concat(cd, Args, Cd)}.
	command(Ls) -->
		whitespace, "ls", whitespace, lsargs(Args), whitespace,
		{atom_concat(ls, Args, Ls)}.
	command(pwd) -->
		whitespace, "pwd", whitespace.

	cdargs(' ~') --> "~".
	cdargs(' ..') --> "..".
	cdargs(' .') --> ".".
	cdargs('') --> [].

	lsargs(' -l') --> "-l".
	lsargs(' -a') --> "-a".
	lsargs('') --> [].

:- end_object.
