%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(url).

	:- info([
		version is 1.0,
		date is 2003/7/7,
		author is 'Paulo Moura',
		comment is 'Simple example of URL parsing.']).

	:- public(parse/2).
	:- mode(parse(@list, -list), zero_or_one).
	:- info(parse/2, [
		comment is 'Parses a URL into its components.',
		argnames is ['URL', 'Components']]).

	parse(URL, [protocol(Protocol), address(Address), path(Path), file(File)]) :-
		phrase(url(Protocol, Address, Path, File), URL).

	url(Protocol, Address, Path, File) --> protocol(Protocol), "://", address(Address), path(Path), file(File).

	protocol(ftp) --> "ftp".
	protocol(http) --> "http".
	protocol(https) --> "https".
	protocol(rtsp) --> "rtsp".

	address(Address) --> ip_number(Address).
	address([Identifier| Identifiers]) --> identifier(Identifier), dot_identifiers(Identifiers).

	ip_number([N1, N2, N3, N4]) --> ip_subnumber(N1), ".", ip_subnumber(N2), ".", ip_subnumber(N3), ".", ip_subnumber(N4).

	ip_subnumber(N) --> [N1, N2, N3], {N is N3 - 0'0 + 10*(N2 - 0'0) + 100*(N1 - 0'0), N >= 0, N =< 255}.
	ip_subnumber(N) --> [N1, N2], {N is N2 - 0'0 + 10*(N1 - 0'0), N >= 0, N =< 255}.
	ip_subnumber(N) --> [N1], {N is N1 - 0'0, N >= 0, N =< 255}.

	identifier(Identifier) --> characters(Codes), {atom_codes(Identifier, Codes)}.

	characters([]) --> [].
	characters([Code| Codes]) --> [Code], {character(Code)}, characters(Codes).

	character(Code) :- Code @>= 0'a, Code @=< 0'z, !.
	character(Code) :- Code @>= 0'A, Code @=< 0'Z.

	dot_identifiers([]) --> [].
	dot_identifiers([Identifier| Identifiers]) --> ".", identifier(Identifier), dot_identifiers(Identifiers).

	path([]) --> [].
	path([Identifier| Path]) --> "/", identifier(Identifier), path(Path).

	file('') --> [].
	file(File) --> "/", identifier(Name), ".", identifier(Extension), {atom_concat(Name, '.', Aux), atom_concat(Aux, Extension, File)}.

:- end_object.
