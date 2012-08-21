:- encoding('ISO-8859-1').	% this directive, when present, must be the first
							% term, in the first line, of a source file

:- object(latin).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/01/10,
		comment is 'Simple test of the encoding/1 directive.']).

	:- public(name/1).
	:- mode(name(?atom), zero_or_more).
	:- info(name/1, [
		comment is 'Table of person names.',
		argnames is ['Name']]).

	name('António Simões').
	name('Cátia Conceição').
	name('João Raínho').
	name('Luís Araújo').

:- end_object.
