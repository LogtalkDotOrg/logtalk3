
:- object(parser).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/12,
		comment is 'Parser predicates for PDDL 3.0 files.']).

	:- public(parse/2).
	:- mode(parse(+atom, -compound), one).
	:- info(parse/2,
		[comment is 'Parses a PDDL 3.0 file, returning a compound term representing its contents.',
		 argnames is ['File', 'Output']]).

	parse(F, O) :-
		::parse(F, O, _).

	:- public(parse/3).
	:- mode(parse(+atom, -compound, -list(atom)), one).
	:- info(parse/3,
		[comment is 'Parses a PDDL 3.0 file, returning a compound term representing its contents and rest of the file. Useful when domain and problem are in one file.',
		 argnames is ['File', 'Output', 'RestOfFile']]).

:- end_object.
