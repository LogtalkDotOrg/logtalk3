
:- protocol(interpreterp).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Protocol for an interpreter.']).

	:- public(prove/2).
	:- mode(prove(+goal, +database), zero_or_more).
	:- info(prove/2, [
		comment is 'True if goal is provable in the specified database.',
		argnames is ['Goal', 'DB']]).

	:- public(prove/3).
	:- mode(prove(+goal, +limit, +database), zero_or_more).
	:- info(prove/3, [
		comment is 'True if goal is provable within the given depth-limit in the specified database.',
		argnames is ['Goal', 'Limit', 'DB']]).

:- end_protocol.
