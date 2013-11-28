
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/13,
		comment is 'Unit tests for the "errors" example.'
	]).

	cover(misspell).
	cover(singletons(_)).
	cover(redefinitions).
	cover(unknownrefs).
	cover(portability).

:- end_object.
