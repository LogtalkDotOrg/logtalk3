
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/13,
		comment is 'Unit tests for the "errors" example.'
	]).

	unit(misspell).
	unit(singletons(_)).
	unit(redefinitions).
	unit(unknownrefs).
	unit(portability).

:- end_object.
