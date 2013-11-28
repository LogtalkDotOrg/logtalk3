
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/06,
		comment is 'Unit tests for the "hello_world" example.'
	]).

	cover(hello_world).

:- end_object.
