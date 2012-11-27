
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the this/1 built-in method.'
	]).

	test(this_1) :-
		this(This),
		This == tests.

:- end_object.
