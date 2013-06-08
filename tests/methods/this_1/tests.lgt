
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/06/07,
		comment is 'Unit tests for the this/1 built-in method.'
	]).

	test(this_1) :-
		this(This),
		This == tests.

	test(this_2) :-
		this_1_test_object_1::p(This),
		This == this_1_test_object_1.

:- end_object.
