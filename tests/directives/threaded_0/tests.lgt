
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the threaded/0 built-in directive.'
	]).

	:- threaded.

	test(threaded_0_1) :-
		this(This),
		object_property(This, threaded).

:- end_object.
