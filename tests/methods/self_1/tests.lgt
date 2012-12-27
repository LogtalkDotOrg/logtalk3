
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/26,
		comment is 'Unit tests for the self/1 built-in method.'
	]).

	test(self_1) :-
		self(Self),
		Self == tests.		

:- end_object.
