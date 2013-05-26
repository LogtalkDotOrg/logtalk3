
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/26,
		comment is 'Unit tests for the forall/2 built-in method.'
	]).

	test(not_1_1) :-
		\+ fail.

	test(not_1_2) :-
		\+ \+ true.

	test(not_1_3) :-
		\+ a(4).

	test(not_1_4) :-
		\+ \+ a(_).

	a(1). a(2). a(3).

:- end_object.
