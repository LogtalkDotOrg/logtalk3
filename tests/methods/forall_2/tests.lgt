
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/26,
		comment is 'Unit tests for the forall/2 built-in method.'
	]).

	test(forall_2_1) :-
		forall(true, true).

	test(forall_2_2) :-
		forall(fail, true).

	test(forall_2_3) :-
		\+ forall(true, fail).

	test(forall_2_4) :-
		forall(a(X), b(X)).

	a(1). a(2). a(3).

	b(1). b(2). b(3).

:- end_object.
