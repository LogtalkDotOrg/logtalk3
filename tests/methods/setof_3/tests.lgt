
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/26,
		comment is 'Unit tests for the setof/3 built-in method.'
	]).

	test(setof_1) :-
		setof(X, Y^a(X, Y), L),
		L == [1, 2, 3, 4].

	test(setof_2) :-
		findall(Y-L, setof(X, a(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
