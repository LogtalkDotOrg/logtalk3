
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/04/07,
		comment is 'Unit tests for the setof/3 built-in method.'
	]).

	test(setof_3_1) :-
		Goal = Y^a(X, Y),
		setof(X, Goal, L),
		L == [1, 2, 3, 4].

	test(setof_3_2) :-
		Goal = a(X, Y),
		setof(X, Y^Goal, L),
		L == [1, 2, 3, 4].

	test(setof_3_3) :-
		setof(X, Y^a(X, Y), L),
		L == [1, 2, 3, 4].

	test(setof_3_4) :-
		findall(Y-L, setof(X, a(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
