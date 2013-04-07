
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/04/07,
		comment is 'Unit tests for the bagof/3 built-in method.'
	]).

	test(bagof_3_1) :-
		Goal = Y^a(X, Y),
		bagof(X, Goal, L),
		L == [1, 2, 3, 4].

	test(bagof_3_2) :-
		Goal = a(X, Y),
		bagof(X, Y^Goal, L),
		L == [1, 2, 3, 4].

	test(bagof_3_3) :-
		bagof(X, Y^a(X, Y), L),
		L == [1, 2, 3, 4].

	test(bagof_3_4) :-
		findall(Y-L, bagof(X, a(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
