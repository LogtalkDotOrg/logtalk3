
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/26,
		comment is 'Unit tests for the bagof/3 built-in method.'
	]).

	test(bagof_1) :-
		bagof(X, Y^a(X, Y), L),
		L == [1, 2, 3, 4].

	test(bagof_2) :-
		findall(Y-L, bagof(X, a(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
