
:- object(bench_fib,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for playing with Fibonacci numbers.'
	]).

	%%Calculates the n:th Fibonacci number recursively.

	fib_rec(1, 1) if true.
	fib_rec(2, 1) if true.
	fib_rec(N, X) if
		{N > 2} and
		{N1 is N - 1} and
		{N2 is N - 2} and
		fib_rec(N1, X1) and
		fib_rec(N2, X2) and
		{X is X1 + X2}.

	%%Calculates the n:th Fibonacci number iteratively.

	fib_iter(1, 1) if true.
	fib_iter(2, 1) if true.
	fib_iter(N, X) if
		{N > 2} and
		fib_iter(2, N, 1, 1, X).

	fib_iter(N, N, X, _, X) if true.
	fib_iter(N0, N, X2, X1, X) if
		{N0 < N} and
		{N1 is N0 + 1} and
		{X3 is X2 + X1} and
		fib_iter(N1, N, X3, X2, X).

	bench_goal(fib_rec(5, 5)).
	bench_goal(fib_rec(10, 55)).

	bench_goal(fib_iter(5, 5)).
	bench_goal(fib_iter(10, 55)).

:- end_object.
