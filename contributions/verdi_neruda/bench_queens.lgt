
:- object(bench_queens,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for solving the 4-queen puzzle.'
	]).

	%%Benchmark 7 - Solving the 4-queen puzzle.

	queens(N,Qs) if
		range(1,N,Ns) and
		queens(Ns,[],Qs).

	queens([],Qs,Qs) if true.
	queens(UnplacedQs,SafeQs,Qs) if
		select(UnplacedQs,UnplacedQs1,Q) and
		not_attack(SafeQs,Q) and
		queens(UnplacedQs1,[Q|SafeQs],Qs).

	not_attack(Xs,X) if
		not_attack(Xs,X,1).

	not_attack([],_,_) if true.
	not_attack([Y|Ys],X,N) if
		{X =\= Y+N} and
		{X =\= Y-N} and
		{N1 is N+1} and
		not_attack(Ys,X,N1).

	select([X|Xs],Xs,X) if true.
	select([Y|Ys],[Y|Zs],X) if select(Ys,Zs,X).

	range(N,N,[N]) if true.
	range(M,N,[M|Ns]) if
		{M < N} and
		{M1 is M+1} and
		range(M1,N,Ns).

	bench_goal(queens(4, [2, 4, 1, 3])).
	bench_goal(queens(4, [2, 4, 3, 1])).

:- end_object.
