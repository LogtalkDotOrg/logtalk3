
:- object(bench_queens,
	implements(databasep)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'A simple database for solving the 4-queen puzzle.']).

	%%Benchmark 7 - Solving the 4-queen puzzle.

	queens(N,Qs) <-
		range(1,N,Ns) &
		queens(Ns,[],Qs).

	queens([],Qs,Qs) <- true.
	queens(UnplacedQs,SafeQs,Qs) <-
		select(UnplacedQs,UnplacedQs1,Q) &
		not_attack(SafeQs,Q) &
		queens(UnplacedQs1,[Q|SafeQs],Qs).

	not_attack(Xs,X) <-
		not_attack(Xs,X,1).

	not_attack([],_,_) <- true.
	not_attack([Y|Ys],X,N) <-
		{X =\= Y+N} & 
		{X =\= Y-N} &
		{N1 is N+1} &
		not_attack(Ys,X,N1).
	
	select([X|Xs],Xs,X) <- true.
	select([Y|Ys],[Y|Zs],X) <- select(Ys,Zs,X).

	range(N,N,[N]) <- true.
	range(M,N,[M|Ns]) <-
		{M < N} &
		{M1 is M+1} &
		range(M1,N,Ns).

	bench_goal(queens(4, [2, 4, 1, 3])).
	bench_goal(queens(4, [2, 4, 3, 1])).

:- end_object.
				