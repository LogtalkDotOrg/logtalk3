
:- object(bench_puzzle,
	implements(databasep)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'A simple database for solving the mu-puzzle from GEB.']).

	append([], Ys, Ys) <- true.
	append([X|Xs], Ys, [X|Zs]) <-
		append(Xs, Ys, Zs).
  
	theorem(_, [m, i]) <- true.
	theorem(_, []) <- fail.
	theorem(Depth, R) <- 
		Depth > 0 &
		D is Depth - 1 &
		theorem(D, S) &
		rules(S, R).

	rules(S, R) <- rule1(S, R).
	rules(S, R) <- rule2(S, R).
	rules(S, R) <- rule3(S, R).
	rules(S, R) <- rule4(S, R).

	rule1(S, R) <-
		append(X, [i], S) &
		append(X, [i,u], R).
	
	rule2([m|T], [m|R]) <-
		append(T, T, R).

	rule3([], _) <-
		fail.

	rule3(R, T) <-
		append([i,i,i], S, R),
		append([u], S, T).
	rule3([H|T], [H|R]) <-
		rule3(T, R).

	rule4([], _) <-
		{fail}.

	rule4(R, T) <-
		append([u,u], T, R).

	rule4([H|T], [H|R]) <-
		rule4(T, R).

	test_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u]).
	test_non_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, x, u]).

	bench_goal(theorem(4, T)) :- test_theorem(T).
	bench_goal(theorem(4, T)) :- test_non_theorem(T).

:- end_object.
