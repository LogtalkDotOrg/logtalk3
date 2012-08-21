
:- object(bench_isotree,
	implements(databasep)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'A simple database for testing if two binary trees are isomorphic.']).

	%% Determining whether two binary trees are isomorphic.

	isotree(void, void) <- true.

	%Normal version.  
	isotree(t(X, L1, R1), t(X, L2, R2)) <-
		isotree(L1, L2) &
		isotree(R1, R2).
	isotree(t(X, L1, R1), t(X, L2, R2)) <-
		isotree(L1, R2) &
		isotree(R1, L2).

	%%Goals in body swapped.
%	 isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		 isotree(R1, R2) &
%		 isotree(L1, L2).
%	 isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		 isotree(R1, L2) &
%		 isotree(L1, R2).

	tree1(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(0, t(3, void, void), t(0, void, void)), t(2, t(4, void, void), t(3, t(2, void, void), t(3, void, void)))))).

	tree1_iso(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(2, t(3, t(2, void, void), t(3, void, void)), t(4, void, void)), t(0, t(0, void, void), t(3, void, void))))).

	tree2(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))).

	tree2_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(2, void, void)))))).

	tree3(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))).

	tree3_non_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(x, void, void)))))).
 
	bench_goal(isotree(T, IsoT)) :- tree1(T), tree1_iso(IsoT).
	bench_goal(isotree(T, IsoT)) :- tree2(T), tree2_iso(IsoT).
	bench_goal(isotree(T, NonIsoT)) :- tree3(T), tree3_non_iso(NonIsoT).

:- end_object.
